package hiroki1117.sruby.core

import cats.data.StateT
import cats.effect.IO
import hiroki1117.sruby.ast.Expr

/**
  * =========================================================
  * Eval モナド
  *   - IO（副作用）
  *   - VMState（インタプリタ状態）
  *   を合成した、Ruby の実行コンテキスト
  * =========================================================
  */
type Eval[A] = StateT[IO, VMState, A]

/**
  * =========================================================
  * RValue - Ruby の全ての値の共通基底
  * =========================================================
  */
trait RValue:
  def rubyClass: RClass

  /** truthiness: false/nilのみfalse */
  def isTruthy: Boolean = true

  /** Ruby’s #inspect (デフォルト実装) */
  def inspect: String = s"#<${rubyClass.name}>"

  /** Ruby’s #to_s (デフォルトはinspectと別物にできる) */
  def toRubyString: String = inspect

  /** Instance variable (only RObject normally supports this) */
  def getInstanceVariable(name: String): Option[RValue] = None
  def setInstanceVariable(name: String, value: RValue): RValue =
    throw new UnsupportedOperationException("Instance variables are not supported here")

/**
  * =========================================================
  * RObject - Ruby の一般オブジェクト（インスタンス変数保持）
  * =========================================================
  */
final case class RObject(
  rubyClass: RClass,
  ivars: Map[String, RValue] = Map.empty
) extends RValue:

  override def getInstanceVariable(name: String): Option[RValue] =
    ivars.get(name)

  override def setInstanceVariable(name: String, value: RValue): RObject =
    this.copy(ivars = ivars.updated(name, value))

  override def inspect: String =
    s"#<${rubyClass.name}:0x${System.identityHashCode(this).toHexString}>"

/**
  * =========================================================
  * Ruby クラス（メソッドテーブル + 継承）
  * =========================================================
  */
final case class RClass(
  name: String,
  superclass: Option[RClass],
  methods: Map[String, RMethod] = Map.empty
) extends RValue:

  override def rubyClass: RClass = Builtins.ClassClass

  override def inspect: String = name

  /**
    * メソッド定義
    */
  def defineMethod(name: String, method: RMethod): RClass =
    this.copy(methods = methods.updated(name, method))

  /**
    * メソッド探索（継承チェーンを辿る）
    */
  def lookupMethod(name: String): Option[RMethod] =
    methods.get(name).orElse(superclass.flatMap(_.lookupMethod(name)))

/**
  * =========================================================
  * Ruby メソッド表現
  * =========================================================
  * 
  * Ruby のメソッドには2種類ある:
  *   1. ユーザー定義メソッド（def で定義、AST を保持）
  *   2. 組み込みメソッド（Scala で実装、関数を保持）
  */
sealed trait RMethod:
  def name: String
  def invoke(receiver: RValue, args: List[RValue]): Eval[RValue]

/**
  * ユーザー定義メソッド（Ruby の def で定義）
  */
final case class RUserMethod(
  name: String,
  params: List[String],           // パラメータ名のリスト
  body: Expr,                      // メソッド本体（AST）
  definingClass: RClass            // 定義されたクラス
) extends RMethod:
  
  def invoke(receiver: RValue, args: List[RValue]): Eval[RValue] =
    // 引数の数をチェック
    if args.length != params.length then
      StateT.liftF(IO.raiseError(
        new RuntimeException(
          s"wrong number of arguments for $name (given ${args.length}, expected ${params.length})"
        )
      ))
    else
      // 引数をバインドした環境を作成
      val newEnv = params.zip(args).foldLeft(Env()) { case (env, (paramName, argValue)) =>
        env.set(paramName, argValue)
      }
      
      // メソッド呼び出し用の新しいフレーム
      val methodFrame = Frame(
        self = receiver,
        env = newEnv,
        currentClass = definingClass
      )
      
      // フレームを push → body 評価 → pop
      for
        state <- StateT.get[IO, VMState]
        // 新しいフレームで評価
        newState = state.pushFrame(methodFrame)
        _ <- StateT.set[IO, VMState](newState)
        
        // メソッド本体を評価
        // NOTE: Evaluator.eval は循環参照を避けるため、
        // RUserMethod.evalBody として外部から設定される
        result <- RUserMethod.evalBody(body)
        
        // フレームを pop
        state2 <- StateT.get[IO, VMState]
        _ <- StateT.set[IO, VMState](state2.popFrame()._2)
      yield result

/**
  * RUserMethod のコンパニオンオブジェクト
  */
object RUserMethod:
  /**
    * メソッド本体を評価する関数
    * Evaluator から注入される
    */
  var evalBody: Expr => Eval[RValue] = _ => StateT.pure(RNil)

/**
  * 組み込みメソッド（Scala で実装）
  */
final case class RBuiltinMethod(
  name: String,
  impl: (RValue, List[RValue]) => Eval[RValue]
) extends RMethod:
  
  def invoke(receiver: RValue, args: List[RValue]): Eval[RValue] =
    impl(receiver, args)

/**
  * RMethod のコンパニオンオブジェクト
  * 
  * 既存のテストコードとの互換性のため、apply メソッドを提供
  */
object RMethod:
  /**
    * 組み込みメソッドを作成（後方互換性のため）
    */
  def apply(impl: (RValue, List[RValue]) => Eval[RValue]): RMethod =
    RBuiltinMethod("<anonymous>", impl)

/**
  * =========================================================
  * Env - ローカル変数環境
  * =========================================================
  */
final case class Env(vars: Map[String, RValue] = Map.empty):
  def get(name: String): Option[RValue] = vars.get(name)
  def set(name: String, value: RValue): Env =
    this.copy(vars = vars.updated(name, value))

/**
  * =========================================================
  * Frame - メソッド呼び出しの実行フレーム
  * =========================================================
  */
final case class Frame(
  self: RValue,
  env: Env,
  currentClass: RClass
)

/**
  * =========================================================
  * VMState - インタプリタ全体状態
  * =========================================================
  */
final case class VMState(
  frames: List[Frame],
  globalClasses: Map[String, RClass] = Map.empty,
  classes: Map[String, RClass] = Map.empty  // クラステーブル（Phase 2）
):
  def currentFrame: Frame = frames.head

  def pushFrame(f: Frame): VMState =
    this.copy(frames = f :: frames)

  def popFrame(): (Frame, VMState) =
    (frames.head, this.copy(frames = frames.tail))
  
  /**
    * クラスを更新（MethodDef で使用）
    */
  def updateClass(klass: RClass): VMState =
    this.copy(classes = classes.updated(klass.name, klass))
  
  /**
    * クラスを取得
    */
  def getClass(name: String): Option[RClass] =
    classes.get(name).orElse(
      // Builtins のクラスもチェック
      name match
        case "Object"    => Some(Builtins.ObjectClass)
        case "Class"     => Some(Builtins.ClassClass)
        case "Integer"   => Some(Builtins.IntegerClass)
        case "String"    => Some(Builtins.StringClass)
        case "Symbol"    => Some(Builtins.SymbolClass)
        case "Float"     => Some(Builtins.FloatClass)
        case "Array"     => Some(Builtins.ArrayClass)
        case "Hash"      => Some(Builtins.HashClass)
        case "Range"     => Some(Builtins.RangeClass)
        case "NilClass"  => Some(Builtins.NilClass)
        case "TrueClass" => Some(Builtins.TrueClass)
        case "FalseClass"=> Some(Builtins.FalseClass)
        case _ => None
    )

/**
  * =========================================================
  * Builtins - Ruby の基本クラス定義
  * Class/Object/Boolean/String/Integer/Nil...
  * =========================================================
  */
object Builtins:

  lazy val ObjectClass: RClass =
    RClass("Object", None)

  lazy val ClassClass: RClass =
    RClass("Class", Some(ObjectClass))

  // プリミティブクラス
  lazy val NilClass: RClass     = RClass("NilClass", Some(ObjectClass))
  lazy val TrueClass: RClass    = RClass("TrueClass", Some(ObjectClass))
  lazy val FalseClass: RClass   = RClass("FalseClass", Some(ObjectClass))
  lazy val IntegerClass: RClass = RClass("Integer", Some(ObjectClass))
  lazy val StringClass: RClass  = RClass("String", Some(ObjectClass))
  lazy val SymbolClass: RClass  = RClass("Symbol", Some(ObjectClass))
  lazy val FloatClass: RClass   = RClass("Float", Some(ObjectClass))
  lazy val ArrayClass: RClass   = RClass("Array", Some(ObjectClass))
  lazy val HashClass: RClass    = RClass("Hash", Some(ObjectClass))
  lazy val RangeClass: RClass   = RClass("Range", Some(ObjectClass))

/**
  * =========================================================
  * Ruby プリミティブ値
  * =========================================================
  */
case object RNil extends RValue:
  override def rubyClass: RClass = Builtins.NilClass
  override def isTruthy: Boolean = false
  override def inspect: String = "nil"
  override def toRubyString: String = ""

case object RTrue extends RValue:
  override def rubyClass: RClass = Builtins.TrueClass
  override def toRubyString: String = "true"

case object RFalse extends RValue:
  override def rubyClass: RClass = Builtins.FalseClass
  override def isTruthy: Boolean = false
  override def toRubyString: String = "false"

/**
  * Integer
  */
final case class RInteger(value: Long) extends RValue:
  override def rubyClass: RClass = Builtins.IntegerClass
  override def inspect: String = value.toString
  override def toRubyString: String = value.toString

/**
  * String
  */
final case class RString(value: String) extends RValue:
  override def rubyClass: RClass = Builtins.StringClass
  override def inspect: String = s""""$value""""
  override def toRubyString: String = value

/**
  * Symbol
  * Ruby の Symbol は不変・高速比較・interned
  */
final case class RSymbol(name: String) extends RValue:
  override def rubyClass: RClass = Builtins.SymbolClass
  override def inspect: String = s":$name"
  override def toRubyString: String = name

object RSymbol:
  // Symbol の intern pool（同じ名前のシンボルは同一オブジェクト）
  private val internPool = scala.collection.mutable.Map[String, RSymbol]()
  
  def intern(name: String): RSymbol =
    internPool.getOrElseUpdate(name, RSymbol(name))
