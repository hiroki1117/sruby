package hiroki1117.sruby.core

import cats.data.StateT
import cats.effect.IO

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
  * Ruby メソッド表現（Eval を返す）
  * =========================================================
  */
final case class RMethod(
  invoke: (RValue, List[RValue]) => Eval[RValue]
)

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
  globalClasses: Map[String, RClass] = Map.empty
):
  def currentFrame: Frame = frames.head

  def pushFrame(f: Frame): VMState =
    this.copy(frames = f :: frames)

  def popFrame(): (Frame, VMState) =
    (frames.head, this.copy(frames = frames.tail))

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
