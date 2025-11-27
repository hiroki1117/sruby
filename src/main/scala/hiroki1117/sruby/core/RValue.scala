package hiroki1117.sruby.core

import cats.data.StateT
import cats.effect.IO

/**
  * =========================================================
  * Ruby Interpreter Core Representation
  * =========================================================
  *
  *  - RValue: Ruby のすべての値の基底型
  *  - RObject: Ruby の一般オブジェクト（インスタンス変数含む）
  *  - RClass: Ruby のクラス（メソッドテーブル・継承）
  *  - RMethod: Ruby メソッド（Eval を返す）
  *  - Env: ローカル変数環境
  *  - Frame: メソッド呼び出し中のフレーム
  *  - VMState: Interpreter 全体の状態
  *
  *  ここまでが Ruby 処理系の「心臓部」
  * =========================================================
  */

/** StateT を使った Ruby 実行モナド */
type Eval[A] = StateT[IO, VMState, A]

/**
  * Ruby の値（すべてのオブジェクトの基底）
  */
trait RValue:
  def rubyClass: RClass

  /** truthiness は false/nil だけ false */
  def isTruthy: Boolean = true

  /** Ruby の inspect は Kernel#inspect が本来実装するが、最低限の初期値 */
  def inspect: String = toRubyString

  /** Ruby の to_s（デフォルトは inspect と同じでよい） */
  def toRubyString: String = inspect

  /** インスタンス変数（通常は RObject だけが実装） */
  def getInstanceVariable(name: String): Option[RValue] = None
  def setInstanceVariable(name: String, value: RValue): RValue =
    throw new UnsupportedOperationException("Instance variables not supported")

/**
  * Ruby の一般オブジェクト（インスタンス変数を保持）
  */
final case class RObject(
  rubyClass: RClass,
  ivars: Map[String, RValue] = Map.empty
) extends RValue:

  override def getInstanceVariable(name: String): Option[RValue] =
    ivars.get(name)

  override def setInstanceVariable(name: String, value: RValue): RObject =
    this.copy(ivars = ivars.updated(name, value))

  override def toRubyString: String =
    s"#<${rubyClass.name}:0x${System.identityHashCode(this).toHexString}>"

/**
  * Ruby のクラス
  *
  * - name
  * - superclass（継承）
  * - methods（メソッドテーブル）
  */
final case class RClass(
  name: String,
  superclass: Option[RClass],
  methods: Map[String, RMethod] = Map.empty
) extends RValue:

  /** Class#class は Class 自身 */
  override def rubyClass: RClass = RClass.ClassClass

  /** class はインスタンス変数を持てる */
  override def getInstanceVariable(name: String): Option[RValue] = None

  override def toRubyString: String = name

  /** メソッド定義 */
  def defineMethod(name: String, method: RMethod): RClass =
    this.copy(methods = methods.updated(name, method))

  /** メソッド探索（継承チェーンを辿る） */
  def lookupMethod(name: String): Option[RMethod] =
    methods.get(name).orElse(superclass.flatMap(_.lookupMethod(name)))

/**
  * Ruby メソッド表現
  *
  * - receiver（self）
  * - 引数
  * - Eval[RValue] を返す（StateT + IO）
  */
final case class RMethod(
  invoke: (RValue, List[RValue]) => Eval[RValue]
)

/**
  * ローカル変数環境
  */
final case class Env(vars: Map[String, RValue] = Map.empty):
  def get(name: String): Option[RV]()
