package hiroki1117.sruby.ast

/**
  * =========================================================
  * Ruby AST (Abstract Syntax Tree)
  * =========================================================
  * 
  * 設計方針:
  *   1. すべてのノードは Position を持つ（エラー報告のため）
  *   2. AST は不変（評価時に変更しない）
  *   3. 演算子は ADT で表現（String より型安全）
  *   4. nil/self は case object（意味論的に唯一の値）
  *   5. Phase 1 に必要な最小限の演算子のみ実装
  */

// =========================================================
// Position - ソースコード位置情報
// =========================================================

/**
  * ソースコード中の位置情報
  * 
  * エラーメッセージで使用:
  *   "example.rb:10:5: undefined variable `x`"
  */
final case class Position(
  filename: String,
  line: Int,
  column: Int
):
  override def toString: String = s"$filename:$line:$column"

object Position:
  /** 位置情報が不明な場合のデフォルト値 */
  val NoPosition: Position = Position("<unknown>", 0, 0)
  
  /** REPL入力用 */
  def repl(line: Int, column: Int): Position = 
    Position("<repl>", line, column)
  
  /** ファイル用 */
  def file(filename: String, line: Int, column: Int): Position =
    Position(filename, line, column)

// =========================================================
// Node - AST 基底
// =========================================================

/**
  * AST ノードの基底トレイト
  * すべての AST ノードはこれを継承する
  */
sealed trait Node:
  /** ソースコード中の位置 */
  def pos: Position

/** 
  * 式 (Expression)
  * 
  * Ruby では「ほぼすべて」が式として評価される
  */
sealed trait Expr extends Node

// =========================================================
// Literal - リテラル値
// =========================================================

/** 整数リテラル: 42, -100, 0 */
final case class IntLiteral(
  value: Long,
  pos: Position = Position.NoPosition
) extends Expr:
  override def toString: String = value.toString

/** 文字列リテラル: "hello", 'world' */
final case class StringLiteral(
  value: String,
  pos: Position = Position.NoPosition
) extends Expr:
  override def toString: String = s""""$value""""

/** Boolean リテラル: true, false */
final case class BooleanLiteral(
  value: Boolean,
  pos: Position = Position.NoPosition
) extends Expr:
  override def toString: String = value.toString

/** 
  * nil リテラル（シングルトン）
  * 
  * Ruby の nil は意味論的に唯一の値。
  * AST レベルでも case object として表現する。
  * 
  * Evaluator で RNil（シングルトン）に変換される。
  */
case object NilLiteral extends Expr:
  def pos: Position = Position.NoPosition
  override def toString: String = "nil"

/** シンボルリテラル: :name, :age */
final case class SymbolLiteral(
  name: String,
  pos: Position = Position.NoPosition
) extends Expr:
  override def toString: String = s":$name"

// =========================================================
// Variables - 変数の参照と代入
// =========================================================

/** ローカル変数参照: x, name, result */
final case class LocalVarRef(
  name: String,
  pos: Position = Position.NoPosition
) extends Expr:
  override def toString: String = name

/** 
  * ローカル変数代入: x = 10
  * 
  * Ruby では代入は「式」であり、代入した値を返す
  */
final case class LocalVarAssign(
  name: String,
  value: Expr,
  pos: Position = Position.NoPosition
) extends Expr:
  override def toString: String = s"$name = $value"

/** インスタンス変数参照: @name, @age */
final case class InstanceVarRef(
  name: String,
  pos: Position = Position.NoPosition
) extends Expr:
  override def toString: String = name

/** インスタンス変数代入: @name = "Alice" */
final case class InstanceVarAssign(
  name: String,
  value: Expr,
  pos: Position = Position.NoPosition
) extends Expr:
  override def toString: String = s"$name = $value"

/** グローバル変数参照: $debug, $LOAD_PATH */
final case class GlobalVarRef(
  name: String,
  pos: Position = Position.NoPosition
) extends Expr:
  override def toString: String = name

/** グローバル変数代入: $debug = true */
final case class GlobalVarAssign(
  name: String,
  value: Expr,
  pos: Position = Position.NoPosition
) extends Expr:
  override def toString: String = s"$name = $value"

/** 
  * 定数参照: Foo, A::B::C
  * 
  * nameParts で名前空間を表現
  * 例: A::B::C => List("A", "B", "C")
  */
final case class ConstantRef(
  nameParts: List[String],
  pos: Position = Position.NoPosition
) extends Expr:
  override def toString: String = nameParts.mkString("::")

// =========================================================
// Operators - 演算子（Phase 1 に必要な最小限のみ）
// =========================================================

/**
  * 単項演算子の種類
  */
sealed trait UnaryKind:
  def toMethodName: String
  def toSymbol: String

object UnaryKind:
  /** 符号反転: -x → x.-@ */
  case object Negate extends UnaryKind:
    def toMethodName: String = "-@"
    def toSymbol: String = "-"
  
  /** 正符号: +x → x.+@ */
  case object Positive extends UnaryKind:
    def toMethodName: String = "+@"
    def toSymbol: String = "+"
  
  /** 論理否定: !x → x.! */
  case object Not extends UnaryKind:
    def toMethodName: String = "!"
    def toSymbol: String = "!"

/**
  * 二項演算子の種類（Phase 1 に必要な基本演算のみ）
  */
sealed trait BinaryKind:
  def toMethodName: String

object BinaryKind:
  // 算術演算
  case object Add extends BinaryKind:
    def toMethodName: String = "+"
  case object Sub extends BinaryKind:
    def toMethodName: String = "-"
  case object Mul extends BinaryKind:
    def toMethodName: String = "*"
  case object Div extends BinaryKind:
    def toMethodName: String = "/"
  case object Mod extends BinaryKind:
    def toMethodName: String = "%"
  
  // 比較演算
  case object Eq extends BinaryKind:
    def toMethodName: String = "=="
  case object Ne extends BinaryKind:
    def toMethodName: String = "!="
  case object Lt extends BinaryKind:
    def toMethodName: String = "<"
  case object Le extends BinaryKind:
    def toMethodName: String = "<="
  case object Gt extends BinaryKind:
    def toMethodName: String = ">"
  case object Ge extends BinaryKind:
    def toMethodName: String = ">="
  
  // ビット演算
  case object BitAnd extends BinaryKind:
    def toMethodName: String = "&"
  case object BitOr extends BinaryKind:
    def toMethodName: String = "|"
  case object BitXor extends BinaryKind:
    def toMethodName: String = "^"
  case object LShift extends BinaryKind:
    def toMethodName: String = "<<"
  case object RShift extends BinaryKind:
    def toMethodName: String = ">>"

// =========================================================
// Expression - 演算とメソッド呼び出し
// =========================================================

/**
  * 単項演算: -x, !flag, +value
  * 
  * Ruby では演算子もメソッド呼び出しの糖衣構文
  * Evaluator で receiver.toMethodName(kind) に変換される
  */
final case class UnaryOp(
  kind: UnaryKind,
  operand: Expr,
  pos: Position = Position.NoPosition
) extends Expr:
  override def toString: String = s"${kind.toSymbol}$operand"

/**
  * 二項演算: 1 + 2, x * y, "hello" + " world"
  * 
  * Ruby では演算子もメソッド呼び出しの糖衣構文
  * (1 + 2 は実際には 1.+(2))
  * 
  * Evaluator で left.toMethodName(kind)(right) に変換される
  */
final case class BinaryOp(
  left: Expr,
  kind: BinaryKind,
  right: Expr,
  pos: Position = Position.NoPosition
) extends Expr:
  override def toString: String = s"($left ${kind.toMethodName} $right)"

/**
  * メソッド呼び出し: obj.method(arg1, arg2) { |x| ... }
  * 
  * receiver が None の場合は self に対するメソッド呼び出し
  * block はオプショナルで、ブロック付きメソッド呼び出しに対応
  */
final case class MethodCall(
  receiver: Option[Expr],
  methodName: String,
  args: List[Expr],
  block: Option[BlockExpr] = None,
  pos: Position = Position.NoPosition
) extends Expr:
  override def toString: String = 
    val recv = receiver.map(r => s"$r.").getOrElse("")
    val argStr = if args.isEmpty then "" else args.mkString("(", ", ", ")")
    val blockStr = block.map(b => s" $b").getOrElse("")
    s"$recv$methodName$argStr$blockStr"

object MethodCall:
  /** レシーバーなしのメソッド呼び出しを作成 */
  def toSelf(
    name: String,
    args: List[Expr],
    block: Option[BlockExpr] = None,
    pos: Position = Position.NoPosition
  ): MethodCall =
    MethodCall(None, name, args, block, pos)

/**
  * Self 参照（シングルトン）
  * 
  * Ruby の self は現在の実行コンテキストのレシーバーを指す特別な予約語。
  * 意味論的に唯一の値なので case object として表現。
  * 
  * Evaluator で frame.self に変換される。
  */
case object Self extends Expr:
  def pos: Position = Position.NoPosition
  override def toString: String = "self"

/**
  * 式のシーケンス（順次実行）
  * 
  * Ruby の begin...end や class/method body に対応
  * 最後の式の値がシーケンス全体の値となる
  * 
  * 例: x = 10; y = 20; x + y
  */
final case class Sequence(
  exprs: List[Expr],
  pos: Position = Position.NoPosition
) extends Expr:
  override def toString: String = exprs.mkString("{ ", "; ", " }")

object Sequence:
  /** 空のシーケンス（nil を返す） */
  def empty(pos: Position = Position.NoPosition): Sequence =
    Sequence(List.empty, pos)

// =========================================================
// Control Flow - 制御構造
// =========================================================

/**
  * If 式: if cond then ... else ... end
  * 
  * Ruby では if も式なので、値を返す
  * 
  * 例:
  *   result = if x > 10 then "big" else "small" end
  */
final case class IfExpr(
  condition: Expr,
  thenBranch: Expr,
  elseBranch: Option[Expr],
  pos: Position = Position.NoPosition
) extends Expr:
  override def toString: String = 
    val elseStr = elseBranch.map(e => s" else $e").getOrElse("")
    s"if $condition then $thenBranch$elseStr end"

/**
  * While ループ: while cond do ... end
  * 
  * Ruby では while も式（nil を返す）
  */
final case class WhileExpr(
  condition: Expr,
  body: Expr,
  pos: Position = Position.NoPosition
) extends Expr:
  override def toString: String = s"while $condition do $body end"

// =========================================================
// Block - ブロック式
// =========================================================

/**
  * ブロックの種類
  * 
  * Ruby では { } と do...end で優先順位が異なる
  */
sealed trait BlockKind
object BlockKind:
  /** { |x| ... } 形式（高優先順位） */
  case object Brace extends BlockKind
  /** do |x| ... end 形式（低優先順位） */
  case object Do extends BlockKind

/**
  * ブロック式: { |x| ... } または do |x| ... end
  * 
  * Ruby のブロックは Proc/Closure を作成する。
  * メソッド呼び出しに渡されるか、単独で Proc として扱われる。
  * 
  * 例:
  *   [1, 2, 3].each { |x| puts x }
  *   5.times do |i| ... end
  * 
  * 注意: ブロック内では新しいローカルスコープが作られる
  *   x = 1
  *   3.times { |x| puts x }  # この x は外側の x とは別
  */
final case class BlockExpr(
  params: List[String],
  body: Expr,
  kind: BlockKind = BlockKind.Brace,
  pos: Position = Position.NoPosition
) extends Expr:
  override def toString: String = 
    val paramStr = if params.isEmpty then "" else params.mkString("|", ", ", "|")
    kind match
      case BlockKind.Brace => s"{ $paramStr $body }"
      case BlockKind.Do => s"do $paramStr $body end"

// =========================================================
// Definition - メソッド定義とクラス定義
// =========================================================

/**
  * メソッド定義: def method_name(params) ... end
  * 
  * Ruby ではメソッド定義自体が式であり、シンボルを返す
  * 
  * 例:
  *   def greet(name)
  *     "Hello, #{name}"
  *   end
  */
final case class MethodDef(
  name: String,
  params: List[String],
  body: Expr,
  pos: Position = Position.NoPosition
) extends Expr:
  override def toString: String = 
    val paramStr = params.mkString("(", ", ", ")")
    s"def $name$paramStr ... end"

/**
  * クラス定義: class ClassName < Superclass ... end
  * 
  * nameParts でネームスペース対応 (class A::B::C)
  * superclass は式として評価可能（動的継承に対応）
  * 
  * 例:
  *   class Person < Object
  *     def initialize(name)
  *       @name = name
  *     end
  *   end
  * 
  * 注意: Evaluator では以下の処理が必要
  *   1. ConstantRef の lookup
  *   2. superclass の評価（Expr → RClass）
  *   3. RClass の生成と登録
  */
final case class ClassDef(
  nameParts: List[String],
  superclass: Option[Expr],
  body: Expr,
  pos: Position = Position.NoPosition
) extends Expr:
  override def toString: String = 
    val name = nameParts.mkString("::")
    val superStr = superclass.map(s => s" < $s").getOrElse("")
    s"class $name$superStr ... end"

/**
  * モジュール定義: module ModuleName ... end
  * 
  * Phase 2 で実装予定
  */
final case class ModuleDef(
  nameParts: List[String],
  body: Expr,
  pos: Position = Position.NoPosition
) extends Expr:
  override def toString: String = 
    val name = nameParts.mkString("::")
    s"module $name ... end"
