package hiroki1117.sruby.ast

/**
  * =========================================================
  * Position - ソースコード位置
  * =========================================================
  */
final case class Position(
  filename: String,
  line: Int,
  column: Int
):
  override def toString: String = s"$filename:$line:$column"

object Position:
  val NoPosition: Position = Position("<unknown>", 0, 0)


/**
  * =========================================================
  * Node - AST 基底
  * =========================================================
  */
sealed trait Node:
  def pos: Position

/** Ruby の式（すべてのノードは式として評価可能にする） */
sealed trait Expr extends Node


/**
  * =========================================================
  * Literal - リテラル値
  * =========================================================
  */
final case class IntLiteral(
  value: Long,
  pos: Position = Position.NoPosition
) extends Expr

final case class StringLiteral(
  value: String,
  pos: Position = Position.NoPosition
) extends Expr

final case class BooleanLiteral(
  value: Boolean,
  pos: Position = Position.NoPosition
) extends Expr

case class SymbolLiteral(
  name: String,
  pos: Position = Position.NoPosition
) extends Expr

case class NilLiteral(
  pos: Position = Position.NoPosition
) extends Expr


/**
  * =========================================================
  * Variables
  * =========================================================
  */
final case class LocalVarRef(
  name: String,
  pos: Position = Position.NoPosition
) extends Expr

final case class LocalVarAssign(
  name: String,
  value: Expr,
  pos: Position = Position.NoPosition
) extends Expr

final case class InstanceVarRef(
  name: String,
  pos: Position = Position.NoPosition
) extends Expr

final case class InstanceVarAssign(
  name: String,
  value: Expr,
  pos: Position = Position.NoPosition
) extends Expr

final case class GlobalVarRef(
  name: String,
  pos: Position = Position.NoPosition
) extends Expr

final case class GlobalVarAssign(
  name: String,
  value: Expr,
  pos: Position = Position.NoPosition
) extends Expr

/** 定数参照 (クラス名など): Foo, A::B */
final case class ConstantRef(
  nameParts: List[String],         // ["A","B"]
  pos: Position = Position.NoPosition
) extends Expr


/**
  * =========================================================
  * Method Call（Ruby の演算子もすべてメソッド呼び出しに正規化）
  * =========================================================
  */
final case class MethodCall(
  receiver: Option[Expr],          // None = self
  methodName: String,
  args: List[Expr],
  block: Option[BlockExpr],        // { |x| ... } が付く場合
  pos: Position = Position.NoPosition
) extends Expr

object MethodCall:
  def toSelf(
    name: String,
    args: List[Expr],
    pos: Position = Position.NoPosition
  ): MethodCall =
    MethodCall(None, name, args, None, pos)


/**
  * =========================================================
  * Self
  * =========================================================
  */
final case class Self(
  pos: Position = Position.NoPosition
) extends Expr


/**
  * =========================================================
  * Sequence - 式の列
  *
  * Ruby の begin { } や class body / method body に対応
  * =========================================================
  */
final case class Sequence(
  exprs: List[Expr],
  pos: Position = Position.NoPosition
) extends Expr


/**
  * =========================================================
  * Control flow
  * =========================================================
  */
final case class IfExpr(
  condition: Expr,
  thenBranch: Expr,
  elseBranch: Option[Expr],
  pos: Position = Position.NoPosition
) extends Expr

final case class WhileExpr(
  condition: Expr,
  body: Expr,
  pos: Position = Position.NoPosition
) extends Expr


/**
  * =========================================================
  * Block ( { |x| ... } )
  *   → Rubyのブロックは Proc/Closure を作るので、Expr 扱い
  * =========================================================
  */
final case class BlockExpr(
  params: List[String],
  body: Expr,
  pos: Position = Position.NoPosition
) extends Expr


/**
  * =========================================================
  * MethodDef / ClassDef も Ruby 的には「式」
  *
  * def foo(x) ... end
  * class A ... end
  *
  * どちらも「最後に評価した式の値を返す」ため Expr に統合
  * =========================================================
  */
final case class MethodDef(
  name: String,
  params: List[String],
  body: Expr,
  pos: Position = Position.NoPosition
) extends Expr

final case class ClassDef(
  nameParts: List[String],          // class A::B
  superclass: Option[Expr],         // Optional superclass expression
  body: Expr,
  pos: Position = Position.NoPosition
) extends Expr
