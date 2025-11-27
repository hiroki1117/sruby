package hiroki1117.sruby.interpreter

import hiroki1117.sruby.core.*
import hiroki1117.sruby.ast.*
import munit.CatsEffectSuite
import cats.effect.IO

/**
  * Evaluator のテスト
  * 
  * 手動で AST を構築して評価をテストする
  */
class EvaluatorSpec extends CatsEffectSuite:
  
  /**
    * AST を評価して結果を取得するヘルパー
    */
  def evalExpr(expr: Expr): IO[RValue] =
    EvaluatorHelper.evalTopLevel(expr)
  
  // =========================================================
  // リテラルのテスト
  // =========================================================
  
  test("evaluate nil literal") {
    val ast = NilLiteral
    evalExpr(ast).map { result =>
      assertEquals(result, RNil)
    }
  }
  
  test("evaluate integer literal") {
    val ast = IntLiteral(42)
    evalExpr(ast).map { result =>
      assertEquals(result, RInteger(42))
    }
  }
  
  test("evaluate string literal") {
    val ast = StringLiteral("hello")
    evalExpr(ast).map { result =>
      assertEquals(result, RString("hello"))
    }
  }
  
  test("evaluate boolean literal true") {
    val ast = BooleanLiteral(true)
    evalExpr(ast).map { result =>
      assertEquals(result, RTrue)
    }
  }
  
  test("evaluate boolean literal false") {
    val ast = BooleanLiteral(false)
    evalExpr(ast).map { result =>
      assertEquals(result, RFalse)
    }
  }
  
  // =========================================================
  // 変数のテスト
  // =========================================================
  
  test("evaluate local variable assignment") {
    val ast = LocalVarAssign("x", IntLiteral(10))
    evalExpr(ast).map { result =>
      assertEquals(result, RInteger(10))
    }
  }
  
  test("evaluate local variable assignment and reference") {
    val ast = Sequence(List(
      LocalVarAssign("x", IntLiteral(10)),
      LocalVarRef("x")
    ))
    evalExpr(ast).map { result =>
      assertEquals(result, RInteger(10))
    }
  }
  
  test("undefined local variable raises error") {
    val ast = LocalVarRef("undefined_var")
    evalExpr(ast).attempt.map { result =>
      assert(result.isLeft)
      assert(result.left.exists(_.getMessage.contains("undefined local variable")))
    }
  }
  
  // =========================================================
  // 二項演算のテスト
  // =========================================================
  
  test("evaluate addition: 1 + 2") {
    val ast = BinaryOp(IntLiteral(1), BinaryKind.Add, IntLiteral(2))
    evalExpr(ast).map { result =>
      assertEquals(result, RInteger(3))
    }
  }
  
  test("evaluate subtraction: 10 - 3") {
    val ast = BinaryOp(IntLiteral(10), BinaryKind.Sub, IntLiteral(3))
    evalExpr(ast).map { result =>
      assertEquals(result, RInteger(7))
    }
  }
  
  test("evaluate multiplication: 6 * 7") {
    val ast = BinaryOp(IntLiteral(6), BinaryKind.Mul, IntLiteral(7))
    evalExpr(ast).map { result =>
      assertEquals(result, RInteger(42))
    }
  }
  
  test("evaluate division: 10 / 2") {
    val ast = BinaryOp(IntLiteral(10), BinaryKind.Div, IntLiteral(2))
    evalExpr(ast).map { result =>
      assertEquals(result, RInteger(5))
    }
  }
  
  test("evaluate modulo: 10 % 3") {
    val ast = BinaryOp(IntLiteral(10), BinaryKind.Mod, IntLiteral(3))
    evalExpr(ast).map { result =>
      assertEquals(result, RInteger(1))
    }
  }
  
  test("evaluate nested arithmetic: (1 + 2) * 3") {
    val ast = BinaryOp(
      BinaryOp(IntLiteral(1), BinaryKind.Add, IntLiteral(2)),
      BinaryKind.Mul,
      IntLiteral(3)
    )
    evalExpr(ast).map { result =>
      assertEquals(result, RInteger(9))
    }
  }
  
  // =========================================================
  // 比較演算のテスト
  // =========================================================
  
  test("evaluate equality: 5 == 5") {
    val ast = BinaryOp(IntLiteral(5), BinaryKind.Eq, IntLiteral(5))
    evalExpr(ast).map { result =>
      assertEquals(result, RTrue)
    }
  }
  
  test("evaluate inequality: 5 == 3") {
    val ast = BinaryOp(IntLiteral(5), BinaryKind.Eq, IntLiteral(3))
    evalExpr(ast).map { result =>
      assertEquals(result, RFalse)
    }
  }
  
  test("evaluate less than: 3 < 5") {
    val ast = BinaryOp(IntLiteral(3), BinaryKind.Lt, IntLiteral(5))
    evalExpr(ast).map { result =>
      assertEquals(result, RTrue)
    }
  }
  
  test("evaluate greater than: 10 > 5") {
    val ast = BinaryOp(IntLiteral(10), BinaryKind.Gt, IntLiteral(5))
    evalExpr(ast).map { result =>
      assertEquals(result, RTrue)
    }
  }
  
  // =========================================================
  // 単項演算のテスト
  // =========================================================
  
  test("evaluate unary minus: -5") {
    val ast = UnaryOp(UnaryKind.Negate, IntLiteral(5))
    evalExpr(ast).map { result =>
      assertEquals(result, RInteger(-5))
    }
  }
  
  test("evaluate unary plus: +5") {
    val ast = UnaryOp(UnaryKind.Positive, IntLiteral(5))
    evalExpr(ast).map { result =>
      assertEquals(result, RInteger(5))
    }
  }
  
  // =========================================================
  // 文字列演算のテスト
  // =========================================================
  
  test("evaluate string concatenation") {
    val ast = BinaryOp(StringLiteral("Hello"), BinaryKind.Add, StringLiteral(" World"))
    evalExpr(ast).map { result =>
      assertEquals(result, RString("Hello World"))
    }
  }
  
  test("evaluate string upcase") {
    val ast = MethodCall(Some(StringLiteral("hello")), "upcase", List.empty)
    evalExpr(ast).map { result =>
      assertEquals(result, RString("HELLO"))
    }
  }
  
  test("evaluate string downcase") {
    val ast = MethodCall(Some(StringLiteral("WORLD")), "downcase", List.empty)
    evalExpr(ast).map { result =>
      assertEquals(result, RString("world"))
    }
  }
  
  test("evaluate string length") {
    val ast = MethodCall(Some(StringLiteral("hello")), "length", List.empty)
    evalExpr(ast).map { result =>
      assertEquals(result, RInteger(5))
    }
  }
  
  // =========================================================
  // シーケンスのテスト
  // =========================================================
  
  test("evaluate empty sequence returns nil") {
    val ast = Sequence(List.empty)
    evalExpr(ast).map { result =>
      assertEquals(result, RNil)
    }
  }
  
  test("evaluate sequence returns last value") {
    val ast = Sequence(List(
      IntLiteral(1),
      IntLiteral(2),
      IntLiteral(3)
    ))
    evalExpr(ast).map { result =>
      assertEquals(result, RInteger(3))
    }
  }
  
  test("evaluate sequence: x = 10; y = 20; x + y") {
    val ast = Sequence(List(
      LocalVarAssign("x", IntLiteral(10)),
      LocalVarAssign("y", IntLiteral(20)),
      BinaryOp(LocalVarRef("x"), BinaryKind.Add, LocalVarRef("y"))
    ))
    evalExpr(ast).map { result =>
      assertEquals(result, RInteger(30))
    }
  }
  
  // =========================================================
  // If 式のテスト
  // =========================================================
  
  test("evaluate if with true condition") {
    val ast = IfExpr(
      BooleanLiteral(true),
      IntLiteral(42),
      Some(IntLiteral(0))
    )
    evalExpr(ast).map { result =>
      assertEquals(result, RInteger(42))
    }
  }
  
  test("evaluate if with false condition") {
    val ast = IfExpr(
      BooleanLiteral(false),
      IntLiteral(42),
      Some(IntLiteral(0))
    )
    evalExpr(ast).map { result =>
      assertEquals(result, RInteger(0))
    }
  }
  
  test("evaluate if without else branch (false condition)") {
    val ast = IfExpr(
      BooleanLiteral(false),
      IntLiteral(42),
      None
    )
    evalExpr(ast).map { result =>
      assertEquals(result, RNil)
    }
  }
  
  test("evaluate if with comparison: if 10 > 5 then 'yes' else 'no' end") {
    val ast = IfExpr(
      BinaryOp(IntLiteral(10), BinaryKind.Gt, IntLiteral(5)),
      StringLiteral("yes"),
      Some(StringLiteral("no"))
    )
    evalExpr(ast).map { result =>
      assertEquals(result, RString("yes"))
    }
  }
  
  // =========================================================
  // While ループのテスト
  // =========================================================
  
  test("evaluate while loop (never executes)") {
    val ast = WhileExpr(
      BooleanLiteral(false),
      IntLiteral(42)
    )
    evalExpr(ast).map { result =>
      assertEquals(result, RNil)
    }
  }
  
  test("evaluate while loop with counter") {
    // i = 0; while i < 3 do i = i + 1 end; i
    val ast = Sequence(List(
      LocalVarAssign("i", IntLiteral(0)),
      WhileExpr(
        BinaryOp(LocalVarRef("i"), BinaryKind.Lt, IntLiteral(3)),
        LocalVarAssign("i", BinaryOp(LocalVarRef("i"), BinaryKind.Add, IntLiteral(1)))
      ),
      LocalVarRef("i")
    ))
    evalExpr(ast).map { result =>
      assertEquals(result, RInteger(3))
    }
  }
  
  // =========================================================
  // 複雑な式のテスト
  // =========================================================
  
  test("evaluate complex expression with variables and operations") {
    // x = 5
    // y = 3
    // z = x * y + 10
    // if z > 20 then z - 5 else z + 5 end
    val ast = Sequence(List(
      LocalVarAssign("x", IntLiteral(5)),
      LocalVarAssign("y", IntLiteral(3)),
      LocalVarAssign("z", BinaryOp(
        BinaryOp(LocalVarRef("x"), BinaryKind.Mul, LocalVarRef("y")),
        BinaryKind.Add,
        IntLiteral(10)
      )),
      IfExpr(
        BinaryOp(LocalVarRef("z"), BinaryKind.Gt, IntLiteral(20)),
        BinaryOp(LocalVarRef("z"), BinaryKind.Sub, IntLiteral(5)),
        Some(BinaryOp(LocalVarRef("z"), BinaryKind.Add, IntLiteral(5)))
      )
    ))
    evalExpr(ast).map { result =>
      // x * y + 10 = 5 * 3 + 10 = 25
      // 25 > 20 => true
      // 25 - 5 = 20
      assertEquals(result, RInteger(20))
    }
  }

