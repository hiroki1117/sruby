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

  test("evaluate symbol literal") {
    val ast = SymbolLiteral("test")
    evalExpr(ast).map { result =>
      assertEquals(result, RSymbol.intern("test"))
      assertEquals(result.inspect, ":test")
    }
  }

  test("symbol literal interning") {
    val ast = Sequence(List(
      SymbolLiteral("foo"),
      SymbolLiteral("foo")
    ))
    evalExpr(ast).map { result =>
      // 2回目も同じオブジェクトが返る
      assertEquals(result, RSymbol.intern("foo"))
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

  test("evaluate obj.class returns class object") {
    val ast = MethodCall(Some(IntLiteral(42)), "class", List.empty)
    evalExpr(ast).map { result =>
      assertEquals(result, Builtins.IntegerClass)
      assertEquals(result.asInstanceOf[RClass].name, "Integer")
    }
  }

  test("evaluate string.class") {
    val ast = MethodCall(Some(StringLiteral("hello")), "class", List.empty)
    evalExpr(ast).map { result =>
      assertEquals(result, Builtins.StringClass)
      assertEquals(result.asInstanceOf[RClass].name, "String")
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
  
  // =========================================================
  // MethodDef のテスト
  // =========================================================
  
  test("define simple method and call it") {
    // def double(x)
    //   x * 2
    // end
    // double(5)
    val ast = Sequence(List(
      MethodDef(
        "double",
        List("x"),
        BinaryOp(LocalVarRef("x"), BinaryKind.Mul, IntLiteral(2))
      ),
      MethodCall(None, "double", List(IntLiteral(5)))
    ))
    evalExpr(ast).map { result =>
      assertEquals(result, RInteger(10))
    }
  }
  
  test("method definition returns symbol") {
    // def hello
    //   "world"
    // end
    val ast = MethodDef("hello", List.empty, StringLiteral("world"))
    evalExpr(ast).map { result =>
      assertEquals(result, RSymbol.intern("hello"))
    }
  }
  
  test("define method with multiple parameters") {
    // def add(a, b)
    //   a + b
    // end
    // add(3, 7)
    val ast = Sequence(List(
      MethodDef(
        "add",
        List("a", "b"),
        BinaryOp(LocalVarRef("a"), BinaryKind.Add, LocalVarRef("b"))
      ),
      MethodCall(None, "add", List(IntLiteral(3), IntLiteral(7)))
    ))
    evalExpr(ast).map { result =>
      assertEquals(result, RInteger(10))
    }
  }
  
  test("method with local variables") {
    // def complex(x)
    //   y = x * 2
    //   z = y + 10
    //   z
    // end
    // complex(5)
    val ast = Sequence(List(
      MethodDef(
        "complex",
        List("x"),
        Sequence(List(
          LocalVarAssign("y", BinaryOp(LocalVarRef("x"), BinaryKind.Mul, IntLiteral(2))),
          LocalVarAssign("z", BinaryOp(LocalVarRef("y"), BinaryKind.Add, IntLiteral(10))),
          LocalVarRef("z")
        ))
      ),
      MethodCall(None, "complex", List(IntLiteral(5)))
    ))
    evalExpr(ast).map { result =>
      // x = 5, y = 10, z = 20
      assertEquals(result, RInteger(20))
    }
  }
  
  test("method with conditional") {
    // def abs(x)
    //   if x < 0 then -x else x end
    // end
    // abs(-5)
    val ast = Sequence(List(
      MethodDef(
        "abs",
        List("x"),
        IfExpr(
          BinaryOp(LocalVarRef("x"), BinaryKind.Lt, IntLiteral(0)),
          UnaryOp(UnaryKind.Negate, LocalVarRef("x")),
          Some(LocalVarRef("x"))
        )
      ),
      MethodCall(None, "abs", List(IntLiteral(-5)))
    ))
    evalExpr(ast).map { result =>
      assertEquals(result, RInteger(5))
    }
  }
  
  test("wrong number of arguments raises error") {
    // def foo(a, b)
    //   a + b
    // end
    // foo(1)  # エラー
    val ast = Sequence(List(
      MethodDef(
        "foo",
        List("a", "b"),
        BinaryOp(LocalVarRef("a"), BinaryKind.Add, LocalVarRef("b"))
      ),
      MethodCall(None, "foo", List(IntLiteral(1)))
    ))
    evalExpr(ast).attempt.map { result =>
      assert(result.isLeft)
      assert(result.left.exists(_.getMessage.contains("wrong number of arguments")))
    }
  }
  
  // =========================================================
  // ClassDef のテスト
  // =========================================================
  
  test("define simple class") {
    // class Greeter
    // end
    val ast = ClassDef(List("Greeter"), None, NilLiteral)
    evalExpr(ast).map { result =>
      result match
        case klass: RClass =>
          assertEquals(klass.name, "Greeter")
          assertEquals(klass.superclass, Some(Builtins.ObjectClass))
        case _ => fail(s"Expected RClass, got ${result.inspect}")
    }
  }
  
  test("class definition returns class object") {
    // result = class Foo
    // end
    val ast = ClassDef(List("Foo"), None, NilLiteral)
    evalExpr(ast).map { result =>
      assert(result.isInstanceOf[RClass])
      assertEquals(result.asInstanceOf[RClass].name, "Foo")
    }
  }
  
  test("define class with method") {
    // class Calculator
    //   def add(a, b)
    //     a + b
    //   end
    // end
    val ast = ClassDef(
      List("Calculator"),
      None,
      MethodDef(
        "add",
        List("a", "b"),
        BinaryOp(LocalVarRef("a"), BinaryKind.Add, LocalVarRef("b"))
      )
    )
    evalExpr(ast).map { result =>
      result match
        case klass: RClass =>
          assertEquals(klass.name, "Calculator")
          // メソッドが定義されているか確認
          assert(klass.lookupMethod("add").isDefined)
        case _ => fail(s"Expected RClass, got ${result.inspect}")
    }
  }
  
  test("define class with inheritance") {
    // class Animal
    // end
    // class Dog < Animal
    // end
    val ast = Sequence(List(
      ClassDef(List("Animal"), None, NilLiteral),
      ClassDef(List("Dog"), Some(ConstantRef(List("Animal"))), NilLiteral)
    ))
    evalExpr(ast).map { result =>
      result match
        case klass: RClass =>
          assertEquals(klass.name, "Dog")
          assert(klass.superclass.isDefined)
          assertEquals(klass.superclass.get.name, "Animal")
        case _ => fail(s"Expected RClass, got ${result.inspect}")
    }
  }
  
  test("class reopening") {
    // class Calculator
    //   def add(a, b)
    //     a + b
    //   end
    // end
    // 
    // class Calculator
    //   def subtract(a, b)
    //     a - b
    //   end
    // end
    val ast = Sequence(List(
      ClassDef(
        List("Calculator"),
        None,
        MethodDef("add", List("a", "b"), BinaryOp(LocalVarRef("a"), BinaryKind.Add, LocalVarRef("b")))
      ),
      ClassDef(
        List("Calculator"),
        None,
        MethodDef("subtract", List("a", "b"), BinaryOp(LocalVarRef("a"), BinaryKind.Sub, LocalVarRef("b")))
      )
    ))
    evalExpr(ast).map { result =>
      result match
        case klass: RClass =>
          assertEquals(klass.name, "Calculator")
          // 両方のメソッドが定義されているか確認
          assert(klass.lookupMethod("add").isDefined)
          assert(klass.lookupMethod("subtract").isDefined)
        case _ => fail(s"Expected RClass, got ${result.inspect}")
    }
  }
  
  test("class with multiple methods") {
    // class Math
    //   def add(a, b)
    //     a + b
    //   end
    //   def multiply(a, b)
    //     a * b
    //   end
    // end
    val ast = ClassDef(
      List("Math"),
      None,
      Sequence(List(
        MethodDef("add", List("a", "b"), BinaryOp(LocalVarRef("a"), BinaryKind.Add, LocalVarRef("b"))),
        MethodDef("multiply", List("a", "b"), BinaryOp(LocalVarRef("a"), BinaryKind.Mul, LocalVarRef("b")))
      ))
    )
    evalExpr(ast).map { result =>
      result match
        case klass: RClass =>
          assertEquals(klass.name, "Math")
          assert(klass.lookupMethod("add").isDefined)
          assert(klass.lookupMethod("multiply").isDefined)
        case _ => fail(s"Expected RClass, got ${result.inspect}")
    }
  }

