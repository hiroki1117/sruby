package hiroki1117.sruby.ast

import munit.FunSuite

/**
  * AST ノードの基本的なテスト
  * 
  * 現時点では構造のテストのみ。
  * 評価のテストは Evaluator 実装後に追加。
  */
class ASTSpec extends FunSuite {
  
  // =========================================================
  // Position のテスト
  // =========================================================
  
  test("Position toString") {
    val pos = Position("example.rb", 10, 5)
    assertEquals(pos.toString, "example.rb:10:5")
  }
  
  test("Position.NoPosition") {
    assertEquals(Position.NoPosition.filename, "<unknown>")
    assertEquals(Position.NoPosition.line, 0)
    assertEquals(Position.NoPosition.column, 0)
  }
  
  test("Position.repl") {
    val pos = Position.repl(1, 0)
    assertEquals(pos.filename, "<repl>")
    assertEquals(pos.line, 1)
  }
  
  // =========================================================
  // リテラルのテスト
  // =========================================================
  
  test("IntLiteral") {
    val lit = IntLiteral(42)
    assertEquals(lit.value, 42L)
    assertEquals(lit.toString, "42")
  }
  
  test("IntLiteral with position") {
    val pos = Position("test.rb", 1, 0)
    val lit = IntLiteral(100, pos)
    assertEquals(lit.pos, pos)
  }
  
  test("StringLiteral") {
    val lit = StringLiteral("hello")
    assertEquals(lit.value, "hello")
    assertEquals(lit.toString, "\"hello\"")
  }
  
  test("StringLiteral empty") {
    val lit = StringLiteral("")
    assertEquals(lit.value, "")
    assertEquals(lit.toString, "\"\"")
  }
  
  test("NilLiteral") {
    assertEquals(NilLiteral.toString, "nil")
    assertEquals(NilLiteral.pos, Position.NoPosition)
  }
  
  test("BooleanLiteral true") {
    val lit = BooleanLiteral(true)
    assertEquals(lit.value, true)
    assertEquals(lit.toString, "true")
  }
  
  test("BooleanLiteral false") {
    val lit = BooleanLiteral(false)
    assertEquals(lit.value, false)
    assertEquals(lit.toString, "false")
  }
  
  test("SymbolLiteral") {
    val sym = SymbolLiteral("name")
    assertEquals(sym.name, "name")
    assertEquals(sym.toString, ":name")
  }
  
  // =========================================================
  // 変数のテスト
  // =========================================================
  
  test("LocalVarRef") {
    val varRef = LocalVarRef("x")
    assertEquals(varRef.name, "x")
    assertEquals(varRef.toString, "x")
  }
  
  test("LocalVarAssign") {
    val assign = LocalVarAssign("x", IntLiteral(10))
    assertEquals(assign.name, "x")
    assertEquals(assign.value, IntLiteral(10))
    assertEquals(assign.toString, "x = 10")
  }
  
  test("LocalVarAssign with expression") {
    val assign = LocalVarAssign(
      "result",
      BinaryOp(LocalVarRef("x"), BinaryKind.Add, LocalVarRef("y"))
    )
    assertEquals(assign.name, "result")
    assert(assign.value.isInstanceOf[BinaryOp])
  }
  
  test("InstanceVarRef") {
    val varRef = InstanceVarRef("@name")
    assertEquals(varRef.name, "@name")
    assertEquals(varRef.toString, "@name")
  }
  
  test("InstanceVarAssign") {
    val assign = InstanceVarAssign("@name", StringLiteral("Alice"))
    assertEquals(assign.name, "@name")
    assertEquals(assign.value, StringLiteral("Alice"))
  }
  
  // =========================================================
  // 二項演算のテスト
  // =========================================================
  
  test("BinaryOp addition") {
    val op = BinaryOp(IntLiteral(1), BinaryKind.Add, IntLiteral(2))
    assertEquals(op.kind, BinaryKind.Add)
    assertEquals(op.left, IntLiteral(1))
    assertEquals(op.right, IntLiteral(2))
    assertEquals(op.toString, "(1 + 2)")
  }
  
  test("BinaryOp nested") {
    // (1 + 2) * 3
    val inner = BinaryOp(IntLiteral(1), BinaryKind.Add, IntLiteral(2))
    val outer = BinaryOp(inner, BinaryKind.Mul, IntLiteral(3))
    assertEquals(outer.toString, "((1 + 2) * 3)")
  }
  
  test("BinaryOp with variables") {
    val op = BinaryOp(LocalVarRef("x"), BinaryKind.Mul, LocalVarRef("y"))
    assertEquals(op.toString, "(x * y)")
  }
  
  test("BinaryOp comparison") {
    val op = BinaryOp(IntLiteral(10), BinaryKind.Gt, IntLiteral(5))
    assertEquals(op.kind, BinaryKind.Gt)
  }
  
  // =========================================================
  // メソッド呼び出しのテスト
  // =========================================================
  
  test("MethodCall with receiver") {
    val call = MethodCall(
      Some(StringLiteral("hello")),
      "upcase",
      List.empty
    )
    assertEquals(call.receiver, Some(StringLiteral("hello")))
    assertEquals(call.methodName, "upcase")
    assertEquals(call.args, List.empty)
    assertEquals(call.toString, "\"hello\".upcase")
  }
  
  test("MethodCall with arguments") {
    val call = MethodCall(
      Some(LocalVarRef("obj")),
      "method",
      List(IntLiteral(1), IntLiteral(2))
    )
    assertEquals(call.args.length, 2)
    assertEquals(call.toString, "obj.method(1, 2)")
  }
  
  test("MethodCall to self") {
    val call = MethodCall.toSelf("puts", List(StringLiteral("hello")))
    assertEquals(call.receiver, None)
    assertEquals(call.methodName, "puts")
    assertEquals(call.toString, "puts(\"hello\")")
  }
  
  test("MethodCall without receiver and without args") {
    val call = MethodCall(None, "foo", List.empty)
    assertEquals(call.toString, "foo")
  }
  
  // =========================================================
  // シーケンスのテスト
  // =========================================================
  
  test("Sequence with multiple expressions") {
    val seq = Sequence(List(
      LocalVarAssign("x", IntLiteral(10)),
      LocalVarAssign("y", IntLiteral(20)),
      BinaryOp(LocalVarRef("x"), BinaryKind.Add, LocalVarRef("y"))
    ))
    assertEquals(seq.exprs.length, 3)
  }
  
  test("Sequence.empty") {
    val seq = Sequence.empty()
    assertEquals(seq.exprs, List.empty)
  }
  
  test("Sequence toString") {
    val seq = Sequence(List(IntLiteral(1), IntLiteral(2)))
    assertEquals(seq.toString, "{ 1; 2 }")
  }
  
  // =========================================================
  // Self のテスト
  // =========================================================
  
  test("Self") {
    assertEquals(Self.toString, "self")
    assertEquals(Self.pos, Position.NoPosition)
  }
  
  // =========================================================
  // 将来実装予定のノードの構造テスト
  // =========================================================
  
  test("IfExpr structure") {
    val ifExpr = IfExpr(
      BinaryOp(LocalVarRef("x"), BinaryKind.Gt, IntLiteral(10)),
      StringLiteral("big"),
      Some(StringLiteral("small"))
    )
    assert(ifExpr.condition.isInstanceOf[BinaryOp])
    assertEquals(ifExpr.thenBranch, StringLiteral("big"))
    assertEquals(ifExpr.elseBranch, Some(StringLiteral("small")))
  }
  
  test("IfExpr without else") {
    val ifExpr = IfExpr(
      BooleanLiteral(true),
      IntLiteral(42),
      None
    )
    assertEquals(ifExpr.elseBranch, None)
  }
  
  test("WhileExpr structure") {
    val whileExpr = WhileExpr(
      BinaryOp(LocalVarRef("x"), BinaryKind.Lt, IntLiteral(10)),
      LocalVarAssign("x", BinaryOp(LocalVarRef("x"), BinaryKind.Add, IntLiteral(1)))
    )
    assert(whileExpr.condition.isInstanceOf[BinaryOp])
    assert(whileExpr.body.isInstanceOf[LocalVarAssign])
  }
  
  test("BlockExpr structure") {
    val block = BlockExpr(
      List("x"),
      BinaryOp(LocalVarRef("x"), BinaryKind.Mul, IntLiteral(2))
    )
    assertEquals(block.params, List("x"))
    assert(block.body.isInstanceOf[BinaryOp])
  }
  
  test("BlockExpr without params") {
    val block = BlockExpr(List.empty, IntLiteral(42))
    assertEquals(block.params, List.empty)
  }
  
  // =========================================================
  // 複雑な AST の構築テスト
  // =========================================================
  
  test("Complex AST: x = 10; y = 20; x + y") {
    val ast = Sequence(List(
      LocalVarAssign("x", IntLiteral(10)),
      LocalVarAssign("y", IntLiteral(20)),
      BinaryOp(LocalVarRef("x"), BinaryKind.Add, LocalVarRef("y"))
    ))
    
    assertEquals(ast.exprs.length, 3)
    assert(ast.exprs(0).isInstanceOf[LocalVarAssign])
    assert(ast.exprs(1).isInstanceOf[LocalVarAssign])
    assert(ast.exprs(2).isInstanceOf[BinaryOp])
  }
  
  test("Complex AST: 'hello'.upcase + ' world'") {
    val ast = BinaryOp(
      MethodCall(Some(StringLiteral("hello")), "upcase", List.empty),
      BinaryKind.Add,
      StringLiteral(" world")
    )
    
    assert(ast.left.isInstanceOf[MethodCall])
    assert(ast.right.isInstanceOf[StringLiteral])
  }
  
  test("Complex AST: obj.method(x + 1, y * 2)") {
    val ast = MethodCall(
      Some(LocalVarRef("obj")),
      "method",
      List(
        BinaryOp(LocalVarRef("x"), BinaryKind.Add, IntLiteral(1)),
        BinaryOp(LocalVarRef("y"), BinaryKind.Mul, IntLiteral(2))
      )
    )
    
    assertEquals(ast.args.length, 2)
    assert(ast.args(0).isInstanceOf[BinaryOp])
    assert(ast.args(1).isInstanceOf[BinaryOp])
  }
}

