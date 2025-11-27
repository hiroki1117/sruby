package hiroki1117.sruby.core

import munit.FunSuite
import cats.effect.IO
import cats.data.StateT
import cats.effect.unsafe.implicits.global

/**
  * RValue の基本的なテスト
  * 
  * 注意: 現在は最小限の実装のみテスト
  * TODO: 演算子オーバーロード、コレクション型などは将来実装
  */
class RValueSpec extends FunSuite {
  
  // =========================================================
  // Nil, True, False の基本テスト
  // =========================================================
  
  test("RNil should be falsy") {
    assert(!RNil.isTruthy)
    assertEquals(RNil.toRubyString, "")
    assertEquals(RNil.inspect, "nil")
  }
  
  test("RTrue should be truthy") {
    assert(RTrue.isTruthy)
    assertEquals(RTrue.toRubyString, "true")
  }
  
  test("RFalse should be falsy") {
    assert(!RFalse.isTruthy)
    assertEquals(RFalse.toRubyString, "false")
  }
  
  // =========================================================
  // RInteger の基本テスト
  // =========================================================
  
  test("RInteger to_s and inspect") {
    assertEquals(RInteger(42).toRubyString, "42")
    assertEquals(RInteger(-100).toRubyString, "-100")
    assertEquals(RInteger(0).inspect, "0")
  }
  
  test("RInteger class") {
    assertEquals(RInteger(42).rubyClass.name, "Integer")
    assertEquals(RInteger(42).rubyClass, Builtins.IntegerClass)
  }
  
  // =========================================================
  // RString の基本テスト
  // =========================================================
  
  test("RString inspect should quote the string") {
    assertEquals(RString("hello").inspect, "\"hello\"")
    assertEquals(RString("hello").toRubyString, "hello")
  }
  
  test("RString class") {
    assertEquals(RString("test").rubyClass.name, "String")
    assertEquals(RString("test").rubyClass, Builtins.StringClass)
  }
  
  test("RString empty") {
    assertEquals(RString("").toRubyString, "")
    assertEquals(RString("").inspect, "\"\"")
  }
  
  // =========================================================
  // RSymbol のテスト
  // =========================================================
  
  test("RSymbol intern returns same object for same name") {
    val sym1 = RSymbol.intern("test")
    val sym2 = RSymbol.intern("test")
    assert(sym1 eq sym2, "Same symbol name should return same object")
  }

  test("RSymbol different names return different objects") {
    val sym1 = RSymbol.intern("foo")
    val sym2 = RSymbol.intern("bar")
    assert(sym1 ne sym2, "Different symbol names should return different objects")
  }

  test("RSymbol inspect") {
    assertEquals(RSymbol.intern("hello").inspect, ":hello")
  }

  test("RSymbol to_s") {
    assertEquals(RSymbol.intern("world").toRubyString, "world")
  }

  test("RSymbol class") {
    assertEquals(RSymbol.intern("test").rubyClass, Builtins.SymbolClass)
    assertEquals(Builtins.SymbolClass.name, "Symbol")
  }
  
  // =========================================================
  // RClass の階層とメソッド探索
  // =========================================================
  
  test("Builtins class hierarchy") {
    assertEquals(Builtins.IntegerClass.name, "Integer")
    assertEquals(Builtins.IntegerClass.superclass, Some(Builtins.ObjectClass))
    assertEquals(Builtins.ObjectClass.superclass, None)
    assertEquals(Builtins.ClassClass.superclass, Some(Builtins.ObjectClass))
  }
  
  test("RClass method lookup") {
    // ダミーメソッドを作成（StateT を正しく使う）
    val method = RMethod((receiver, args) => 
      StateT.pure[IO, VMState, RValue](RTrue)
    )
    
    val klass = Builtins.ObjectClass.defineMethod("test", method)
    
    assertEquals(klass.lookupMethod("test"), Some(method))
    assertEquals(klass.lookupMethod("nonexistent"), None)
  }
  
  test("RClass method lookup through inheritance") {
    val method = RMethod((receiver, args) => 
      StateT.pure[IO, VMState, RValue](RTrue)
    )
    
    val parent = RClass("Parent", None).defineMethod("inherited_method", method)
    val child = RClass("Child", Some(parent))
    
    assertEquals(child.lookupMethod("inherited_method"), Some(method))
    assertEquals(child.lookupMethod("only_in_child"), None)
  }
  
  test("RClass.rubyClass is ClassClass") {
    assertEquals(Builtins.IntegerClass.rubyClass, Builtins.ClassClass)
    assertEquals(Builtins.ObjectClass.rubyClass, Builtins.ClassClass)
    assertEquals(Builtins.ClassClass.rubyClass, Builtins.ClassClass)
  }
  
  // =========================================================
  // RValue classes
  // =========================================================
  
  test("RValue classes") {
    assertEquals(RNil.rubyClass.name, "NilClass")
    assertEquals(RTrue.rubyClass.name, "TrueClass")
    assertEquals(RFalse.rubyClass.name, "FalseClass")
    assertEquals(RInteger(42).rubyClass.name, "Integer")
    assertEquals(RString("test").rubyClass.name, "String")
  }
  
  test("All builtin values have Object as ancestor") {
    assertEquals(Builtins.NilClass.superclass, Some(Builtins.ObjectClass))
    assertEquals(Builtins.TrueClass.superclass, Some(Builtins.ObjectClass))
    assertEquals(Builtins.FalseClass.superclass, Some(Builtins.ObjectClass))
    assertEquals(Builtins.IntegerClass.superclass, Some(Builtins.ObjectClass))
    assertEquals(Builtins.StringClass.superclass, Some(Builtins.ObjectClass))
  }
  
  // =========================================================
  // RObject インスタンス変数
  // =========================================================
  
  test("RObject instance variables") {
    val obj = RObject(Builtins.ObjectClass)
    assertEquals(obj.getInstanceVariable("@name"), None)
    
    val updated = obj.setInstanceVariable("@name", RString("Alice"))
    assertEquals(updated.getInstanceVariable("@name"), Some(RString("Alice")))
  }
  
  test("RObject with multiple instance variables") {
    val obj = RObject(Builtins.ObjectClass)
      .setInstanceVariable("@name", RString("Alice"))
      .setInstanceVariable("@age", RInteger(30))
    
    assertEquals(obj.getInstanceVariable("@name"), Some(RString("Alice")))
    assertEquals(obj.getInstanceVariable("@age"), Some(RInteger(30)))
    assertEquals(obj.getInstanceVariable("@unknown"), None)
  }
  
  test("RObject inspect shows class and object id") {
    val obj = RObject(Builtins.ObjectClass)
    val inspectStr = obj.inspect
    
    assert(inspectStr.startsWith("#<Object:0x"))
    assert(inspectStr.endsWith(">"))
  }
  
  test("RObject with custom class") {
    val personClass = RClass("Person", Some(Builtins.ObjectClass))
    val person = RObject(personClass)
      .setInstanceVariable("@name", RString("Bob"))
    
    assertEquals(person.rubyClass.name, "Person")
    assert(person.inspect.contains("Person"))
  }
  
  // =========================================================
  // Env - ローカル変数環境
  // =========================================================
  
  test("Env get and set") {
    val env = Env()
    assertEquals(env.get("x"), None)
    
    val env2 = env.set("x", RInteger(10))
    assertEquals(env2.get("x"), Some(RInteger(10)))
  }
  
  test("Env with multiple variables") {
    val env = Env()
      .set("x", RInteger(10))
      .set("y", RString("hello"))
      .set("z", RTrue)
    
    assertEquals(env.get("x"), Some(RInteger(10)))
    assertEquals(env.get("y"), Some(RString("hello")))
    assertEquals(env.get("z"), Some(RTrue))
    assertEquals(env.get("unknown"), None)
  }
  
  test("Env variable shadowing") {
    val env = Env()
      .set("x", RInteger(10))
      .set("x", RInteger(20))  // 上書き
    
    assertEquals(env.get("x"), Some(RInteger(20)))
  }
  
  // =========================================================
  // Frame - 実行フレーム
  // =========================================================
  
  test("Frame contains self, env, and currentClass") {
    val obj = RObject(Builtins.ObjectClass)
    val env = Env().set("x", RInteger(42))
    val frame = Frame(obj, env, Builtins.ObjectClass)
    
    assertEquals(frame.self, obj)
    assertEquals(frame.env.get("x"), Some(RInteger(42)))
    assertEquals(frame.currentClass, Builtins.ObjectClass)
  }
  
  test("Frame with different self types") {
    val intFrame = Frame(RInteger(10), Env(), Builtins.IntegerClass)
    val strFrame = Frame(RString("hi"), Env(), Builtins.StringClass)
    
    assertEquals(intFrame.self, RInteger(10))
    assertEquals(strFrame.self, RString("hi"))
  }
  
  // =========================================================
  // VMState - インタプリタ状態
  // =========================================================
  
  test("VMState frame stack operations") {
    val frame1 = Frame(RNil, Env(), Builtins.ObjectClass)
    val frame2 = Frame(RTrue, Env(), Builtins.TrueClass)
    
    val state = VMState(List(frame1))
    assertEquals(state.currentFrame, frame1)
    assertEquals(state.frames.length, 1)
    
    val state2 = state.pushFrame(frame2)
    assertEquals(state2.currentFrame, frame2)
    assertEquals(state2.frames.length, 2)
    
    val (popped, state3) = state2.popFrame()
    assertEquals(popped, frame2)
    assertEquals(state3.currentFrame, frame1)
    assertEquals(state3.frames.length, 1)
  }
  
  test("VMState global classes") {
    val myClass = RClass("MyClass", Some(Builtins.ObjectClass))
    val state = VMState(
      frames = List.empty,
      globalClasses = Map("MyClass" -> myClass)
    )
    
    assertEquals(state.globalClasses.get("MyClass"), Some(myClass))
    assertEquals(state.globalClasses.get("NoSuchClass"), None)
  }
  
  test("VMState with empty frames") {
    val state = VMState(List.empty)
    assertEquals(state.frames.isEmpty, true)
  }
  
  // =========================================================
  // RMethod - メソッド表現
  // =========================================================
  
  test("RMethod can be invoked") {
    val method = RMethod((receiver, args) => 
      StateT.pure[IO, VMState, RValue](RInteger(42))
    )
    
    val initialState = VMState(List(Frame(RNil, Env(), Builtins.ObjectClass)))
    val result = method.invoke(RNil, List.empty).run(initialState).unsafeRunSync()
    
    assertEquals(result._2, RInteger(42))
  }
  
  test("RMethod receives receiver and arguments") {
    val method = RMethod((receiver, args) => 
      StateT.pure[IO, VMState, RValue](receiver)
    )
    
    val obj = RString("test")
    val initialState = VMState(List(Frame(obj, Env(), Builtins.StringClass)))
    val result = method.invoke(obj, List.empty).run(initialState).unsafeRunSync()
    
    assertEquals(result._2, RString("test"))
  }
}

// =========================================================
// TODO: 将来実装予定の型とメソッド
// =========================================================
// - RBoolean ヘルパー (RBoolean(true) => RTrue)
// - RFloat
// - RSymbol
// - RArray
// - RHash
// - RRange
// - RProc
// - 各型の演算子オーバーロード（Builtin メソッドとして）
//   * RInteger: +, -, *, /, %, <, >, <=, >=, ==
//   * RString: +, length, upcase, downcase, reverse
//   * RArray: push, <<, +, length, at
//   * RHash: get, set, length
// =========================================================
