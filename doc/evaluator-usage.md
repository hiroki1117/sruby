# Evaluator 使用ガイド

## 概要

Evaluator は Ruby の AST を評価して RValue を生成する、sruby インタプリタの心臓部です。

## アーキテクチャ

```
Evaluator
  ├─ Eval[A] = StateT[IO, VMState, A]  # 評価モナド
  ├─ VMState                            # 実行状態
  │   ├─ Frame (self, env, currentClass)
  │   └─ グローバル変数テーブル
  └─ 組み込みメソッド実装
```


## 使用例

### 基本的な使い方

```scala
import hiroki1117.sruby.interpreter.*
import hiroki1117.sruby.ast.*
import cats.effect.IO

// AST を構築
val ast = BinaryOp(
  IntLiteral(10),
  BinaryKind.Mul,
  IntLiteral(5)
)

// 評価実行
val result: IO[RValue] = EvaluatorHelper.evalTopLevel(ast)

// 結果を取得
result.unsafeRunSync() // => RInteger(50)
```

### 複雑な例：フィボナッチ数列（ループ版）

```scala
// n = 10
// a = 0
// b = 1
// i = 0
// while i < n do
//   temp = a
//   a = b
//   b = temp + b
//   i = i + 1
// end
// a

val ast = Sequence(List(
  LocalVarAssign("n", IntLiteral(10)),
  LocalVarAssign("a", IntLiteral(0)),
  LocalVarAssign("b", IntLiteral(1)),
  LocalVarAssign("i", IntLiteral(0)),
  WhileExpr(
    BinaryOp(LocalVarRef("i"), BinaryKind.Lt, LocalVarRef("n")),
    Sequence(List(
      LocalVarAssign("temp", LocalVarRef("a")),
      LocalVarAssign("a", LocalVarRef("b")),
      LocalVarAssign("b", BinaryOp(LocalVarRef("temp"), BinaryKind.Add, LocalVarRef("b"))),
      LocalVarAssign("i", BinaryOp(LocalVarRef("i"), BinaryKind.Add, IntLiteral(1)))
    ))
  ),
  LocalVarRef("a")
))

EvaluatorHelper.evalTopLevel(ast).unsafeRunSync()
// => RInteger(55) (10番目のフィボナッチ数)
```

### 文字列処理の例

```scala
// "hello".upcase + " " + "world".upcase

val ast = BinaryOp(
  BinaryOp(
    MethodCall(Some(StringLiteral("hello")), "upcase", List.empty),
    BinaryKind.Add,
    StringLiteral(" ")
  ),
  BinaryKind.Add,
  MethodCall(Some(StringLiteral("world")), "upcase", List.empty)
)

EvaluatorHelper.evalTopLevel(ast).unsafeRunSync()
// => RString("HELLO WORLD")
```


## 内部実装の詳細

### Eval モナド

```scala
type Eval[A] = StateT[IO, VMState, A]
```

- **IO**: 副作用（エラー、I/O など）
- **VMState**: インタプリタの状態（フレームスタック）
- **A**: 評価結果の型

### VMState の構造

```scala
case class VMState(
  frames: List[Frame],
  globalClasses: Map[String, RClass] = Map.empty
)

case class Frame(
  self: RValue,              // 現在の self
  env: Env,                  // ローカル変数環境
  currentClass: RClass       // 現在のクラス
)

case class Env(
  vars: Map[String, RValue] = Map.empty
)
```

### メソッド探索アルゴリズム

1. `receiver.rubyClass.lookupMethod(methodName)` でメソッドを探索
2. 見つかれば実行
3. 見つからなければ `tryBuiltinMethod` で組み込みメソッドをチェック
4. それでも見つからなければエラー（将来的には `method_missing` を呼び出し）

