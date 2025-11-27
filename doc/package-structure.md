# sruby パッケージ階層設計

## 概要

srubyは、ScalaによるRubyインタプリタの実装です。
このドキュメントでは、プロジェクト全体のパッケージ構造と各パッケージの役割を説明します。

## パッケージ階層

```
hiroki1117.sruby/
├── ast/                      // 抽象構文木(AST)の定義
├── lexer/                    // 字句解析
├── parser/                   // 構文解析
├── runtime/                  // ランタイムシステム
├── core/                     // Rubyのコアクラス
├── builtin/                  // 組み込みメソッド
├── interpreter/              // インタプリタ/評価器
├── method/                   // メソッド関連
├── block/                    // ブロック/イテレータ
├── exception/                // 例外処理
├── stdlib/                   // 標準ライブラリ
├── util/                     // ユーティリティ
└── repl/                     // REPL (Read-Eval-Print Loop)
```

## 各パッケージの詳細

### 1. ast/ - 抽象構文木

Rubyコードを表現する抽象構文木(AST)のノード定義。

**主要コンポーネント:**
- `Node.scala` - すべてのASTノードの基底トレイト
- `Expression.scala` - 式を表現するノード（メソッド呼び出し、演算子、変数参照など）
- `Statement.scala` - 文を表現するノード（クラス定義、メソッド定義、代入文など）
- `Literal.scala` - リテラル値（数値、文字列、シンボル、配列リテラルなど）
- `Visitor.scala` - ASTを走査するためのビジターパターン実装

**例:**
```scala
sealed trait Node
case class IntLiteral(value: Int) extends Node
case class MethodCall(receiver: Node, name: String, args: List[Node]) extends Node
```

---

### 2. lexer/ - 字句解析

ソースコードを読み込んでトークン列に変換する字句解析器。

**主要コンポーネント:**
- `Token.scala` - トークンの種類定義（識別子、キーワード、演算子、リテラルなど）
- `Lexer.scala` - 字句解析器本体
- `Position.scala` - ソースコード内の位置情報（ファイル名、行番号、列番号）

**責務:**
- 文字列からトークン列への変換
- 空白やコメントの処理
- エラー報告のための位置情報の保持

---

### 3. parser/ - 構文解析

トークン列をASTに変換する構文解析器。

**主要コンポーネント:**
- `Parser.scala` - パーサー本体（再帰下降パーサーまたはコンビネータパーサー）
- `Precedence.scala` - 演算子の優先順位と結合性の定義
- `Error.scala` - 構文エラーの表現と報告

**責務:**
- トークン列からASTへの変換
- 文法エラーの検出と報告
- 演算子の優先順位処理

---

### 4. runtime/ - ランタイムシステム

Rubyプログラムの実行環境を管理するランタイムシステム。

**主要コンポーネント:**
- `RubyRuntime.scala` - ランタイム全体の初期化と管理
- `ObjectSpace.scala` - 生成されたすべてのオブジェクトの管理
- `GC.scala` - ガベージコレクション（将来的な最適化用）
- `ThreadContext.scala` - スレッドローカルな実行コンテキスト

**責務:**
- グローバル定数の管理
- トップレベルのselfの提供
- スレッドごとの実行コンテキスト管理

---

### 5. core/ - Rubyのコアクラス

Ruby言語のコアクラスの実装。

**主要コンポーネント:**
- `RubyObject.scala` - Object クラス（すべてのオブジェクトの基底）
- `RubyClass.scala` - Class クラス
- `RubyModule.scala` - Module クラス
- `RubyString.scala` - String クラス
- `RubyNumeric.scala` - Numeric/Integer/Float クラス
- `RubyArray.scala` - Array クラス
- `RubyHash.scala` - Hash クラス
- `RubySymbol.scala` - Symbol クラス
- `RubyProc.scala` - Proc/Lambda
- `RubyRange.scala` - Range クラス
- `RubyRegexp.scala` - Regexp クラス

**設計方針:**
- Rubyのオブジェクトモデルを忠実に再現
- クラス階層とメタクラスの実装
- インスタンス変数とメソッドの管理

---

### 6. builtin/ - 組み込みメソッド

各コアクラスの組み込みメソッドの実装。

**主要コンポーネント:**
- `BasicObjectMethods.scala` - BasicObject のメソッド
- `ObjectMethods.scala` - Object のメソッド
- `KernelMethods.scala` - Kernel モジュールのメソッド（puts, print, requireなど）
- `StringMethods.scala` - String のメソッド
- `ArrayMethods.scala` - Array のメソッド
- その他各クラスのメソッド実装

**例:**
```ruby
"hello".upcase  # => "HELLO"
[1, 2, 3].map { |x| x * 2 }  # => [2, 4, 6]
```

---

### 7. interpreter/ - インタプリタ/評価器

ASTを評価してRubyプログラムを実行する。

**主要コンポーネント:**
- `Evaluator.scala` - AST評価器（ASTノードを実行）
- `Context.scala` - 評価コンテキスト（selfやローカル変数の管理）
- `Scope.scala` - 変数スコープの管理（ローカル/インスタンス/クラス/グローバル）
- `MethodLookup.scala` - メソッド探索アルゴリズム

**責務:**
- ASTの評価と実行
- 変数の解決
- メソッド呼び出しの処理

---

### 8. method/ - メソッド関連

メソッドの定義、呼び出し、ディスパッチの管理。

**主要コンポーネント:**
- `MethodDefinition.scala` - メソッド定義の表現
- `MethodVisibility.scala` - メソッドの可視性（public/private/protected）
- `MethodCache.scala` - メソッド探索のキャッシュ（パフォーマンス最適化）
- `Dispatch.scala` - メソッドディスパッチロジック

**責務:**
- メソッドの定義と格納
- メソッド探索（継承チェーンとミックスイン）
- 動的メソッド呼び出し

---

### 9. block/ - ブロック/イテレータ

Rubyのブロックとイテレータの実装。

**主要コンポーネント:**
- `Block.scala` - ブロックの表現
- `Binding.scala` - ブロックのバインディング（クロージャ）
- `Yield.scala` - yield文の実装

**例:**
```ruby
[1, 2, 3].each { |x| puts x }
5.times do |i|
  puts i
end
```

---

### 10. exception/ - 例外処理

Rubyの例外システムの実装。

**主要コンポーネント:**
- `RubyException.scala` - 例外の基底クラス
- `StandardError.scala` - StandardError とそのサブクラス
- `RuntimeError.scala` - RuntimeError
- `ExceptionHandler.scala` - begin/rescue/ensure/raiseの処理

**例:**
```ruby
begin
  raise "Error!"
rescue => e
  puts e.message
ensure
  puts "cleanup"
end
```

---

### 11. stdlib/ - 標準ライブラリ

Rubyの標準ライブラリの実装。

**主要サブパッケージ:**
- `io/` - ファイルI/O関連（File, IO, Dir など）
- `net/` - ネットワーク関連（HTTP, TCPなど）
- `json/` - JSON処理
- その他必要に応じて追加

**実装優先度:**
- Phase 1: File, IO（基本的な入出力）
- Phase 2: Dir, FileTest（ファイルシステム操作）
- Phase 3: その他のライブラリ

---

### 12. util/ - ユーティリティ

共通で使用されるユーティリティクラス。

**主要コンポーネント:**
- `ErrorReporter.scala` - エラーメッセージの整形と出力
- `SourceReader.scala` - ソースコードの読み込み
- `PrettyPrinter.scala` - ASTやオブジェクトの見やすい表示

---

### 13. repl/ - REPL

対話型Ruby実行環境（Interactive Ruby Shell, irb相当）。

**主要コンポーネント:**
- `Repl.scala` - REPL本体
- `Completion.scala` - タブ補完機能
- `History.scala` - コマンド履歴

**機能:**
- 式の対話的評価
- 複数行入力のサポート
- コマンド履歴と補完

---

