package hiroki1117.sruby.interpreter

import hiroki1117.sruby.core.{*, given}
import hiroki1117.sruby.ast.*
import cats.effect.IO
import cats.data.StateT
import cats.implicits.*

/**
  * =========================================================
  * Evaluator - AST を評価して RValue を生成
  * =========================================================
  * 
  * Ruby AST を実行し、RValue を返す評価器
  * 
  * 設計:
  *   - Eval[A] = StateT[IO, VMState, A] を使用
  *   - VMState で実行状態（フレーム、グローバル変数等）を管理
  *   - すべての式は値を返す（Ruby の特性）
  */
object Evaluator:
  
  /**
    * AST ノードを評価して RValue を返す
    */
  def eval(expr: Expr): Eval[RValue] = expr match
    // =========================================================
    // リテラル
    // =========================================================
    
    case NilLiteral =>
      StateT.pure(RNil)
    
    case IntLiteral(n, _) =>
      StateT.pure(RInteger(n))
    
    case StringLiteral(s, _) =>
      StateT.pure(RString(s))
    
    case BooleanLiteral(b, _) =>
      StateT.pure(if b then RTrue else RFalse)
    
    case SymbolLiteral(name, _) =>
      // TODO: Symbol pool で管理（同じ名前のシンボルは同一オブジェクト）
      StateT.pure(RString(name)) // 暫定的に String として扱う
    
    // =========================================================
    // Self
    // =========================================================
    
    case Self =>
      getCurrentSelf()
    
    // =========================================================
    // 変数の参照
    // =========================================================
    
    case LocalVarRef(name, pos) =>
      for
        frame <- getCurrentFrame()
        value <- frame.env.get(name) match
          case Some(v) => StateT.pure[IO, VMState, RValue](v)
          case None => raiseError(s"undefined local variable or method `$name'", pos)
      yield value
    
    case InstanceVarRef(name, pos) =>
      for
        self <- getCurrentSelf()
        value = self.getInstanceVariable(name).getOrElse(RNil)
      yield value
    
    case GlobalVarRef(name, pos) =>
      for
        state <- getState()
        // TODO: グローバル変数テーブルを VMState に追加
        value = RNil // 暫定
      yield value
    
    // =========================================================
    // 変数の代入
    // =========================================================
    
    case LocalVarAssign(name, valueExpr, _) =>
      for
        value <- eval(valueExpr)
        _ <- updateEnv(env => env.set(name, value))
      yield value  // Ruby では代入式は値を返す
    
    case InstanceVarAssign(name, valueExpr, _) =>
      for
        value <- eval(valueExpr)
        self <- getCurrentSelf()
        newSelf = self.setInstanceVariable(name, value)
        _ <- updateSelf(newSelf)
      yield value
    
    case GlobalVarAssign(name, valueExpr, _) =>
      for
        value <- eval(valueExpr)
        // TODO: グローバル変数テーブルに設定
      yield value
    
    // =========================================================
    // 定数参照（クラス名等）
    // =========================================================
    
    case ConstantRef(nameParts, pos) =>
      lookupConstant(nameParts, pos)
    
    // =========================================================
    // 単項演算（メソッド呼び出しに変換）
    // =========================================================
    
    case UnaryOp(kind, operand, _) =>
      for
        value <- eval(operand)
        result <- callMethod(value, kind.toMethodName, List.empty)
      yield result
    
    // =========================================================
    // 二項演算（メソッド呼び出しに変換）
    // =========================================================
    
    case BinaryOp(left, kind, right, _) =>
      for
        leftValue <- eval(left)
        rightValue <- eval(right)
        result <- callMethod(leftValue, kind.toMethodName, List(rightValue))
      yield result
    
    // =========================================================
    // メソッド呼び出し
    // =========================================================
    
    case MethodCall(receiverExpr, methodName, argExprs, blockExpr, pos) =>
      for
        receiver <- receiverExpr match
          case Some(expr) => eval(expr)
          case None => getCurrentSelf()
        args <- argExprs.traverse(eval)
        // TODO: ブロック引数の処理
        result <- callMethod(receiver, methodName, args)
      yield result
    
    // =========================================================
    // シーケンス（複数の式を順次実行）
    // =========================================================
    
    case Sequence(exprs, _) =>
      exprs match
        case Nil => StateT.pure(RNil)  // 空のシーケンスは nil
        case _ =>
          // 全ての式を評価し、最後の値を返す
          exprs.traverse(eval).map(_.lastOption.getOrElse(RNil))
    
    // =========================================================
    // If 式
    // =========================================================
    
    case IfExpr(condition, thenBranch, elseBranch, _) =>
      for
        condValue <- eval(condition)
        result <- if condValue.isTruthy then
          eval(thenBranch)
        else
          elseBranch match
            case Some(expr) => eval(expr)
            case None => StateT.pure[IO, VMState, RValue](RNil)
      yield result
    
    // =========================================================
    // While ループ
    // =========================================================
    
    case WhileExpr(condition, body, _) =>
      def loop(): Eval[RValue] =
        for
          condValue <- eval(condition)
          result <- if condValue.isTruthy then
            eval(body) *> loop()  // body を評価してループ継続
          else
            StateT.pure[IO, VMState, RValue](RNil)  // while は nil を返す
        yield result
      loop()
    
    // =========================================================
    // ブロック式
    // =========================================================
    
    case BlockExpr(params, body, kind, _) =>
      // TODO: Proc オブジェクトの生成
      StateT.pure(RNil)  // 暫定
    
    // =========================================================
    // メソッド定義
    // =========================================================
    
    case MethodDef(name, params, body, _) =>
      // TODO: 現在のクラスにメソッドを定義
      StateT.pure(RNil)  // 暫定（本来はシンボルを返す）
    
    // =========================================================
    // クラス定義
    // =========================================================
    
    case ClassDef(nameParts, superclassExpr, body, pos) =>
      // TODO: クラスの生成と登録
      StateT.pure(RNil)  // 暫定
    
    // =========================================================
    // モジュール定義
    // =========================================================
    
    case ModuleDef(nameParts, body, _) =>
      // TODO: モジュールの生成と登録
      StateT.pure(RNil)  // 暫定
  
  // =========================================================
  // ヘルパー関数
  // =========================================================
  
  /**
    * 現在のフレームを取得
    */
  private def getCurrentFrame(): Eval[Frame] =
    StateT.inspect(_.currentFrame)
  
  /**
    * 現在の self を取得
    */
  private def getCurrentSelf(): Eval[RValue] =
    getCurrentFrame().map(_.self)
  
  /**
    * VMState 全体を取得
    */
  private def getState(): Eval[VMState] =
    StateT.get
  
  /**
    * self を更新
    */
  private def updateSelf(newSelf: RValue): Eval[Unit] =
    StateT.modify { state =>
      val oldFrame = state.currentFrame
      val newFrame = Frame(newSelf, oldFrame.env, oldFrame.currentClass)
      state.copy(frames = newFrame :: state.frames.tail)
    }
  
  /**
    * 環境（ローカル変数）を更新
    */
  private def updateEnv(f: Env => Env): Eval[Unit] =
    StateT.modify { state =>
      val oldFrame = state.currentFrame
      val newFrame = oldFrame.copy(env = f(oldFrame.env))
      state.copy(frames = newFrame :: state.frames.tail)
    }
  
  /**
    * 定数を lookup（クラス名等）
    */
  private def lookupConstant(nameParts: List[String], pos: Position): Eval[RValue] =
    // TODO: 定数テーブルから検索
    // 暫定: Builtins から基本クラスを返す
    nameParts match
      case List("Object") => StateT.pure(RObject(Builtins.ClassClass))
      case List("Class") => StateT.pure(RObject(Builtins.ClassClass))
      case List("Integer") => StateT.pure(RObject(Builtins.ClassClass))
      case List("String") => StateT.pure(RObject(Builtins.ClassClass))
      case _ => raiseError(s"uninitialized constant ${nameParts.mkString("::")}", pos)
  
  /**
    * メソッドを呼び出す
    * 
    * Ruby のメソッド探索:
    *   1. receiver.class からメソッドを探索
    *   2. 継承チェーンを辿る
    *   3. method_missing を呼び出す（TODO）
    */
  private def callMethod(receiver: RValue, methodName: String, args: List[RValue]): Eval[RValue] =
    receiver.rubyClass.lookupMethod(methodName) match
      case Some(method) =>
        // メソッドを実行
        method.invoke(receiver, args)
      
      case None =>
        // 組み込みメソッドをチェック
        tryBuiltinMethod(receiver, methodName, args) match
          case Some(result) => result
          case None =>
            raiseError(
              s"undefined method `$methodName' for ${receiver.inspect}:${receiver.rubyClass.name}",
              Position.NoPosition
            )
  
  /**
    * 組み込みメソッドを試行
    * 
    * Phase 1 では基本的な演算のみ実装
    */
  private def tryBuiltinMethod(receiver: RValue, methodName: String, args: List[RValue]): Option[Eval[RValue]] =
    (receiver, methodName, args) match
      // =========================================================
      // Integer の組み込みメソッド
      // =========================================================
      
      case (RInteger(a), "+", List(RInteger(b))) =>
        Some(StateT.pure(RInteger(a + b)))
      
      case (RInteger(a), "-", List(RInteger(b))) =>
        Some(StateT.pure(RInteger(a - b)))
      
      case (RInteger(a), "*", List(RInteger(b))) =>
        Some(StateT.pure(RInteger(a * b)))
      
      case (RInteger(a), "/", List(RInteger(b))) =>
        if b == 0 then
          Some(raiseError("divided by 0", Position.NoPosition))
        else
          Some(StateT.pure(RInteger(a / b)))
      
      case (RInteger(a), "%", List(RInteger(b))) =>
        if b == 0 then
          Some(raiseError("divided by 0", Position.NoPosition))
        else
          Some(StateT.pure(RInteger(a % b)))
      
      case (RInteger(a), "==", List(RInteger(b))) =>
        Some(StateT.pure(if a == b then RTrue else RFalse))
      
      case (RInteger(a), "!=", List(RInteger(b))) =>
        Some(StateT.pure(if a != b then RTrue else RFalse))
      
      case (RInteger(a), "<", List(RInteger(b))) =>
        Some(StateT.pure(if a < b then RTrue else RFalse))
      
      case (RInteger(a), "<=", List(RInteger(b))) =>
        Some(StateT.pure(if a <= b then RTrue else RFalse))
      
      case (RInteger(a), ">", List(RInteger(b))) =>
        Some(StateT.pure(if a > b then RTrue else RFalse))
      
      case (RInteger(a), ">=", List(RInteger(b))) =>
        Some(StateT.pure(if a >= b then RTrue else RFalse))
      
      case (RInteger(n), "-@", Nil) =>
        Some(StateT.pure(RInteger(-n)))
      
      case (RInteger(n), "+@", Nil) =>
        Some(StateT.pure(RInteger(n)))
      
      // =========================================================
      // String の組み込みメソッド
      // =========================================================
      
      case (RString(a), "+", List(RString(b))) =>
        Some(StateT.pure(RString(a + b)))
      
      case (RString(a), "==", List(RString(b))) =>
        Some(StateT.pure(if a == b then RTrue else RFalse))
      
      case (RString(s), "upcase", Nil) =>
        Some(StateT.pure(RString(s.toUpperCase)))
      
      case (RString(s), "downcase", Nil) =>
        Some(StateT.pure(RString(s.toLowerCase)))
      
      case (RString(s), "length", Nil) =>
        Some(StateT.pure(RInteger(s.length.toLong)))
      
      case (RString(s), "reverse", Nil) =>
        Some(StateT.pure(RString(s.reverse)))
      
      // =========================================================
      // Boolean の組み込みメソッド
      // =========================================================
      
      case (RTrue, "!", Nil) =>
        Some(StateT.pure(RFalse))
      
      case (RFalse, "!", Nil) =>
        Some(StateT.pure(RTrue))
      
      // =========================================================
      // すべてのオブジェクト共通
      // =========================================================
      
      case (obj, "class", Nil) =>
        Some(StateT.pure(RObject(Builtins.ClassClass)))  // 暫定
      
      case (obj, "to_s", Nil) =>
        Some(StateT.pure(RString(obj.toRubyString)))
      
      case (obj, "inspect", Nil) =>
        Some(StateT.pure(RString(obj.inspect)))
      
      case _ =>
        None
  
  /**
    * エラーを発生させる
    */
  private def raiseError(message: String, pos: Position): Eval[RValue] =
    StateT.liftF(IO.raiseError(new RuntimeException(s"$pos: $message")))

/**
  * Evaluator のヘルパー
  */
object EvaluatorHelper:
  /**
    * トップレベルで式を評価
    */
  def evalTopLevel(expr: Expr): IO[RValue] =
    val initialState = VMState(
      frames = List(
        Frame(
          self = RObject(Builtins.ObjectClass),
          env = Env(),
          currentClass = Builtins.ObjectClass
        )
      )
    )
    
    Evaluator.eval(expr).runA(initialState)

