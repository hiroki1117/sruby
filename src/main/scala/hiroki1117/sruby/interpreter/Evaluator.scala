package hiroki1117.sruby.interpreter

import hiroki1117.sruby.core.*
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
  * 
  * Phase 1 改善（Ruby 仕様準拠）:
  *   ✅ updateSelf/updateEnv のフレームスタック破壊バグ修正
  *   ✅ lookupConstant を Ruby 仕様に準拠（クラスオブジェクトを返す）
  *   ✅ method_missing へのフォールバック追加
  *   ✅ コメントの改善と TODO の明確化
  */
object Evaluator:
  
  // RUserMethod に評価関数を注入（循環参照を避けるため）
  RUserMethod.evalBody = eval
  
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
      // Ruby の Symbol は不変・高速比較・interned
      // RSymbol.intern により同名シンボルは同一オブジェクト
      StateT.pure(RSymbol.intern(name))
    
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
        // TODO Phase 2: ブロック引数を Proc オブジェクトに変換
        // blockOpt <- blockExpr.traverse(makeProc)
        // result <- callMethod(receiver, methodName, args, blockOpt)
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
      // TODO Phase 2: Proc オブジェクトの生成
      // - 現在の env と self をキャプチャ
      // - RProc(params, body, capturedEnv, capturedSelf) を生成
      StateT.pure(RNil)  // 暫定
    
    // =========================================================
    // メソッド定義
    // =========================================================
    
    case MethodDef(name, params, body, pos) =>
      for
        frame <- getCurrentFrame()
        
        // ユーザー定義メソッドを作成
        method = RUserMethod(
          name = name,
          params = params,
          body = body,
          definingClass = frame.currentClass
        )
        
        // 現在のクラスにメソッドを定義
        _ <- defineMethod(frame.currentClass, method)
        
        // Ruby の仕様: def は定義したメソッド名のシンボルを返す
      yield RSymbol.intern(name)
    
    // =========================================================
    // クラス定義
    // =========================================================
    
    case ClassDef(nameParts, superclassExpr, body, pos) =>
      // 1. クラス名の解決（Phase 2: シンプルな名前のみサポート）
      val className = nameParts match
        case List(name) => name
        case _ => 
          // ネストしたクラス名は Phase 3 で実装
          return raiseError(s"nested class names not yet supported: ${nameParts.mkString("::")}", pos)
      
      for
        // 2. スーパークラスの評価
        parentClass <- (superclassExpr match
          case Some(expr) =>
            eval(expr).flatMap {
              case k: RClass => StateT.pure[IO, VMState, Option[RClass]](Some(k))
              case other => raiseError(s"superclass must be a Class, got ${other.inspect}", pos).map(_ => None)
            }
          
          case None =>
            // スーパークラスが指定されていない場合は Object
            StateT.pure[IO, VMState, Option[RClass]](Some(Builtins.ObjectClass))
        )
        
        // 3. クラスの作成 or 取得（再オープン）
        state <- getState()
        klass <- (state.getClass(className) match
          case Some(existingClass) =>
            // 再オープンの場合
            // スーパークラスの整合性チェック
            val existingParentName = existingClass.superclass.map(_.name).getOrElse("none")
            val newParentName = parentClass.map(_.name).getOrElse("none")
            
            if existingClass.superclass != parentClass then
              raiseError(
                s"superclass mismatch for class $className " +
                s"(expected $existingParentName, got $newParentName)",
                pos
              )
            else
              StateT.pure[IO, VMState, RClass](existingClass)
          
          case None =>
            // 新規クラスの作成
            val newClass = RClass(
              name = className,
              superclass = parentClass,
              methods = Map.empty
            )
            // すぐに登録（クラス本体内で self として参照できるようにするため）
            StateT.modify[IO, VMState](_.updateClass(newClass)) *>
              StateT.pure[IO, VMState, RClass](newClass)
        ): Eval[RClass]
        
        // 4. クラス本体を評価するための新しいフレーム
        classFrame = Frame(
          self = klass,              // self = クラスオブジェクト
          env = Env(),               // クラス定義内のローカル変数は空
          currentClass = klass       // 現在のクラス = 定義中のクラス
        )
        
        // 5. フレームを push → body 評価 → pop
        _ <- pushFrame(classFrame)
        _ <- eval(body)
        _ <- popFrame()
        
        // 6. 評価後の最新クラス定義を取得
        finalState <- getState()
        finalClass = finalState.getClass(className).getOrElse(klass)
        
      yield finalClass  // クラスオブジェクトを返す（Phase 2 で実装）
    
    // =========================================================
    // モジュール定義
    // =========================================================
    
    case ModuleDef(nameParts, body, _) =>
      // TODO Phase 3: モジュールの生成と登録
      StateT.pure(RNil)  // 暫定（Phase 3 で実装）
  
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
    * フレームを push（メソッド呼び出し時）
    */
  private def pushFrame(frame: Frame): Eval[Unit] =
    StateT.modify { state =>
      state.pushFrame(frame)
    }
  
  /**
    * フレームを pop（メソッド呼び出し終了時）
    */
  private def popFrame(): Eval[Unit] =
    StateT.modify { state =>
      state.popFrame()._2  // (Frame, VMState) のタプルから VMState を取得
    }
  
  /**
    * クラスにメソッドを定義
    * 
    * 注意: Ruby では同じクラスに複数回メソッドを定義できる（上書き）
    */
  private def defineMethod(klass: RClass, method: RMethod): Eval[Unit] =
    StateT.modify { state =>
      // VMState.classes から最新のクラス定義を取得
      // （複数メソッド定義時に、前のメソッドが反映されたクラスを取得するため）
      val currentClass = state.getClass(klass.name).getOrElse(klass)
      
      // メソッドを追加
      val updatedClass = currentClass.copy(
        methods = currentClass.methods.updated(method.name, method)
      )
      
      // VMState のクラステーブルを更新
      state.updateClass(updatedClass)
    }
  
  /**
    * self を更新
    * 
    * 注意: Ruby のフレームスタックは LIFO
    * 現在のフレーム（head）のみを更新し、残りのスタックは保持
    */
  private def updateSelf(newSelf: RValue): Eval[Unit] =
    StateT.modify { state =>
      state.frames match
        case Nil => 
          // フレームが空の場合（通常はありえない）
          state
        case head :: tail =>
          val newFrame = head.copy(self = newSelf)
          state.copy(frames = newFrame :: tail)
    }
  
  /**
    * 環境（ローカル変数）を更新
    * 
    * 注意: Ruby のフレームスタックは LIFO
    * 現在のフレーム（head）のみを更新し、残りのスタックは保持
    */
  private def updateEnv(f: Env => Env): Eval[Unit] =
    StateT.modify { state =>
      state.frames match
        case Nil =>
          // フレームが空の場合（通常はありえない）
          state
        case head :: tail =>
          val newFrame = head.copy(env = f(head.env))
          state.copy(frames = newFrame :: tail)
    }
  
  /**
    * 定数を lookup（クラス名等）
    * 
    * Ruby の定数解決順序:
    *   1. 現在のクラス/モジュールの nesting
    *   2. 現在のクラスの superclass chain
    *   3. トップレベル Object の定数
    *   4. ::CONST の場合はルートから lookup
    * 
    * 現在は Phase 1 なので、Builtins の基本クラスのみ対応
    */
  private def lookupConstant(nameParts: List[String], pos: Position): Eval[RValue] =
    // シンプルな定数名のみサポート（Phase 2）
    nameParts match
      case List(name) =>
        for
          state <- getState()
          klass <- state.getClass(name) match
            case Some(k) => StateT.pure[IO, VMState, RValue](k)
            case None => raiseError(s"uninitialized constant $name", pos)
        yield klass
      
      case _ =>
        // ネストした定数は Phase 3 で実装
        raiseError(s"nested constants not yet supported: ${nameParts.mkString("::")}", pos)
  
  /**
    * メソッドを呼び出す
    * 
    * Ruby のメソッド探索:
    *   1. receiver.class からメソッドを探索（VMState.classes から最新定義を取得）
    *   2. 継承チェーンを辿る（RClass.lookupMethod が実施）
    *   3. 組み込みメソッドをチェック
    *   4. method_missing を呼び出す
    *   5. それでもダメなら NoMethodError
    */
  private def callMethod(receiver: RValue, methodName: String, args: List[RValue]): Eval[RValue] =
    for
      state <- getState()
      
      // VMState.classes から最新のクラス定義を取得
      // （MethodDef で更新されたクラスを反映するため）
      receiverClass = state.getClass(receiver.rubyClass.name)
        .getOrElse(receiver.rubyClass)
      
      result <- receiverClass.lookupMethod(methodName) match
        case Some(method) =>
          // メソッドを実行
          method.invoke(receiver, args)
        
        case None =>
          // 組み込みメソッドをチェック
          tryBuiltinMethod(receiver, methodName, args) match
            case Some(result) => 
              result
            
            case None =>
              // method_missing にフォールバック
              receiverClass.lookupMethod("method_missing") match
                case Some(missingMethod) =>
                  // Ruby の method_missing は第1引数にシンボルを受け取る
                  val methodNameSymbol = RSymbol.intern(methodName)
                  missingMethod.invoke(receiver, methodNameSymbol :: args)
                
                case None =>
                  // method_missing も見つからない場合はエラー
                  raiseError(
                    s"undefined method `$methodName' for ${receiver.inspect}:${receiverClass.name}",
                    Position.NoPosition
                  )
    yield result
  
  /**
    * 組み込みメソッドを試行
    * 
    * Phase 1: パターンマッチで直接実装（現在）
    * Phase 2: RClass のメソッドテーブルに登録して dispatch
    * 
    * 注意: この関数は Phase 1 の暫定実装
    * 将来的には Builtins で各クラスにメソッドを defineMethod する形に移行
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
        // Ruby の obj.class は receiver のクラス（RClass）を返す
        Some(StateT.pure(obj.rubyClass))
      
      case (obj, "to_s", Nil) =>
        Some(StateT.pure(RString(obj.toRubyString)))
      
      case (obj, "inspect", Nil) =>
        Some(StateT.pure(RString(obj.inspect)))
      
      case _ =>
        None
  
  /**
    * エラーを発生させる
    */
  private def raiseError[A](message: String, pos: Position): Eval[A] =
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

