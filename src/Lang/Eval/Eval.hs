module Lang.Eval.Eval (evalExpr, evalStmt, runEval) where

import Control.Monad.Except (runExceptT, throwError)
import Data.Functor.Identity (Identity (runIdentity))
import qualified Data.Map as Map
import Lang.Eval.Errors (expectVBool, expectVInt)
import Lang.Eval.Op (applyAssignOp, calcBinOp)
import Lang.Eval.Types (EvalM, ProgramEnv, Value (..))
import Lang.Parser.Expr (Expr (..), Stmt (..), TwoExprOperator (..))

-- Run evaluator
runEval :: EvalM a -> Either String a
runEval ev = runIdentity (runExceptT ev)

evalStmt :: ProgramEnv -> Stmt -> EvalM (ProgramEnv, Value)
evalStmt env stmt =
  case stmt of
    ExprStmt e -> do
      val <- evalExpr env e
      return (env, val)
    Assign name expr -> do
      val <- evalExpr env expr
      let env' = Map.insert name val env
      return (env', val)
    AssignOp op name expr -> do
      val <- evalExpr env expr
      old <- case Map.lookup name env of
        Just v -> return v
        Nothing -> throwError ("Undefined identifier: " ++ name)

      -- Apply calculation and assignment
      result <- applyAssignOp op old val
      let env' = Map.insert name result env in return (env', result)

    -- Not implemented error
    s -> throwError $ "ERROR: Function `" ++ show s ++ "` is not defined"

-- Evaluation of expressions
evalExpr :: ProgramEnv -> Expr -> EvalM Value
evalExpr env expr =
  case expr of
    NullLit -> return VNull
    BoolLit n -> return $ VBool n
    IntLit n -> return $ VInt n
    FloatLit n -> return $ VFloat n
    CharLit n -> return $ VChar n
    StringLit n -> return $ VString n
    Var v ->
      case Map.lookup v env of
        Just val -> return val
        Nothing -> throwError ("Undefined identifier: " ++ v)
    BinOp And a b -> logicOpLazy False a b
    BinOp Or a b -> logicOpLazy True a b
    BinOp op a b -> do
      va <- eval a
      vb <- eval b
      calcBinOp op va vb
    Negate a -> do
      x <- eInt a
      return $ VInt (-x)
    Not a -> do
      va <- eBool a
      return $ VBool (not (va))
    s -> throwError $ "Function `" ++ show s ++ "` is not defined"
  where
    -- Evaluation
    eval = evalExpr env
    eInt e = eval e >>= expectVInt
    eBool e = eval e >>= expectVBool

    -- Lazy evaluation of and | or
    -- Example: true || 'hello' => true (Since LHS is true, 'hello' is not evaluated)
    logicOpLazy retEarly a b = do
      va <- eBool a
      if va == retEarly
        then return (VBool retEarly)
        else eval b
