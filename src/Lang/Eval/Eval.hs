module Lang.Eval.Eval (evalExpr, evalStmt, runEval) where

import Control.Monad.Except (runExceptT, throwError)
import Data.Bits (Bits (shiftL, shiftR, xor), (.&.), (.|.))
import Data.Functor.Identity (Identity (runIdentity))
import qualified Data.Map as Map
import Lang.Eval.Types (EvalM, ProgramEnv, Value(..))
import Lang.Parser.Expr (Expr(..), Stmt(..))
import Lang.Eval.Errors (expectVInt, expectVBool)

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
    AssignWithType vType name expr -> throwError "ERROR: `Type assignment` method not implemented."
    If cond ifBlock elseBlock -> throwError "ERROR: `If` method not implemented."
    For start step stop forBlock -> throwError "ERROR: `For` method not implemented."
    While cond whileBlock -> throwError "ERROR: `While` method not implemented."

evalExpr :: ProgramEnv -> Expr -> EvalM Value
evalExpr env expr =
  case expr of
    IntLit n -> return $ VInt n
    CharLit n -> return $ VChar n
    BoolLit n -> return $ VBool n
    DoubleLit n -> return $ VDouble n
    StringLit n -> return $ VString n
    NullLit -> return VNull
    Var v ->
      case Map.lookup v env of
        Just val -> return val
        Nothing -> throwError ("Undefined identifier: " ++ v)

    Brack a -> eval a
    SqBrack a -> eval a
    CBrack a -> eval a

    Add a b -> arithOpInt (+) a b
    Sub a b -> arithOpInt (-) a b
    Mul a b -> arithOpInt (*) a b
    Div a b -> arithOpInt div a b
    Mod a b -> arithOpInt mod a b
    Pow a b -> arithOpInt (^) a b
    FloorDiv a b -> arithOpInt div a b

    Negate a -> do
      x <- eInt a
      return $ VInt (-x)

    Eq a b -> do
      va <- eval a
      vb <- eval b
      return $ VBool (va == vb)

    Neq a b -> do
      va <- eval a
      vb <- eval b
      return $ VBool (va /= vb)

    Lte a b -> cmpOpInt (<=) a b
    Lt a b -> cmpOpInt (<) a b
    Gte a b -> cmpOpInt (>=) a b
    Gt a b -> cmpOpInt (>) a b

    BinAnd a b -> bitOp (.&.) a b
    BinOr a b -> bitOp (.|.) a b
    BinXor a b -> bitOp xor a b
    BinLShift a b -> bitOp shiftL a b
    BinRShift a b -> bitOp shiftR a b

    -- Lazy evaluation of and / or
    And a b -> do
      va <- eBool a
      case va of
        False -> return $ VBool False
        True -> do
          vb <- eBool b
          case vb of
            vb -> return $ VBool vb

    Or a b -> do
      va <- eBool a
      case va of
        True -> return $ VBool True
        False -> do
          vb <- eBool b
          case vb of
            vb -> return $ VBool vb

    Not a -> do
      va <- eBool a
      return $ VBool (not (va))

  where
    -- Shortcut Helpers
    arithOpInt f a b = do
      x <- eInt a
      y <- eInt b
      return $ VInt (f x y)

    cmpOpInt f a b = do
      x <- eInt a
      y <- eInt b
      return $ VBool (f x y)

    bitOp f a b = do
      x <- eInt a
      y <- eInt b
      return $ VInt (f x y)

    eval = evalExpr env
    eInt e = eval e >>= expectVInt
    eBool e = eval e >>= expectVBool