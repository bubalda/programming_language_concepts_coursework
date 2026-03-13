module Lang.Parser.Eval (ProgramEnv, Expr (..), Stmt (..), evalExpr, evalStmt) where

import qualified Data.Map as Map

type ProgramEnv = Map.Map String Int

data Expr
  = Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Var String
  | IntLit Int
  deriving (Show)

data Stmt
  = Assign String Expr
  | ExprStmt Expr
  deriving (Show)

evalExpr :: ProgramEnv -> Expr -> Int
evalExpr env expr =
  case expr of
    IntLit n -> n
    Var v ->
      case Map.lookup v env of
        Just val -> val
        Nothing -> error ("Undefined variable: " ++ v)
    Add a b -> evalExpr env a + evalExpr env b
    Sub a b -> evalExpr env a - evalExpr env b
    Mul a b -> evalExpr env a * evalExpr env b
    Div a b -> evalExpr env a `div` evalExpr env b

evalStmt :: ProgramEnv -> Stmt -> (ProgramEnv, Int)
evalStmt env stmt =
  case stmt of
    ExprStmt e ->
      (env, evalExpr env e)
    Assign name expr ->
      let val = evalExpr env expr
          env' = Map.insert name val env
       in (env', val)