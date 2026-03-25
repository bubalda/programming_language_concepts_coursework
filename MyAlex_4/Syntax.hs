module Syntax where

data Op = Plus | Minus | Times | Div | Less | LessEq | Greater | GreaterEq | Eq | NotEq
 deriving (Show, Eq)

data Expr = IntLit Int
          | BoolLit Bool
          | Var String
          | Binop Op Expr Expr
          | Let String Expr Expr
          | If Expr Expr Expr
          | Invalid
 deriving (Show, Eq)


-- Types used by the type checker.
data Type = TInt | TBool | TError
 deriving (Show, Eq)

-- Environments used during type checking and evaluation.
type Env = [(String, Type)]
type ValEnv = [(String, Value)]

-- Runtime values produced by evaluation.
data Value = VInt Int
           | VBool Bool
           | VError String
 deriving (Show, Read, Eq)

data Program = Program Expr
    deriving (Show, Eq)
