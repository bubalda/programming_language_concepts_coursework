  module Lang.Parser.Expr (Expr (..), Stmt (..), AssignOperator(..), TwoExprOperator(..)) where

  -- An element / expression within code
  data Expr
    = Var String
    | IntLit Int
    | BoolLit Bool
    | CharLit Char
    | FloatLit Float
    | StringLit String
    | NullLit
    | BinOp TwoExprOperator Expr Expr
    | Negate Expr
    | Not Expr
    | SqBrack Expr
    deriving (Show)

  -- A line of code
  data Stmt
    = Assign String Expr
    | Call String [Expr]
    | ExprStmt Expr
    | AssignOp AssignOperator String Expr
    | If Expr [Stmt] (Maybe [Stmt])
    | For Stmt Expr Stmt [Stmt]
    | While Expr [Stmt]
    deriving (Show)

  -- About someOp ++ '='
  data AssignOperator
    = AddEq
    | SubEq
    | MulEq
    | DivEq
    | ModEq
    | BinAndEq
    | BinOrEq
    | BinXorEq
    | BinLShiftEq
    | BinRShiftEq
    deriving (Show)

  -- Operator that uses 2 numbers to return a number
  data TwoExprOperator
    = Add
    | Sub
    | Mul
    | Div
    | Mod
    | BinAnd
    | BinOr
    | BinXor
    | BinLShift
    | BinRShift
    | Eq
    | Neq
    | Lte
    | Lt
    | Gte
    | Gt
    | And
    | Or
    deriving (Show)