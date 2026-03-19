module Lang.Parser.Expr (Expr (..), Stmt (..), AssignOperator(..), TwoExprOperator(..), Slice(..)) where

-- An element / expression within code
data Expr
  = Var String
  | Let String Expr Expr
  | IntLit Int
  | BoolLit Bool
  | CharLit Char
  | FloatLit Float
  | StringLit String
  | ListLit [Expr]
  | NullLit
  | BinOp TwoExprOperator Expr Expr
  | Negate Expr
  | Not Expr
  | Call Expr [Expr] -- Call "sin" [params]
  | ListIndex Expr Expr
  | ListSlice Expr Slice
  | ListTexasRange Expr Expr
  deriving (Show)

-- A line of code
data Stmt
  = Assign String Expr
  | ExprStmt Expr
  | AssignOp AssignOperator String Expr
  | If Expr Stmt (Maybe Stmt)
  deriving (Show)

-- About someOp ++ '='
data AssignOperator
  = AddEq
  | SubEq
  | MulEq
  | DivEq
  | ModEq
  | BitAndEq
  | BitOrEq
  | BitXorEq
  | BitLShiftEq
  | BitRShiftEq
  deriving (Show)

-- Operator that uses 2 numbers to return a number
data TwoExprOperator
  = Add
  | Sub
  | Mul
  | Div
  | Mod
  | BitAnd
  | BitOr
  | BitXor
  | BitLShift
  | BitRShift
  | Eq
  | Neq
  | Lte
  | Lt
  | Gte
  | Gt
  | And
  | Or
  deriving (Show)

data Slice = Slice (Maybe Expr) (Maybe Expr) (Maybe Expr)
  deriving (Show)