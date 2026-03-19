module Lang.Parser.Expr (Expr (..), Stmt (..), AssignOperator(..), TwoExprOperator(..), Slice(..)) where

-- An element / expression within code
data Expr
  = Var String -- x
  | Let String Expr Expr -- let x = 4 in x * x, in scope
  | IntLit Int -- 16
  | BoolLit Bool -- false
  | CharLit Char -- 'c'
  | FloatLit Float -- "1.223"
  | StringLit String -- "Hello World"
  | ListLit [Expr] -- [a]
  | NullLit -- null
  | BinOp TwoExprOperator Expr Expr
  | Negate Expr -- -x
  | Not Expr -- !true
  | Call Expr [Expr] -- Call "function_name" [params]
  | ListIndex Expr Expr -- list[i]
  | ListSlice Expr Slice -- list[start:stop:step]
  | ListRange Expr Expr -- [1..100]
  deriving (Show)

-- A line of code
data Stmt
  = Assign String Expr -- x = 10
  | ExprStmt Expr
  | AssignOp AssignOperator String Expr
  | If Expr Stmt (Maybe Stmt) -- if (cond) then r = 2; else r = 3
  deriving (Show)

-- About someOp ++ '=', like +=, -= etc.
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

-- Operator that uses 2 numbers to return a number (int / float)
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

-- x[1:7:2], like Python
data Slice = Slice (Maybe Expr) (Maybe Expr) (Maybe Expr)
  deriving (Show)