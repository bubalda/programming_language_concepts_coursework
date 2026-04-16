module Lang.Parser.Expr (Expr (..), Stmt (..), AssignOperator (..), TwoExprOperator (..), Slice (..), Type (..)) where

-- An element / expression within code
data Expr
  = Var String -- x
  | Let String Expr Expr -- let x = 4 in x * x, in scope
  | IntLit Int -- 16
  | BoolLit Bool -- false
  | CharLit Char -- 'c'
  | FloatLit Float -- Type casted
  | DoubleLit Double -- Dynamic reading defaults all numbers with decimal points to double 1.32
  | StringLit String -- "Hello World"
  | ListLit [Expr] -- [a]
  | NullLit -- null
  | BinOp TwoExprOperator Expr Expr -- a () b operators, where a and b are variables
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
  | ExprStmt Expr -- x
  | AssignOp AssignOperator String Expr -- x += 10
  | Block [Stmt] -- { x += 1; x += 2; }
  | If Expr Stmt (Maybe Stmt) -- if (cond) {r = 2;} else {r = 3}
  | Decl Type String Expr -- double x = 10
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

-- Operator that uses 2 numbers to return a number (numeric type)
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

-- Static Declaration types
data Type
  = TBool
  | TInt
  | TFloat
  | TDouble
  | TChar
  | TString
  deriving (Show)
