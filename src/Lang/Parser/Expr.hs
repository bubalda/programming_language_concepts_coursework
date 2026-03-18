module Lang.Parser.Expr (Expr (..), Stmt (..)) where

-- An element / expression within code
data Expr
  = Var String
  | IntLit Int
  | BoolLit Bool
  | CharLit Char
  | FloatLit Float
  | StringLit String
  | NullLit
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Mod Expr Expr
  | Negate Expr
  | Brack Expr
  | SqBrack Expr
  | CBrack Expr
  | BinAnd Expr Expr
  | BinOr Expr Expr
  | BinXor Expr Expr
  | BinLShift Expr Expr
  | BinRShift Expr Expr
  | Eq Expr Expr
  | Neq Expr Expr
  | Lte Expr Expr
  | Lt Expr Expr
  | Gte Expr Expr
  | Gt Expr Expr
  | And Expr Expr
  | Or Expr Expr
  | Not Expr
  | Sinh Expr
  | Cosh Expr
  | Tanh Expr
  | Csch Expr
  | Sech Expr
  | Coth Expr
  | Asinh Expr
  | Acosh Expr
  | Mean Expr
  | Median Expr
  | Mode Expr
  | Sum Expr
  | Product Expr
  | Min Expr
  | Max Expr
  | Stddev Expr
  | Sqrt Expr
  | Cbrt Expr
  | Pow Expr Expr
  | Exp Expr
  | Square Expr
  | Cube Expr
  | Exp10 Expr
  | Sin Expr
  | Cos Expr
  | Tan Expr
  | Asin Expr
  | Acos Expr
  | Atan Expr
  | Atan2 Expr Expr
  | Sec Expr
  | Csc Expr
  | Cot Expr
  | Versin Expr
  | Exsec Expr
  | Ln Expr
  | Log10 Expr
  | Log2 Expr
  | Log Expr
  | Log1p Expr
  | Fact Expr
  | Fact2 Expr
  | Comb Expr Expr
  | Perm Expr Expr
  | Gcd Expr
  | Lcm Expr
  | Fib Expr
  | Gamma Expr
  deriving (Show)

-- A line of code
data Stmt
  = AssignWithType String String Expr
  | Assign String Expr
  | ExprStmt Expr
  | If Expr [Stmt] (Maybe [Stmt])
  | For Stmt Expr Stmt [Stmt]
  | While Expr [Stmt]
  deriving (Show)
