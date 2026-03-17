module Lang.Parser.Expr (Expr(..), Stmt(..)) where

data Expr
  = Var String
  | IntLit Int | BoolLit Bool | CharLit Char | DoubleLit Double | StringLit String | NullLit
  | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr | Mod Expr Expr | Pow Expr Expr | FloorDiv Expr Expr
  | Negate Expr 
  | Brack Expr | SqBrack Expr | CBrack Expr
  | BinAnd Expr Expr | BinOr Expr Expr | BinXor Expr Expr | BinLShift Expr Expr | BinRShift Expr Expr
  | Eq Expr Expr | Neq Expr Expr | Lte Expr Expr | Lt Expr Expr | Gte Expr Expr | Gt Expr Expr
  | And Expr Expr | Or Expr Expr | Not Expr 
  deriving (Show)

data Stmt 
  = AssignWithType String String Expr
  | Assign String Expr
  | ExprStmt Expr
  | If Expr [Stmt] (Maybe [Stmt])
  | For Stmt Expr Stmt [Stmt]
  | While Expr [Stmt]
  deriving (Show)