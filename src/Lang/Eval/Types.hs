module Lang.Eval.Types (ProgramEnv, Expr(..), Stmt(..), Value(..), EvalM) where
import qualified Data.Map as Map
import Control.Monad.Except (ExceptT)
import Data.Functor.Identity (Identity)

type ProgramEnv = Map.Map String Value

data Expr
  = Var String
  | IntLit Int | BoolLit Bool | CharLit Char | DoubleLit Double | StringLit String | NullLit
  | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr | Mod Expr Expr | Pow Expr Expr | FloorDiv Expr Expr
  | Negate Expr | Brack Expr
  | BinAND Expr Expr | BinOR Expr Expr | BinXOR Expr Expr | BinLShift Expr Expr | BinRShift Expr Expr
  | Eq Expr Expr | Neq Expr Expr | Lte Expr Expr | Lt Expr Expr | Gte Expr Expr | Gt Expr Expr
  | And Expr Expr | Or Expr Expr | Not Expr 
  deriving (Show)

data Stmt
  = Assign String Expr
  | ExprStmt Expr
  deriving (Show)

data Value
  = VInt Int
  | VBool Bool
  | VChar Char
  | VDouble Double
  | VString String
  | VNull
  deriving (Show, Eq)

type EvalM = ExceptT String Identity