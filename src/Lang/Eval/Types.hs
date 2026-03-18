module Lang.Eval.Types (ProgramEnv, Value(..), EvalM) where
import qualified Data.Map as Map
import Control.Monad.Except (ExceptT)
import Data.Functor.Identity (Identity)

type ProgramEnv = Map.Map String Value

data Value
  = VNull
  | VInt Int
  | VBool Bool
  | VFloat Float
  | VChar Char
  | VString String
  deriving (Show, Eq)

type EvalM = ExceptT String Identity