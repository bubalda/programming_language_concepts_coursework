module Lang.Eval.Types (ProgramEnv, Value(..), EvalM) where
import qualified Data.Map as Map
import Control.Monad.Except (ExceptT)
import Data.Functor.Identity (Identity)

type ProgramEnv = Map.Map String Value

data Value
  = VInt Int
  | VBool Bool
  | VChar Char
  | VDouble Double
  | VString String
  | VNull
  deriving (Show, Eq)

type EvalM = ExceptT String Identity