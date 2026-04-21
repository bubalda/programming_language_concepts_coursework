import Test.Hspec
import qualified Data.Map as Map

import Lang.Eval.Eval (evalExpr, runEval)
import Lang.Syntax.Syntax (Expr(..), TwoExprOperator(..))
import Lang.Eval.Types (Value(..))

-- Helper runner
runTest :: Expr -> Either String Value
runTest expr = runEval (evalExpr Map.empty expr)

-- Helper check
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

main :: IO ()
main = putStrLn "Test suite not yet implemented"
