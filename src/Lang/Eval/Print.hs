module Lang.Eval.Print
  ( printEval,
    printEvalPretty,
  )
where

import Lang.Eval.Types (Value(..))
import Lang.Repl.Helper (wrapSection)

printEval :: Value -> IO ()
printEval (VBool b) = putStrLn (show b)
printEval (VInt i) = putStrLn (show i)
printEval (VChar v) = putStrLn (show v)
printEval (VDouble d) = putStrLn (show d)
printEval (VString s) = putStrLn s
printEval (VNull) = putStrLn "null"

printEvalPretty :: Value -> IO ()
printEvalPretty val = wrapSection "Evaluation Result" (printEval val)
