module Lang.Eval.Print
  ( printEval,
    printEvalPretty,
  )
where

import Lang.Eval.Types (Value(..))
import Lang.Repl.Helper (wrapSection)

-- For REPL
printEval :: Value -> IO ()
printEval (VBool b) = putStrLn (show b)
printEval (VInt i) = putStrLn (show i)
printEval (VChar v) = putStrLn (show v)
printEval (VFloat f) = putStrLn (show f)
printEval (VString s) = putStrLn s
printEval (VNull) = putStrLn "null"

printEvalPretty :: Value -> Int -> IO ()
printEvalPretty val line = wrapSection ("Evaluation Result (Line " ++ show line ++ ")") (printEval val)
