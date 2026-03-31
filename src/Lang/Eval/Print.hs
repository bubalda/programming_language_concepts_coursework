module Lang.Eval.Print
  ( printEval,
    printEvalPretty,
    renderEval,
  )
where

import Lang.Eval.Types (Value(..))
import Lang.Repl.Helper (putSuccessLn, wrapSection)

renderEval :: Value -> String
renderEval (VBool b) = show b
renderEval (VInt i) = show i
renderEval (VChar v) = show v
renderEval (VFloat f) = show f
renderEval (VString s) = s
renderEval (VList xs) = show xs
renderEval VNull = "null"

-- For REPL
printEval :: Value -> IO ()
printEval = putSuccessLn . renderEval

printEvalPretty :: Value -> Int -> IO ()
printEvalPretty val line = wrapSection ("Evaluation Result (Line " ++ show line ++ ")") (printEval val)
