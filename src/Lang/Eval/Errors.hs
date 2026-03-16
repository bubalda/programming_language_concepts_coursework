module Lang.Eval.Errors where

import Lang.Eval.Types (EvalM, Value(..))
import Control.Monad.Except (throwError)

-- Error handles
expectVInt :: Value -> EvalM Int
expectVInt (VInt v) = return v
expectVInt v = throwError $ expectVErr (VInt 0) v

expectVBool :: Value -> EvalM Bool
expectVBool (VBool v) = return v
expectVBool v = throwError $ expectVErr (VBool False) v

-- Error formatter
expectVErr :: Value -> Value -> String
expectVErr expectedVal inputVal =
  let text v = case v of
        (VNull) -> "Null"
        (VInt _) -> "Int"
        (VBool _) -> "Bool"
        (VChar _) -> "Char"
        (VDouble _) -> "Double"
        (VString _) -> "String"
   in "Expected object with type `" ++ text expectedVal ++ "`, got type `" ++ text inputVal ++ "` instead."