module Lang.Eval.Errors
  ( expectVInt,
    expectVBool,
    expectVFloat,
    expectVNumeric,
    expectVList,
    expectVErr
  )
where

import Lang.Eval.Types (EvalM, Value(..))
import Control.Monad.Except (throwError)

-- Error handlers for typechecker
expectVInt :: Value -> EvalM Int
expectVInt (VInt v) = return v
expectVInt v = throwError $ expectVErr (VInt 0) v

expectVBool :: Value -> EvalM Bool
expectVBool (VBool v) = return v
expectVBool v = throwError $ expectVErr (VBool False) v

expectVFloat :: Value -> EvalM Float
expectVFloat (VFloat v) = return v
expectVFloat v = throwError $ expectVErr (VFloat 0) v

-- Support Int/Float inputs uniformly
expectVNumeric :: Value -> EvalM Float
expectVNumeric (VInt v) = return (fromIntegral v)
expectVNumeric (VFloat v) = return v
expectVNumeric v = throwError $ "Expected numeric value, got `" ++ typeName v ++ "` instead."

expectVList :: Value -> EvalM [Value]
expectVList (VList xs) = return xs
expectVList v = throwError $ expectVErr (VList []) v

-- Error formatter
expectVErr :: Value -> Value -> String
expectVErr expectedVal inputVal =
  let text v = typeName v
   in "Expected object with type `" ++ text expectedVal ++ "`, got type `" ++ text inputVal ++ "` instead."

-- For converting value types to texts
typeName :: Value -> String
typeName v = case v of
  VNull -> "Null"
  VInt _ -> "Int"
  VBool _ -> "Bool"
  VChar _ -> "Char"
  VFloat _ -> "Float"
  VString _ -> "String"
  VList _ -> "List"
