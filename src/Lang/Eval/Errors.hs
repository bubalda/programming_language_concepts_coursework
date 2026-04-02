module Lang.Eval.Errors
  ( expectVInt,
    expectVIntIn,
    expectVBool,
    expectVBoolIn,
    expectVFloat,
    expectVDouble,
    expectVNumeric,
    expectVNumericIn,
    expectVList,
    expectVErr,
    typeName,
    formatTypedValue
  )
where

import Control.Monad.Except (throwError)
import Data.List (intercalate)
import Lang.Eval.Types (EvalM, Value (..))

-- Error handlers for typechecker
expectVInt :: Value -> EvalM Int
expectVInt (VInt v) = return v
expectVInt v = throwError $ expectVErr (VInt 0) v

expectVIntIn :: String -> Value -> EvalM Int
expectVIntIn _ (VInt v) = return v
expectVIntIn context v = throwError $ expectSpecificType context "integer" v

expectVBool :: Value -> EvalM Bool
expectVBool (VBool v) = return v
expectVBool v = throwError $ expectVErr (VBool False) v

expectVBoolIn :: String -> Value -> EvalM Bool
expectVBoolIn _ (VBool v) = return v
expectVBoolIn context v = throwError $ expectSpecificType context "boolean" v

expectVFloat :: Value -> EvalM Float
expectVFloat (VFloat v) = return v
expectVFloat (VDouble v) = return (realToFrac v)
expectVFloat (VInt v) = return (fromIntegral v)
expectVFloat v = throwError $ expectVErr (VFloat 0) v

expectVDouble :: Value -> EvalM Double
expectVDouble (VDouble v) = return v
expectVDouble (VFloat v) = return (realToFrac v)
expectVDouble (VInt v) = return (fromIntegral v)
expectVDouble v = throwError $ expectVErr (VDouble 0) v

-- Support Int/Float/Double inputs uniformly
expectVNumeric :: Value -> EvalM Double
expectVNumeric (VInt v) = return (fromIntegral v)
expectVNumeric (VFloat v) = return (realToFrac v)
expectVNumeric (VDouble v) = return v
expectVNumeric v = throwError $ "Type error: expected a numeric value, but got " ++ formatTypedValue v ++ "."

expectVNumericIn :: String -> Value -> EvalM Double
expectVNumericIn _ (VInt v) = return (fromIntegral v)
expectVNumericIn _ (VFloat v) = return (realToFrac v)
expectVNumericIn _ (VDouble v) = return v
expectVNumericIn context v = throwError $ expectSpecificType context "numeric value" v

expectVList :: Value -> EvalM [Value]
expectVList (VList xs) = return xs
expectVList v = throwError $ expectVErr (VList []) v

-- Error formatter
expectVErr :: Value -> Value -> String
expectVErr expectedVal inputVal =
  "Type error: expected `" ++ typeName expectedVal ++ "`, but got " ++ formatTypedValue inputVal ++ "."

expectSpecificType :: String -> String -> Value -> String
expectSpecificType context expected inputVal =
  "Type error: " ++ context ++ " expects a " ++ expected ++ ", but got " ++ formatTypedValue inputVal ++ "."

formatTypedValue :: Value -> String
formatTypedValue v = formatValue v ++ " :: " ++ typeName v

formatValue :: Value -> String
formatValue v =
  case v of
    VNull -> "null"
    VInt n -> show n
    VBool b -> if b then "true" else "false"
    VChar c -> show c
    VFloat f -> show f
    VDouble d -> show d
    VString s -> show s
    VList xs -> "[" ++ intercalate ", " (map formatValue xs) ++ "]"

-- For converting value types to texts
typeName :: Value -> String
typeName v = case v of
  VNull -> "Null"
  VInt _ -> "Int"
  VBool _ -> "Bool"
  VChar _ -> "Char"
  VFloat _ -> "Float"
  VDouble _ -> "Double"
  VString _ -> "String"
  VList _ -> "List"
