module Lang.Eval.Op
  ( applyAssignOp,
    assignToBin,
    calcBinOp,
  )
where

import Data.Bits (Bits (shiftL, shiftR, xor), (.&.), (.|.))
import Data.Maybe (isJust)
import Control.Monad.Except (throwError)
import Lang.Eval.Errors (expectVBoolIn, expectVIntIn, expectVNumericIn)
import Lang.Eval.Types (EvalM, Value (..))
import Lang.Parser.Expr (AssignOperator (..), TwoExprOperator (..))

-- Evaluator
applyAssignOp :: AssignOperator -> Value -> Value -> EvalM Value
applyAssignOp op old val = calcBinOp (assignToBin op) old val

-- Helper function
assignToBin :: AssignOperator -> TwoExprOperator
assignToBin AddEq = Add
assignToBin SubEq = Sub
assignToBin MulEq = Mul
assignToBin DivEq = Div
assignToBin ModEq = Mod
assignToBin BitAndEq = BitAnd
assignToBin BitOrEq = BitOr
assignToBin BitXorEq = BitXor
assignToBin BitLShiftEq = BitLShift
assignToBin BitRShiftEq = BitRShift

-- a () b operators, where a and b are variables
calcBinOp :: TwoExprOperator -> Value -> Value -> EvalM Value
calcBinOp op a b =
  case op of

    Add -> numBinOp (+) (+)
    Sub -> numBinOp (-) (-)
    Mul -> numBinOp (*) (*)
    Div -> do
      y <- expectNumericOperand "right-hand side" b
      if abs y < 1e-7
        then throwError "Division by zero"
        else numBinOp div (/)
    Mod -> intBinOpSafe mod "Modulo by zero"

    -- Bit operations
    BitAnd -> intBinOp (.&.)
    BitOr -> intBinOp (.|.)
    BitXor -> intBinOp xor
    BitLShift -> intBinOp shiftL
    BitRShift -> intBinOp shiftR

    -- Comparison
    Eq -> cmpEq True
    Neq -> cmpEq False
    Lte -> cmpNum (<=)
    Lt -> cmpNum (<)
    Gte -> cmpNum (>=)
    Gt -> cmpNum (>)
    And -> logicAnd
    Or -> logicOr
    
  where
    opLabel = "operator (" ++ operatorSymbol op ++ ")"

    intBinOp f = do
      x <- expectIntegerOperand "left-hand side" a
      y <- expectIntegerOperand "right-hand side" b
      return $ VInt (f x y)

    intBinOpSafe f err = do
      x <- expectIntegerOperand "left-hand side" a
      y <- expectIntegerOperand "right-hand side" b
      if y == 0 then throwError err else return $ VInt (f x y)

    numBinOp fInt fFloat = case (a, b) of
      (VInt x, VInt y) -> return $ VInt (fInt x y)
      _ -> do
        x <- expectNumericOperand "left-hand side" a
        y <- expectNumericOperand "right-hand side" b
        return $ numericResult (fFloat x y)

    cmpNum f = do
      x <- expectNumericOperand "left-hand side" a
      y <- expectNumericOperand "right-hand side" b
      return $ VBool (f x y)

    cmpEq shouldEq =
      case (numericValue a, numericValue b) of
        (Just x, Just y) -> return $ VBool ((x == y) == shouldEq)
        _ -> return $ VBool ((a == b) == shouldEq)

    logicAnd = do
      x <- expectBooleanOperand "left-hand side" a
      y <- expectBooleanOperand "right-hand side" b
      return $ VBool (x && y)

    logicOr = do
      x <- expectBooleanOperand "left-hand side" a
      y <- expectBooleanOperand "right-hand side" b
      return $ VBool (x || y)

    expectNumericOperand side = expectVNumericIn (opLabel ++ " on the " ++ side)
    expectIntegerOperand side = expectVIntIn (opLabel ++ " on the " ++ side)
    expectBooleanOperand side = expectVBoolIn (opLabel ++ " on the " ++ side)

    numericValue :: Value -> Maybe Double
    numericValue (VInt x) = Just (fromIntegral x)
    numericValue (VFloat x) = Just (realToFrac x)
    numericValue (VDouble x) = Just x
    numericValue _ = Nothing

    numericResult :: Double -> Value
    numericResult out
      | hasDouble = VDouble out
      | hasFloat = VFloat (realToFrac out)
      | otherwise = VDouble out

    hasDouble = isJust (onlyDouble a) || isJust (onlyDouble b)
    hasFloat = isJust (onlyFloat a) || isJust (onlyFloat b)

    onlyDouble (VDouble x) = Just x
    onlyDouble _ = Nothing

    onlyFloat (VFloat x) = Just x
    onlyFloat _ = Nothing

operatorSymbol :: TwoExprOperator -> String
operatorSymbol op =
  case op of
    Add -> "+"
    Sub -> "-"
    Mul -> "*"
    Div -> "/"
    Mod -> "%"
    BitAnd -> "&"
    BitOr -> "|"
    BitXor -> "^"
    BitLShift -> "<<"
    BitRShift -> ">>"
    Eq -> "=="
    Neq -> "!="
    Lte -> "<="
    Lt -> "<"
    Gte -> ">="
    Gt -> ">"
    And -> "&&"
    Or -> "||"
