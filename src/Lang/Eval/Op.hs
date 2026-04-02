module Lang.Eval.Op
  ( applyAssignOp,
    assignToBin,
    calcBinOp,
  )
where

import Data.Bits (Bits (shiftL, shiftR, xor), (.&.), (.|.))
import Lang.Eval.Types (EvalM, Value (..))
import Lang.Parser.Expr (AssignOperator (..), TwoExprOperator (..))
import Lang.Eval.Errors (expectVBool, expectVInt, expectVNumeric)
import Control.Monad.Except (throwError)

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
      y <- expectVNumeric b
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
    intBinOp f = do
      x <- expectVInt a
      y <- expectVInt b
      return $ VInt (f x y)

    intBinOpSafe f err = do
      x <- expectVInt a
      y <- expectVInt b
      if y == 0 then throwError err else return $ VInt (f x y)

    numBinOp fInt fFloat = case (a, b) of
      (VInt x, VInt y) -> return $ VInt (fInt x y)
      _ -> do
        x <- expectVNumeric a
        y <- expectVNumeric b
        return $ VFloat (fFloat x y)

    cmpNum f = do
      x <- expectVNumeric a
      y <- expectVNumeric b
      return $ VBool (f x y)

    cmpEq shouldEq = case (a, b) of

      (VInt x, VFloat y) -> return $ VBool ((fromIntegral x == y) == shouldEq)
      (VFloat x, VInt y) -> return $ VBool ((x == fromIntegral y) == shouldEq)
      _ -> return $ VBool ((a == b) == shouldEq)

    logicAnd = do
      x <- expectVBool a
      y <- expectVBool b
      return $ VBool (x && y)

    logicOr = do
      x <- expectVBool a
      y <- expectVBool b
      return $ VBool (x || y)
