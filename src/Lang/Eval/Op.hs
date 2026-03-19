module Lang.Eval.Op
  ( applyAssignOp,
    assignToBin,
    calcBinOp,
  )
where

import Data.Bits (Bits (shiftL, shiftR, xor), (.&.), (.|.))
import Lang.Eval.Types (EvalM, Value (..))
import Lang.Parser.Expr (AssignOperator (..), TwoExprOperator (..))
import Lang.Eval.Errors (expectVInt, expectVBool)

applyAssignOp :: AssignOperator -> Value -> Value -> EvalM Value
applyAssignOp op old val = calcBinOp (assignToBin op) old val

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

calcBinOp :: TwoExprOperator -> Value -> Value -> EvalM Value
calcBinOp op a b =
  case op of
    -- Arithmetic operations (TODO type check for float and allow it)
    Add -> intBinOp (+)
    Sub -> intBinOp (-)
    Mul -> intBinOp (*)
    Div -> intBinOp div
    Mod -> intBinOp mod

    -- Bit operations
    BitAnd -> intBinOp (.&.)
    BitOr -> intBinOp (.|.)
    BitXor -> intBinOp xor
    BitLShift -> intBinOp shiftL
    BitRShift -> intBinOp shiftR

    -- Comparison operations (TODO type check for float and allow it)
    Eq -> return $ VBool (a == b)
    Neq -> return $ VBool (a /= b)
    Lte -> cmpInt (<=)
    Lt -> cmpInt (<)
    Gte -> cmpInt (>=)
    Gt -> cmpInt (>)
    And -> logicAnd
    Or -> logicOr
    
  where
    intBinOp f = do
      x <- expectVInt a
      y <- expectVInt b
      return $ VInt (f x y)

    cmpInt f = do
      x <- expectVInt a
      y <- expectVInt b
      return $ VBool (f x y)

    logicAnd = do
      x <- expectVBool a
      y <- expectVBool b
      return $ VBool (x && y)

    logicOr = do
      x <- expectVBool a
      y <- expectVBool b
      return $ VBool (x || y)
