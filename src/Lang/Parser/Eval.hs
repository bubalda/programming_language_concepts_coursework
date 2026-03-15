module Lang.Parser.Eval (ProgramEnv, Expr (..), Stmt (..), evalExpr, evalStmt, printEval, printEvalPretty) where

import qualified Data.Map as Map
import Data.Bits (Bits(xor, shiftL, shiftR), (.|.), (.&.))
import Lang.Repl.Helper (wrapSection)

type ProgramEnv = Map.Map String Value

data Expr
  = Var String
  | IntLit Int | BoolLit Bool | NullLit
  | Add Expr Expr | Sub Expr Expr | Mul Expr Expr | Div Expr Expr | Mod Expr Expr | Pow Expr Expr | FloorDiv Expr Expr
  | Negate Expr | Brack Expr
  | BinAND Expr Expr | BinOR Expr Expr | BinXOR Expr Expr | BinLShift Expr Expr | BinRShift Expr Expr
  | Eq Expr Expr | Neq Expr Expr | Lte Expr Expr | Lt Expr Expr | Gte Expr Expr | Gt Expr Expr
  | And Expr Expr | Or Expr Expr | Not Expr 
  deriving (Show)

data Stmt
  = Assign String Expr
  | ExprStmt Expr
  deriving (Show)

data Value
  = VInt Int
  | VBool Bool
  | VNull
  | VError String
  deriving (Show, Eq)

evalExpr :: ProgramEnv -> Expr -> Value
evalExpr env expr =
  case expr of
    IntLit n -> VInt n
    BoolLit n -> VBool n
    NullLit -> VNull
    Var v ->
      case Map.lookup v env of
        Just val -> val
        Nothing -> error ("Undefined identifier: " ++ v)
    
    Add a b       -> VInt $ eInt a + eInt b
    Sub a b       -> VInt $ eInt a - eInt b
    Mul a b       -> VInt $ eInt a * eInt b
    Div a b       -> VInt $ eInt a `div` eInt b
    Mod a b       -> VInt $ eInt a `mod` eInt b
    Pow a b       -> VInt $ eInt a ^ eInt b
    FloorDiv a b  -> VInt $ eInt a `div` eInt b

    Negate a      -> VInt $ -1 * eInt a
    Brack a       -> eval a

    Eq a b        -> VBool (eval a == eval b)
    Neq a b       -> VBool (eval a /= eval b)
    
    Lte a b       -> VBool (eInt a <= eInt b)
    Lt a b        -> VBool (eInt a < eInt b)
    Gte a b       -> VBool (eInt a >= eInt b)
    Gt a b        -> VBool (eInt a > eInt b)

    -- Lazy evaluation
    And a b -> case eval a of
      VBool False -> VBool False
      VBool True  -> VBool (eBool b) 
      v -> error $ expectVErr (VBool False) v 
      
    Or a b -> case eval a of
      VBool True  -> VBool True
      VBool False -> VBool (eBool b) 
      v -> error $ expectVErr (VBool False) v 
      
    Not a -> VBool (not (eBool a))

    BinAND a b    -> VInt (eInt a .&. eInt b)
    BinOR a b     -> VInt (eInt a .|. eInt b)
    BinXOR a b    -> VInt (eInt a `xor` eInt b)
    BinLShift a b -> VInt (eInt a `shiftL` eInt b) 
    BinRShift a b -> VInt (eInt a `shiftR` eInt b)
    where
      -- Inner helper
      eval :: Expr -> Value
      eval = evalExpr env

      eInt :: Expr -> Int
      eInt = expectVInt . eval

      eBool :: Expr -> Bool
      eBool = expectVBool . eval

      expectVInt :: Value -> Int
      expectVInt (VInt v) = v
      expectVInt v = error $ expectVErr (VInt 0) v 

      expectVBool :: Value -> Bool
      expectVBool (VBool v) = v
      expectVBool v = error $ expectVErr (VBool False) v 

      -- TODO: Monadic
      expectVErr :: Value -> Value -> String
      expectVErr expectedVal inputVal = 
        let
          text v = case v of 
            (VNull) -> "null"
            (VInt _) -> "integer"
            (VBool _) -> "boolean"
            (VError _) -> "error"
        in
          "Expected type `" ++ text expectedVal ++ "` object, got type `" ++ text inputVal ++ "` instead."

evalStmt :: ProgramEnv -> Stmt -> Either String (ProgramEnv, Value)
evalStmt env stmt =
  case stmt of
    ExprStmt e ->
      Right (env, evalExpr env e)
    Assign name expr ->
      let val = evalExpr env expr
          env' = Map.insert name val env
       in Right (env', val)


printEval :: Value -> IO ()
printEval (VBool b) = putStrLn (show b)
printEval (VInt i) = putStrLn (show i)
printEval (VNull) = putStrLn "null"
printEval (VError e) = putStrLn e

printEvalPretty :: Value -> IO ()
printEvalPretty val = wrapSection "Evaluation Result" (printEval val)
