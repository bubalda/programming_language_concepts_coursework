module Lang.Eval.Eval (evalExpr, evalStmt, runEval) where

import Control.Monad.Except (runExceptT, throwError)
import Data.Char (toLower)
import Data.Functor.Identity (Identity (runIdentity))
import Data.List (foldl1', group, sort)
import qualified Data.Map as Map
import Lang.Eval.Errors (expectVBool, expectVInt, expectVList, expectVNumeric, formatTypedValue)
import Lang.Eval.Op (applyAssignOp, calcBinOp)
import Lang.Eval.Types (EvalM, ProgramEnv, Value (..))
import Lang.Parser.Expr (Expr (..), Slice (..), Stmt (..), TwoExprOperator (..), Type (..))

-- Run evaluator
runEval :: EvalM a -> Either String a
runEval ev = runIdentity (runExceptT ev)

evalStmt :: ProgramEnv -> Stmt -> EvalM (ProgramEnv, Value)
evalStmt env stmt =
  case stmt of
    ExprStmt e -> do
      val <- evalExpr env e
      return (env, val)
    Assign name expr -> do
      val <- evalExpr env expr
      let env' = Map.insert name val env
      return (env', val)
    AssignOp op name expr -> do
      val <- evalExpr env expr
      old <- case Map.lookup name env of
        Just v -> return v
        Nothing -> throwError ("Undefined identifier: " ++ name)

      result <- applyAssignOp op old val
      let env' = Map.insert name result env
      return (env', result)
    If cond thenStmt elseStmt -> do
      ok <- evalExpr env cond >>= expectVBool
      if ok
        then evalStmt env thenStmt
        else case elseStmt of
          Just s -> evalStmt env s
          Nothing -> return (env, VNull)
    Decl typ str expr -> do
      val <- evalExpr env expr
      casted <- castToType typ val
      let env' = Map.insert str casted env
      return (env', casted)

-- Evaluation of expressions
evalExpr :: ProgramEnv -> Expr -> EvalM Value
evalExpr env expr =
  case expr of
    NullLit -> return VNull
    BoolLit n -> return $ VBool n
    IntLit n -> return $ VInt n
    FloatLit n -> return $ VFloat n
    DoubleLit n -> return $ VDouble n
    CharLit n -> return $ VChar n
    StringLit n -> return $ VString n
    Var v ->
      case Map.lookup v env of
        Just val -> return val
        Nothing ->

          if map toLower v == "e"
            then return $ VDouble (exp 1)
            else throwError ("Undefined identifier: " ++ v)
    Let name bindExpr bodyExpr -> do
      bindVal <- eval bindExpr
      let innerEnv = Map.insert name bindVal env
      evalExpr innerEnv bodyExpr

    BinOp And a b -> logicOpLazy False a b
    BinOp Or a b -> logicOpLazy True a b
    BinOp op a b -> do
      va <- eval a
      vb <- eval b
      calcBinOp op va vb

    Negate a -> do
      va <- eval a
      case va of
        VInt x -> return $ VInt (-x)
        VFloat x -> return $ VFloat (-x)
        VDouble x -> return $ VDouble (-x)
        _ -> VDouble . negate <$> expectVNumeric va
    Not a -> do
      va <- eBool a
      return $ VBool (not va)


    Call fnExpr argsExpr -> evalCall fnExpr argsExpr


    ListLit xs -> VList <$> mapM eval xs
    ListRange startExpr endExpr -> do
      s <- eInt startExpr
      e <- eInt endExpr
      let rangeVals =
            if s <= e
              then [s .. e]
              else [s, s -1 .. e]
      return $ VList (map VInt rangeVals)
    ListIndex xsExpr idxExpr -> do
      xs <- eval xsExpr >>= expectVList
      idx <- eInt idxExpr
      atIndex xs idx
    ListSlice xsExpr sl -> do
      xs <- eval xsExpr >>= expectVList
      sliceList env xs sl
  where
    -- Evaluation helper
    eval = evalExpr env
    eInt e = eval e >>= expectVInt
    eBool e = eval e >>= expectVBool

    -- Lazy evaluation of and/or
    logicOpLazy retEarly a b = do
      va <- eBool a
      if va == retEarly
        then return (VBool retEarly)
        else do
          vb <- eBool b
          return (VBool vb)

    evalCall fnExpr argExprs = do
      args <- mapM eval argExprs
      fnName <- case fnExpr of
        Var n -> return (map toLower n)
        _ -> throwError "Function call target must be an identifier"
      dispatchCall fnName args


-- Function dispatch calls
dispatchCall :: String -> [Value] -> EvalM Value
dispatchCall fn args =
  case fn of

    "length" -> fnLength args

    -- Power/Root
    "sqrt" -> unaryFloat "sqrt" args $ \x ->
      if x < 0 then Left "sqrt: input must be >= 0" else Right (sqrt x)
    "cbrt" -> unaryFloat "cbrt" args $ \x ->
      Right (signum x * (abs x ** (1 / 3)))
    "pow" -> binaryFloat "pow" args $ \x y -> Right (x ** y)
    "exp" -> unaryFloat "exp" args (Right . exp)
    "square" -> unaryFloat "square" args (\x -> Right (x * x))
    "cube" -> unaryFloat "cube" args (\x -> Right (x * x * x))
    "exp10" -> unaryFloat "exp10" args (\x -> Right (10 ** x))

    -- Trigonometric
    "sin" -> unaryFloat "sin" args (Right . sin)
    "cos" -> unaryFloat "cos" args (Right . cos)
    "tan" -> unaryFloat "tan" args (Right . tan)
    "asin" -> unaryFloat "asin" args $ \x ->
      if x < (-1) || x > 1 then Left "asin: input must be in [-1,1]" else Right (asin x)
    "acos" -> unaryFloat "acos" args $ \x ->
      if x < (-1) || x > 1 then Left "acos: input must be in [-1,1]" else Right (acos x)
    "atan" -> unaryFloat "atan" args (Right . atan)
    "atan2" -> binaryFloat "atan2" args (\y x -> Right (atan2 y x))
    "sec" -> unaryFloat "sec" args $ \x -> reciprocalSafe "sec" (cos x)
    "csc" -> unaryFloat "csc" args $ \x -> reciprocalSafe "csc" (sin x)
    "cot" -> unaryFloat "cot" args $ \x -> reciprocalSafe "cot" (tan x)
    "versin" -> unaryFloat "versin" args (\x -> Right (1 - cos x))
    "exsec" -> unaryFloat "exsec" args $ \x -> do
      s <- reciprocalSafe "exsec" (cos x)
      Right (s -1)

    -- Logarithmic
    "ln" -> unaryFloat "ln" args $ \x ->
      if x <= 0 then Left "ln: input must be > 0" else Right (log x)
    "log10" -> unaryFloat "log10" args $ \x ->
      if x <= 0 then Left "log10: input must be > 0" else Right (logBase 10 x)
    "log2" -> unaryFloat "log2" args $ \x ->
      if x <= 0 then Left "log2: input must be > 0" else Right (logBase 2 x)
    "log1p" -> unaryFloat "log1p" args $ \x ->
      if x <= (-1) then Left "log1p: input must be > -1" else Right (log (1 + x))
    "log" -> fnLog args

    -- Combinatorial
    "fact" -> unaryInt "fact" args fact
    "fact2" -> unaryInt "fact2" args fact2
    "comb" -> binaryInt "comb" args comb
    "perm" -> binaryInt "perm" args perm
    "fib" -> unaryInt "fib" args fib
    "gamma" -> unaryFloat "gamma" args gamma
    "gcd" -> fnGcd args
    "lcm" -> fnLcm args

    -- Statistics
    "mean" -> fnMean args
    "median" -> fnMedian args
    "mode" -> fnMode args
    "sum" -> fnSum args
    "product" -> fnProduct args
    "min" -> fnMin args
    "max" -> fnMax args
    "stddev" -> fnStddev args

    -- Hyperbolic
    "sinh" -> unaryFloat "sinh" args (Right . sinh)
    "cosh" -> unaryFloat "cosh" args (Right . cosh)
    "tanh" -> unaryFloat "tanh" args (Right . tanh)
    "csch" -> unaryFloat "csch" args $ \x -> reciprocalSafe "csch" (sinh x)
    "sech" -> unaryFloat "sech" args $ \x -> reciprocalSafe "sech" (cosh x)
    "coth" -> unaryFloat "coth" args $ \x -> reciprocalSafe "coth" (tanh x)
    "asinh" -> unaryFloat "asinh" args (Right . asinh)
    "acosh" -> unaryFloat "acosh" args $ \x ->
      if x < 1 then Left "acosh: input must be >= 1" else Right (acosh x)

    _ -> throwError ("Unknown function: `" ++ fn ++ "`")

paramError :: String -> Int -> String -> String
paramError f x t = "ParamError: The function `" ++ f ++ "` expects exactly "++ show x ++ " argument" ++ plural ++ withType
  where
    plural :: String 
    plural = if (x > 1) then "s" else ""

    withType :: String 
    withType = if (length t /= 0) then (" with type " ++ t ++ ".") else "."

unaryFloat :: String -> [Value] -> (Double -> Either String Double) -> EvalM Value
unaryFloat name args f =
  case args of
    [x] -> do
      vx <- expectVNumeric x
      case f vx of
        Left err -> throwError err
        Right out -> pureDouble name out
    _ -> throwError (paramError name 1 "")

binaryFloat :: String -> [Value] -> (Double -> Double -> Either String Double) -> EvalM Value
binaryFloat name args f =
  case args of
    [a, b] -> do
      x <- expectVNumeric a
      y <- expectVNumeric b
      case f x y of
        Left err -> throwError err
        Right out -> pureDouble name out
    _ -> throwError (paramError name 2 "")

unaryInt :: String -> [Value] -> (Int -> Either String Int) -> EvalM Value
unaryInt name args f =
  case args of
    [x] -> do
      vx <- expectVInt x
      case f vx of
        Left err -> throwError err
        Right out -> return $ VInt out
    _ -> throwError (paramError name 1 "integer")

binaryInt :: String -> [Value] -> (Int -> Int -> Either String Int) -> EvalM Value
binaryInt name args f =
  case args of
    [a, b] -> do
      x <- expectVInt a
      y <- expectVInt b
      case f x y of
        Left err -> throwError err
        Right out -> return $ VInt out
    _ -> throwError (paramError name 2 "integer")

pureDouble :: String -> Double -> EvalM Value
pureDouble name x
  | isNaN x || isInfinite x = throwError (name ++ ": invalid numeric result")
  | otherwise = return (VDouble x)

reciprocalSafe :: String -> Double -> Either String Double
reciprocalSafe fn x
  | abs x < 1e-7 = Left (fn ++ ": undefined for this input")
  | otherwise = Right (1 / x)

toFloatListArgs :: String -> [Value] -> EvalM [Double]
toFloatListArgs fn args =
  case args of
    [VList xs] -> mapM expectVNumeric xs
    _ ->
      if null args
        then throwError (paramError fn 1 "numeric")
        else mapM expectVNumeric args

toIntListArgs :: String -> [Value] -> EvalM [Int]
toIntListArgs fn args =
  case args of
    [VList xs] -> mapM expectVInt xs
    _ ->
      if null args
        then throwError (paramError fn 1 "integer")
        else mapM expectVInt args

-- =====================
-- Task5: Function impl
-- =====================
fnLength :: [Value] -> EvalM Value
fnLength args =
  case args of
    [VList xs] -> return $ VInt (length xs)
    [VString s] -> return $ VInt (length s)
    [_] -> throwError "length expects a list or string"
    _ -> throwError "length expects exactly 1 argument"

fnLog :: [Value] -> EvalM Value
fnLog args =
  case args of
    [x] -> unaryFloat "log" [x] $ \v ->
      if v <= 0 then Left "log: input must be > 0" else Right (logBase 10 v)
    [base, x] -> binaryFloat "log" [base, x] $ \b v ->
      if b <= 0 || b == 1
        then Left "log: base must be > 0 and != 1"
        else
          if v <= 0
            then Left "log: input must be > 0"
            else Right (logBase b v)
    _ -> throwError "log: expects 1 or 2 arguments"

fnGcd :: [Value] -> EvalM Value
fnGcd args = do
  xs <- toIntListArgs "gcd" args
  if null xs
    then throwError "gcd: expects at least 1 integer argument"
    else return $ VInt (foldl1' gcd xs)

fnLcm :: [Value] -> EvalM Value
fnLcm args = do
  xs <- toIntListArgs "lcm" args
  if null xs
    then throwError "lcm: expects at least 1 integer argument"
    else return $ VInt (foldl1' lcm xs)

fnMean :: [Value] -> EvalM Value
fnMean args = do
  xs <- toFloatListArgs "mean" args
  ensureNonEmpty "mean" xs
  pureDouble "mean" (sum xs / fromIntegral (length xs))

fnMedian :: [Value] -> EvalM Value
fnMedian args = do
  xs <- toFloatListArgs "median" args
  ensureNonEmpty "median" xs
  let ys = sort xs
      n = length ys
      m = n `div` 2
      out =
        if odd n
          then ys !! m
          else (ys !! (m -1) + ys !! m) / 2
  pureDouble "median" out

fnMode :: [Value] -> EvalM Value
fnMode args = do
  xs <- toFloatListArgs "mode" args
  ensureNonEmpty "mode" xs
  let groups = group (sort xs)
      best = foldl1' (\a b -> if length a >= length b then a else b) groups
  case best of
    (v : _) -> return $ VDouble v
    [] -> throwError "mode: empty input"

fnSum :: [Value] -> EvalM Value
fnSum args = do
  xs <- toFloatListArgs "sum" args
  pureDouble "sum" (sum xs)

fnProduct :: [Value] -> EvalM Value
fnProduct args = do
  xs <- toFloatListArgs "product" args
  pureDouble "product" (product xs)

fnMin :: [Value] -> EvalM Value
fnMin args = do
  xs <- toFloatListArgs "min" args
  ensureNonEmpty "min" xs
  pureDouble "min" (minimum xs)

fnMax :: [Value] -> EvalM Value
fnMax args = do
  xs <- toFloatListArgs "max" args
  ensureNonEmpty "max" xs
  pureDouble "max" (maximum xs)

fnStddev :: [Value] -> EvalM Value
fnStddev args = do
  xs <- toFloatListArgs "stddev" args
  ensureNonEmpty "stddev" xs
  let m = sum xs / fromIntegral (length xs)
      variance = sum (map (\x -> (x - m) * (x - m)) xs) / fromIntegral (length xs)
  pureDouble "stddev" (sqrt variance)

ensureNonEmpty :: String -> [a] -> EvalM ()
ensureNonEmpty fn xs =
  if null xs
    then throwError (fn ++ " expects at least 1 value")
    else return ()

fact :: Int -> Either String Int
fact n
  | n < 0 = Left "fact: input must be >= 0"
  | otherwise = Right (product [1 .. n])

fact2 :: Int -> Either String Int
fact2 n
  | n < 0 = Left "fact2: input must be >= 0"
  | n == 0 || n == 1 = Right 1
  | otherwise = Right (product [n, n -2 .. 1])

comb :: Int -> Int -> Either String Int
comb n r
  | n < 0 || r < 0 = Left "comb: n and r must be >= 0"
  | r > n = Left "comb: r must be <= n"
  | otherwise = do
      nf <- fact n
      rf <- fact r
      nrf <- fact (n - r)
      Right (nf `div` (rf * nrf))

perm :: Int -> Int -> Either String Int
perm n r
  | n < 0 || r < 0 = Left "perm: n and r must be >= 0"
  | r > n = Left "perm: r must be <= n"
  | otherwise = do
      nf <- fact n
      nrf <- fact (n - r)
      Right (nf `div` nrf)

fib :: Int -> Either String Int
fib n
  | n < 0 = Left "fib: input must be >= 0"
  | otherwise = Right (go n 0 1)
  where
    go 0 a _ = a
    go k a b = go (k -1) b (a + b)

-- Similar to Lanczos
gamma :: Double -> Either String Double
gamma zf =
  let z = zf
   in if z <= 0 && abs (z - fromIntegral (round z :: Int)) < 1e-12
        then Left "gamma: undefined for non-positive integers"
        else Right (gammaLanczos z)

gammaLanczos :: Double -> Double
gammaLanczos z
  | z < 0.5 = pi / (sin (pi * z) * gammaLanczos (1 - z))
  | otherwise =
      let p =
            [ 676.5203681218851,
              -1259.1392167224028,
              771.32342877765313,
              -176.61502916214059,
              12.507343278686905,
              -0.13857109526572012,
              9.9843695780195716e-6,
              1.5056327351493116e-7
            ]
          g = 7.0 :: Double
          z' = z -1
          x = 0.99999999999980993 + sum [c / (z' + fromIntegral i + 1) | (i, c) <- zip [0 :: Int ..] p]
          t = z' + g + 0.5
       in sqrt (2 * pi) * (t ** (z' + 0.5)) * exp (-t) * x

-- =====================
-- Task5: List helpers
-- =====================
atIndex :: [Value] -> Int -> EvalM Value
atIndex xs idx = do
  let n = length xs
      i = if idx < 0 then n + idx else idx
  if i < 0 || i >= n
    then throwError "List index out of bounds"
    else return (xs !! i)

sliceList :: ProgramEnv -> [Value] -> Slice -> EvalM Value
sliceList env xs (Slice mStart mStop mStep) = do
  step <- case mStep of
    Nothing -> return 1
    Just e -> expectVInt =<< evalTemp e

  if step == 0
    then throwError "Slice step cannot be 0"
    else do
      start <- resolveIndex mStart (if step > 0 then 0 else n -1)
      stop <- resolveIndex mStop (if step > 0 then n else (-1))
      let idxs = buildIndices start stop step
      return $ VList [xs !! i | i <- idxs, i >= 0, i < n]
  where
    n = length xs

    -- Helper functions
    evalTemp :: Expr -> EvalM Value
    evalTemp e = evalExpr env e

    resolveIndex :: Maybe Expr -> Int -> EvalM Int
    resolveIndex Nothing defVal = return defVal
    resolveIndex (Just e) _ = do
      idx <- expectVInt =<< evalTemp e
      return $ if idx < 0 then n + idx else idx

    buildIndices :: Int -> Int -> Int -> [Int]
    buildIndices start stop stepSize
      | stepSize > 0 =
          if start >= stop then [] else [start, start + stepSize .. stop -1]
      | otherwise =
          if start <= stop then [] else [start, start + stepSize .. stop +1]

castToType :: Type -> Value -> EvalM Value
castToType typ val =
  case typ of
    TInt ->
      case val of
        VInt n -> return (VInt n)
        VFloat n -> return (VInt (truncate n))
        VDouble n -> return (VInt (truncate n))
        _ -> throwError (castError "Int" val)
    TFloat ->
      case val of
        VInt n -> return (VFloat (fromIntegral n))
        VFloat n -> return (VFloat n)
        VDouble n -> return (VFloat (realToFrac n))
        _ -> throwError (castError "Float" val)
    TDouble ->
      case val of
        VInt n -> return (VDouble (fromIntegral n))
        VFloat n -> return (VDouble (realToFrac n))
        VDouble n -> return (VDouble n)
        _ -> throwError (castError "Double" val)
    TBool ->
      case val of
        VBool b -> return (VBool b)
        _ -> throwError (castError "Bool" val)
    TChar ->
      case val of
        VChar c -> return (VChar c)
        _ -> throwError (castError "Char" val)
    TString ->
      case val of
        VString s -> return (VString s)
        _ -> throwError (castError "String" val)
  where
    castError targetType badValue =
      "Type error: declaration expects `" ++ targetType ++ "`, but got " ++ formatTypedValue badValue ++ "."
