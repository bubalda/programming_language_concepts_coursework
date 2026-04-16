module Lang.TypeChecker.Helper where

import Lang.Syntax.Syntax
import Lang.TypeChecker.Types


-- Helpers for TypeChecker (Just move to TypeChecker?)
isNumeric :: Type -> Bool
isNumeric TInt = True
isNumeric TFloat = True
isNumeric TDouble = True
isNumeric TDynamic = True
isNumeric _ = False

numericCoercion :: Type -> Type -> Type 
numericCoercion TDouble _ = TDouble
numericCoercion _ TDouble = TDouble
numericCoercion TFloat _ = TFloat 
numericCoercion _ TFloat = TFloat
numericCoercion _ _ = TInt

isNumericType :: [Type] -> Maybe Type --(combine all numeric types to the most precise one)
isNumericType [] = Nothing
isNumericType xs
    | all isNumeric xs = Just (foldl1 numericCoercion xs)
    | otherwise = Nothing

isCompatibleType :: Type -> Type -> Bool
isCompatibleType t1 t2 
    | t1 == t2 = True
    | t1 == TDynamic = True
    | t2 == TDynamic = True
    | isNumeric t1 && isNumeric t2 = True
    | otherwise = False


-- Function Signatures
data FunctionSignature =
     FunctionSignature
    {
        functionName :: String, -- eg: "sqrt"
        functionArity :: Int,   -- Num of arguments
        functionCheckArg :: [Type] -> Either TypeError (), -- Validation func
        funcReturn :: Type      -- The Type it returns
    }


