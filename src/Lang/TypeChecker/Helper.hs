module Lang.TypeChecker.Helper where

import Lang.Syntax.Syntax

-- Helpers for TypeChecker (Just move to TypeChecker?)
isNumeric :: Type -> Bool
isNumeric TInt = True
isNumeric TFloat = True
isNumeric TDouble = True
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
    | isNumeric t1 && isNumeric t2 = True
    | otherwise = False
