module Lang.TypeChecker.Types where

import Lang.Syntax.Syntax
import qualified Data.Map as Map
import Data.List (intercalate)


type TypeEnv = Map.Map String Type

data TypeError
    = UndefinedVariable String
    | ExpectedNumeric Type Expr
    | ExpectedTypeBool Type Expr
    | ExpectedTypeInt Type Expr
    | ExpectedList Type Expr
    | TypeMismatch Type Type Expr
    | EmptyList Expr
    | UnknownFunction String
    | ArgsMismatch String Int Int
    | NotFunction Expr

prettyPrintType :: Type -> String 
prettyPrintType t =
    case t of
        TInt    -> "Int"
        TFloat  -> "Float"
        TDouble -> "Double"
        TBool   -> "Bool"
        TChar   -> "Char"
        TString -> "String"
        TNull   -> "Null"
        TList inner -> "[" ++ prettyPrintType inner ++ "]" 

prettyPrintExpr :: Expr -> String 
prettyPrintExpr (Var s) = s
prettyPrintExpr (IntLit n) = show n
prettyPrintExpr (BoolLit b) = if b then "True" else "False"
prettyPrintExpr (StringLit s) = "\"" ++ s ++ "\""

prettyPrintExpr (Call (Var f) args) = 
    f ++ "(" ++ intercalate ", " (map prettyPrintExpr args) ++ ")"

prettyPrintExpr (BinOp op e1 e2) =
    "(" ++ prettyPrintExpr e1 ++ " " ++ prettyPrintOp op ++ " " ++ prettyPrintExpr e2 ++ ")"
    

prettyPrintExpr e = show e  -- Fallback

prettyPrintOp :: TwoExprOperator -> String
prettyPrintOp Add = "+"
prettyPrintOp Sub = "-"
prettyPrintOp Mul = "*"
prettyPrintOp Div = "/"
prettyPrintOp Mod = "%"
prettyPrintOp Eq  = "=="
prettyPrintOp Neq = "!="
prettyPrintOp Lt  = "<"
prettyPrintOp Lte = "<="
prettyPrintOp Gt  = ">"
prettyPrintOp Gte = ">="
prettyPrintOp And = "&&"
prettyPrintOp Or  = "||"
prettyPrintOp BitAnd = "&"
prettyPrintOp BitOr  = "|"
prettyPrintOp BitXor = "^"
prettyPrintOp BitLShift = "<<"
prettyPrintOp BitRShift = ">>"

instance Show TypeError where
    show err = case err of

        UndefinedVariable name ->
            "Type Error: Undefined variable '" ++ name ++ "'"

        TypeMismatch expected actual expr ->
            "Type Error: Expected type " ++ prettyPrintType expected ++
            ", but got " ++ prettyPrintType actual ++
            " in " ++ prettyPrintExpr expr

        ExpectedNumeric actual expr ->
            "Type Error: Expected a numeric value, but got " ++
            prettyPrintType actual ++
            " in " ++ prettyPrintExpr expr

        ExpectedTypeInt actual expr ->
            "Type Error: Expected an integer, but got " ++
            prettyPrintType actual ++
            " in " ++ prettyPrintExpr expr

        ExpectedTypeBool actual expr ->
            "Type Error: Expected a boolean, but got " ++
            prettyPrintType actual ++
            " in " ++ prettyPrintExpr expr

        ExpectedList actual expr ->
            "Type Error: Expected a list, but got " ++
            prettyPrintType actual ++
            " in " ++ prettyPrintExpr expr

        EmptyList _ ->
            "Type Error: Cannot infer type of empty list"

        ArgsMismatch fname expected actual ->
            "Type Error: Function '" ++ fname ++ "' expects " ++
            show expected ++ " argument(s), but got " ++ show actual

        UnknownFunction fname ->
            "Type Error: Unknown function '" ++ fname ++ "'"

        NotFunction expr ->
            "Type Error: Expression is not a function: " ++ prettyPrintExpr expr