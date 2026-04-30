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
    | BranchTypeMismatch Type Type Stmt

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
        TDynamic -> "Dynamic"

prettyPrintExpr :: Expr -> String 
prettyPrintExpr (Var s) = s
prettyPrintExpr (IntLit n) = show n
prettyPrintExpr (BoolLit b) = if b then "True" else "False"
prettyPrintExpr (FloatLit f) = show f       
prettyPrintExpr (DoubleLit d) = show d

prettyPrintExpr (Negate e) = "-" ++ prettyPrintExpr e

prettyPrintExpr (Not e) = "!" ++ prettyPrintExpr e

prettyPrintExpr (ListIndex e i) = 
    prettyPrintExpr e ++ "[" ++ prettyPrintExpr i ++ "]"

prettyPrintExpr (ListRange e1 e2) =
    "[" ++ prettyPrintExpr e1 ++ ".." ++ prettyPrintExpr e2 ++ "]"

prettyPrintExpr (ListSlice e _) = prettyPrintExpr e ++ "[slice]"

prettyPrintExpr NullLit = "null"

prettyPrintExpr (CharLit c) = "'" ++ [c] ++ "'"

prettyPrintExpr (StringLit s) = "\"" ++ s ++ "\""

prettyPrintExpr (ListLit es) = "[" ++ intercalate ", " (map prettyPrintExpr es) ++ "]"

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
            "Type Error: Undefined variable '" ++ name ++ "'" ++
            "\n  Hint: Declare the variable first, e.g. '" ++ name ++ " = <value>' or 'int " ++ name ++ " = <value>'"

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
            prettyPrintType actual ++ " in " ++ prettyPrintExpr expr ++
            if actual == TDynamic
                then "\n  Hint: '" ++ prettyPrintExpr expr ++ 
                "' was declared without a type. Use 'bool " ++ prettyPrintExpr expr ++ " = ...' to declare it as boolean."
            else "\n  Hint: '" ++ prettyPrintExpr expr ++ 
                "' is of type " ++ prettyPrintType actual ++ 
                ". Did you mean to declare it as 'bool " ++ prettyPrintExpr expr ++ " = ...' instead?"

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
            "Type Error: Unknown function '" ++ fname ++ "'\n" ++
            "  Hint: Did you mean one of these?:\n" ++
            "  abs, sqrt, sin, cos, tan, exp, ln, log, pow, mean, sum, length, ...\n" ++
            "  See FUNCTIONS.md for the complete list of available functions."

        NotFunction expr ->
            "Type Error: Expression is not a function: " ++ prettyPrintExpr expr

        BranchTypeMismatch thenT elseT _ -> 
            "Type Error: 'if' branches have mismatched types: " ++
            prettyPrintType thenT ++ " vs " ++ prettyPrintType elseT
