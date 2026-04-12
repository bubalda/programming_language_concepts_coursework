module Lang.TypeChecker.Types where

import Lang.Syntax.Syntax
import qualified Data.Map as Map

type TypeEnv = Map.Map String Type

data TypeError
    = UndefinedVariable String
    | ExpectedNumeric { actual :: Type, expr :: Expr }
    | ExpectedTypeBool { actual :: Type, expr :: Expr }
    | ExpectedTypeInt { actual :: Type, expr :: Expr }
    | ExpectedList { actual :: Type, expr :: Expr }
    | TypeMismatch { leftType :: Type , rightType :: Type, expr :: Expr }
    | EmptyList Expr
    | UndefinedFunction String
    | WrongArity String Int Int
    deriving (Show)