module Lang.Syntax.Syntax where
import Data.Maybe (catMaybes)

data Type 
    = TBool
    | TInt
    | TFloat
    | TDouble
    | TChar
    | TString
    | TList Type
    | TNull
    deriving (Show, Eq)

data Expr
    -- Variables and Literals
    = Var String -- x
    | Let String Expr Expr -- let x = 4 in x * x, in scope
    | IntLit Int -- 16
    | BoolLit Bool -- false
    | CharLit Char -- 'c'
    | FloatLit Float -- Type casted
    | DoubleLit Double -- Dynamic reading defaults all numbers with decimal points to double 1.32
    | StringLit String -- "Hello World"
    | ListLit [Expr] -- [a]
    | NullLit -- null

    -- Binary Operations
    | BinOp TwoExprOperator Expr Expr -- a () b operators, where a and b are variables

    -- Unary Operations
    | Negate Expr -- -x
    | Not Expr -- !true

    -- Function
    | Call Expr [Expr] -- Call "function_name" [params]

    -- List Operations
    | ListIndex Expr Expr -- list[i]
    | ListSlice Expr Slice -- list[start:stop:step]
    | ListRange Expr Expr -- [1..100]
    deriving (Show, Eq)


data TwoExprOperator
    -- Arithmetic Operators
    = Add
    | Sub
    | Mul
    | Div
    | Mod

    -- Bitwise Operators
    | BitAnd
    | BitOr
    | BitXor
    | BitLShift
    | BitRShift

    -- Comparison Operators
    | Eq
    | Neq
    | Lte
    | Lt
    | Gte
    | Gt

    -- Logical Operators
    | And
    | Or
    deriving (Show, Eq)


-- Assignment Operators
data AssignOperator 
    = AddEq 
    | SubEq
    | MulEq
    | DivEq
    | ModEq
    | BitAndEq
    | BitOrEq
    | BitXorEq
    | BitLShiftEq
    | BitRShiftEq
    deriving(Show, Eq)

data Slice = Slice (Maybe Expr) (Maybe Expr) (Maybe Expr)
  deriving (Show, Eq)

data Stmt
    = Assign String Expr -- x = 10
    | ExprStmt Expr -- x
    | AssignOp AssignOperator String Expr -- x += 10
    | If Expr Stmt (Maybe Stmt) -- if (cond) then r = 2; else r = 3
    | Decl Type String Expr -- double x = 10
    deriving (Show, Eq)

isAssignment :: Stmt -> Bool
isAssignment Assign{} = True
isAssignment AssignOp{} = True
isAssignment Decl{} = True
isAssignment _ = False

-- Type safe functions
data Function 
    -- Core Functions
    = FAbs | FCeil | FFloor | FRound | FSign 

    -- Power/Root
    | FSqrt | FCbrt | FPow | FExp | FSquare | FCube | FExp10

    -- Trigonometry
    | FSin | FCos | FTan | FAsin | FAcos | FAtan | FAtan2 
    | FSec | FCsc | FCot | FVersin | FExsec

    -- Log
    | FLn | FLog10 | FLog2 | FLog | FLog1p

    -- Combinatorics
    | FFact | FFact2 | FComb | FPerm | FFib | FGamma | FGcd | FLcm 

    -- Stats 
    | FMean | FMedian | FMode | FSum | FProduct | FMin | FMax | FStddev

    -- Advanced Maths
    | FSinh | FCosh | FTanh | FAcosh | FAtanh | FCsch | FCoth

    -- Utility
    | FLength
    deriving (Show, Eq)  

-- Operator hierarchy with all 7 categories (Function metadata used by typechecker)
data FuncCategory
    = FCCore
    | FCPowerRoot
    | FCTrig
    | FCLog 
    | FCCombinatorials
    | FCStats
    | FCAdvancedMaths

functionCategory :: Function -> FuncCategory
functionCategory func =
    case func of
        FAbs -> FCCore
        FCeil -> FCCore
        FFloor -> FCCore
        FRound -> FCCore
        FSign -> FCCore

        FSqrt -> FCPowerRoot
        FCbrt -> FCPowerRoot
        FPow -> FCPowerRoot
        FExp -> FCPowerRoot
        FSquare -> FCPowerRoot
        FCube -> FCPowerRoot
        FExp10 -> FCPowerRoot

        FSin -> FCTrig
        FCos -> FCTrig
        FTan -> FCTrig
        FAsin -> FCTrig
        FAcos -> FCTrig
        FAtan -> FCTrig
        FAtan2 -> FCTrig
        FSec -> FCTrig
        FCsc -> FCTrig
        FCot -> FCTrig
        FVersin -> FCTrig
        FExsec -> FCTrig

        FLn -> FCLog
        FLog10 -> FCLog
        FLog2 -> FCLog
        FLog -> FCLog
        FLog1p -> FCLog

        FFact -> FCCombinatorials
        FFact2 -> FCCombinatorials
        FComb -> FCCombinatorials
        FPerm -> FCCombinatorials
        FFib -> FCCombinatorials
        FGamma -> FCCombinatorials
        FGcd -> FCCombinatorials
        FLcm -> FCCombinatorials

        FMean -> FCStats
        FMedian -> FCStats
        FMode -> FCStats
        FSum -> FCStats
        FProduct -> FCStats
        FMin -> FCStats
        FMax -> FCStats
        FStddev -> FCStats

        FSinh -> FCAdvancedMaths
        FCosh -> FCAdvancedMaths
        FTanh -> FCAdvancedMaths
        FAcosh -> FCAdvancedMaths
        FAtanh -> FCAdvancedMaths
        FCsch -> FCAdvancedMaths
        FCoth -> FCAdvancedMaths

        FLength -> FCCore

-- AST helper for TypeChecker
isAtomic :: Expr -> Bool
-- {} records wildcard
isAtomic Var{} = True
isAtomic IntLit{} = True
isAtomic BoolLit{} = True
isAtomic CharLit{} = True
isAtomic FloatLit{} = True
isAtomic DoubleLit{} = True
isAtomic StringLit{} = True
isAtomic ListLit{} = True
isAtomic _ = False

subExpr :: Expr -> [Expr]
subExpr (Let _ e1 e2) = [e1, e2]
subExpr (Negate e)    = [e]
subExpr (Not e)       = [e]
subExpr (BinOp _ e1 e2) = [e1, e2]
subExpr (Call f args) = f:args
subExpr (ListIndex a b) = [a, b]
subExpr (ListSlice e (Slice s1 s2 s3)) =
    e : catMaybes [s1,s2,s3]
subExpr (ListRange e1 e2) = [e1, e2]
subExpr (ListLit es) = es -- ListLit [IntLit 1, IntLit 2, IntLit 3]
subExpr _ = []