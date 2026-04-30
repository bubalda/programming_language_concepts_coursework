-- |
-- This module defines the syntax tree that is utilized by the parser, type checker and evaluator
-- It represents all the syntactic constructs in our C2 language
--
-- The design contains & emphasizes on:

-- * Primitive and compound types
-- * Expressions (Expr)
-- * Statements (Stmt)
-- * Operators
-- * Function metadata
-- * Utility mappings for built-in functions

-- * Type Safety: Built-in functions that are represented as a sum type, allowing compile-time exhaustiveness checking
-- * Extensibility: New functions, types and operators can be added without the need to modify existing code logic
-- * Gradual Typing: Both statically-typed declarations and dynamic variables are supported
--
module Lang.Syntax.Syntax where
import Data.Char ( toLower ) 
import qualified Data.Map as Map

-- | Represents all the supported language-level 
-- These types are used by the type checker to validate expressions and declarations

-- The type system allows:
-- * Numeric types with automatic promotion
-- * Parametric lists via 'TList'
--
data Type 
    = TBool       -- ^ Boolean type (@True@, @False@)
    | TInt        -- ^ 32-bit signed integer
    | TFloat      -- ^ Single-precision floating point
    | TDouble     -- ^ Double-precision floating point
    | TChar       -- ^ Unicode character
    | TString     -- ^ UTF-8 string
    | TList Type  -- ^ Homogeneous list of elements
    | TNull       -- ^ Null type
    | TDynamic    -- ^ Dynamic type
    deriving (Show, Eq, Read)

-- | Expression represents all computable values
-- This includes variables, literals, operations, function calls and list operations
data Expr
    -- | Variables and Literals
    = Var String -- ^ x
    | Let String Expr Expr -- ^ let x = 4 in x * x, in scope
    | IntLit Int -- ^ 16
    | BoolLit Bool -- ^ false
    | CharLit Char -- ^ 'c'
    | FloatLit Float -- ^ Type casted
    | DoubleLit Double -- ^ Dynamic reading defaults all numbers with decimal points to double 1.32
    | StringLit String -- ^ "Hello World"
    | ListLit [Expr] -- ^ [a]
    | NullLit -- ^ null

    -- | Binary Operations
    | BinOp TwoExprOperator Expr Expr -- ^ a () b operators, where a and b are variables

    -- | Unary Operations
    | Negate Expr -- ^ -x
    | Not Expr -- ^ !true

    -- | Function
    | Call Expr [Expr] -- ^ Call "function_name" [params]

    -- | List Operations
    | ListIndex Expr Expr -- ^ list[i]
    | ListSlice Expr Slice -- ^ list[start:stop:step]
    | ListRange Expr Expr -- ^ [1..100]
    deriving (Show, Eq)

-- | This represents binary operators between two expressions
-- They are grouped by category: Arithmetic, bitwise, comparison and logical operators
data TwoExprOperator
    -- | Arithmetic Operators
    = Add    
    | Sub
    | Mul
    | Div
    | Mod

    -- | Bitwise Operators
    | BitAnd
    | BitOr
    | BitXor
    | BitLShift
    | BitRShift

    -- | Comparison Operators
    | Eq
    | Neq
    | Lte
    | Lt
    | Gte
    | Gt

    -- | Logical Operators
    | And
    | Or
    deriving (Show, Eq)


-- | Assignment operators are used to combine an operation with assignment
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
    = Assign String Expr -- ^ x = 10
    | ExprStmt Expr -- ^ x
    | AssignOp AssignOperator String Expr -- ^ x += 10
    | Block [Stmt] -- ^ { x += 1; x += 2; }
    | If Expr Stmt (Maybe Stmt) -- ^ if (cond) then r = 2; else r = 3
    | Decl Type String Expr -- ^ double x = 10
    deriving (Show, Eq)

-- | Built-in mathematical functions across 7 categories
-- Used for:
-- * Type checking
-- * Arity checking
-- * Return type inference
-- * Function dispatch
data Function 
    -- | Category 1: Core Mathematic Functions
    = FAbs | FCeil | FFloor | FRound | FSign 

    -- | Category 2: Power and Roots
    | FSqrt | FCbrt | FPow | FExp | FSquare | FCube | FExp10

    -- | Category 3: Trigonometry
    | FSin | FCos | FTan | FAsin | FAcos | FAtan | FAtan2 
    | FSec | FCsc | FCot | FVersin | FExsec 

    -- | Category 4: Logarithmics
    | FLn | FLog10 | FLog2 | FLog | FLog1p

    -- | Category 5: Combinatorics
    | FFact | FFact2 | FComb | FPerm | FFib | FGamma | FGcd | FLcm 

    -- | Category 6: Statistical
    | FMean | FMedian | FMode | FSum | FProduct | FMin | FMax | FStddev

    -- | Category 7: Hyperbolic and Advanced Mathematical Functions
    | FSinh | FCosh | FTanh | FAcosh | FAtanh | FCsch | FCoth | FAsinh | FSech


    -- | Utility
    | FLength
    deriving (Show, Eq)  

-- | functionArgs returns the required number of arguments for a function
-- It is used by the type checker to validate function calls
-- Most functions only take 1 argument
-- Functions such as 'FPow', 'FAtan2', 'FLog', 'FComb', 'FPerm', 'FGcd', and 'FLcm' take 2 arguments.
functionArgs :: Function -> Int
functionArgs func =
    case func of 
        FAbs     -> 1
        FCeil    -> 1
        FFloor   -> 1
        FRound   -> 1
        FSign    -> 1

        FSqrt    -> 1
        FCbrt    -> 1
        FPow     -> 2
        FExp     -> 1
        FSquare  -> 1
        FCube    -> 1
        FExp10   -> 1

        FSin     -> 1
        FCos     -> 1
        FTan     -> 1
        FAsin    -> 1
        FAcos    -> 1
        FAtan    -> 1
        FAtan2   -> 2
        FSec     -> 1
        FCsc     -> 1
        FCot     -> 1
        FVersin  -> 1
        FExsec   -> 1

        FLn      -> 1
        FLog10   -> 1
        FLog2    -> 1
        FLog     -> 2
        FLog1p   -> 1

        FFact    -> 1
        FFact2   -> 1
        FComb    -> 2
        FPerm    -> 2
        FFib     -> 1
        FGamma   -> 1
        FGcd     -> 2
        FLcm     -> 2

        FMean    -> 1
        FMedian  -> 1
        FMode    -> 1
        FSum     -> 1
        FProduct -> 1
        FMin     -> 1
        FMax     -> 1
        FStddev  -> 1

        FSinh    -> 1
        FCosh    -> 1
        FTanh    -> 1
        FAcosh   -> 1
        FAtanh   -> 1
        FCsch    -> 1
        FCoth    -> 1
        FAsinh   -> 1
        FSech    -> 1

        FLength  -> 1

-- | Returns the output type of all the built-in functions
-- Integer based combinatorics like 'FFact' returns 'TInt'
-- Most of the mathematical functions return 'TDouble'
funcReturnType :: Function -> Type
funcReturnType func =
    case func of
        FLength -> TInt
        FFact   -> TInt
        FFact2  -> TInt
        FFib    -> TInt
        FComb   -> TInt
        FPerm   -> TInt
        FGcd    -> TInt
        FLcm    -> TInt
        _       -> TDouble

-- | Mapping between string names and internal function constructors
-- Used by 'funcConvertString' to allow for efficient lookup
funcMap :: Map.Map String Function
funcMap = Map.fromList
    [   ("abs", FAbs), ("ceil", FCeil), ("floor", FFloor), ("round", FRound), ("sign", FSign), 
        ("sqrt", FSqrt), ("cbrt", FCbrt), ("pow", FPow), ("exp", FExp),
        ("square", FSquare), ("cube", FCube), ("exp10", FExp10),
        ("sin", FSin), ("cos", FCos), ("tan", FTan),
        ("asin", FAsin), ("acos", FAcos), ("atan", FAtan), ("atan2", FAtan2),
        ("sec", FSec), ("csc", FCsc), ("cot", FCot), ("versin", FVersin), ("exsec", FExsec),
        ("ln", FLn), ("log10", FLog10), ("log2", FLog2), ("log", FLog), ("log1p", FLog1p),
        ("fact", FFact), ("fact2", FFact2), ("comb", FComb), ("perm", FPerm), ("fib", FFib),
        ("gamma", FGamma), ("gcd", FGcd), ("lcm", FLcm),
        ("mean", FMean), ("median", FMedian), ("mode", FMode), ("sum", FSum), ("product", FProduct),
        ("min", FMin), ("max", FMax), ("stddev", FStddev),
        ("sinh", FSinh), ("cosh", FCosh), ("tanh", FTanh),
        ("acosh", FAcosh), ("atanh", FAtanh), ("csch", FCsch), ("coth", FCoth),
        ("asinh", FAsinh), ("sech", FSech),
        ("length", FLength)
    ]

-- | Converts a function name string into a built-in function.
-- Lookup is also case insensitive
funcConvertString :: String -> Maybe Function 
funcConvertString s = Map.lookup (map toLower s) funcMap

