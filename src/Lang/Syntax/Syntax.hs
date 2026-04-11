module Lang.Syntax.Syntax where

data Type 
    = TBool
    | TInt
    | TFloat
    | TDouble
    | TChar
    | TString
    | TList Type
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

-- Helper func (already exists in op.hs just add first)
operatorSymbol :: TwoExprOperator -> String
operatorSymbol op =
  case op of
    Add -> "+"
    Sub -> "-"
    Mul -> "*"
    Div -> "/"
    Mod -> "%"
    BitAnd -> "&"
    BitOr -> "|"
    BitXor -> "^"
    BitLShift -> "<<"
    BitRShift -> ">>"
    Eq -> "=="
    Neq -> "!="
    Lte -> "<="
    Lt -> "<"
    Gte -> ">="
    Gt -> ">"
    And -> "&&"
    Or -> "||"