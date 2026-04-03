module Lang.Lexer.Tokens
  ( TokenPos (..),
    TokenType (..),
    Token (..),
  )
where

data Token = Token
  { tokenType :: TokenType, -- For differenciating tokens
    tokenPos :: TokenPos    -- For error feedback
  }
  deriving (Show, Eq)

data TokenPos = TokenPos
  { line :: !Int,
    column :: !Int
  }
  deriving (Show, Eq)

data TokenType
  = TokEOF            -- End of program
  | TokError String   -- Error handled by lexer
  | TokIdent String   -- Variable names
  
  -- Static type declaration
  | TokDeclBool
  | TokDeclDouble | TokDeclFloat | TokDeclInt
  | TokDeclChar | TokDeclString 

  -- Values
  | TokBool Bool | TokNull
  | TokInt Int | TokFloat Float | TokDouble Double
  | TokChar Char | TokString String

  -- List
  | TokLSqBrack | TokRSqBrack
  | TokComma | TokColon | TokDotDot
  
  -- End of line
  | TokSemiColon

  -- Control Structures Variables
  | TokIf | TokThen | TokElse | TokLCBrack | TokRCBrack
  | TokLet | TokIn

  -- Scoping               
  | TokLBrack | TokRBrack 

  -- Logical Op
  | TokNot | TokAnd | TokOr                                                                   

  -- Comparison Op
  | TokEq | TokNeq | TokLte | TokLt | TokGte | TokGt                                          

  -- Arithmetic Op
  | TokAdd | TokSub | TokMul | TokDiv | TokMod

  -- Binary Arithmetic Op
  | TokBitAnd | TokBitOr | TokBitXor | TokBitLShift | TokBitRShift                            

  -- Operation Assignments
  | TokAssign
  | TokAddAssign | TokSubAssign | TokMulAssign | TokDivAssign | TokModAssign 
  | TokBitAndAssign | TokBitOrAssign | TokBitXorAssign | TokBitLShiftAssign | TokBitRShiftAssign
  deriving (Show, Eq)