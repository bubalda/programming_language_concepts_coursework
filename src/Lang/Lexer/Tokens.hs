module Lang.Lexer.Tokens
  ( TokenPos (..),
    TokenType (..),
    Token (..),
    formatToken
  )
where

data Token = Token
  { tokenType :: TokenType, -- For differenciating tokens
    tokenPos :: TokenPos -- For error feedback
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
  | TokType String    -- Static type declaration

  -- Values
  | TokInt Int | TokChar Char | TokBool Bool | TokNull
  | TokDouble Double | TokString String

  -- Special characters
  | TokAssign | TokEscape | TokDot | TokComma
  | TokSemiColon | TokColon | TokQuestion

  -- Conditional
  | TokIf | TokElse

  -- Loops
  | TokFor | TokWhile                                                                         

  -- Abstractions
  | TokFunc                                                                    

  -- Scoping               
  | TokLBrack | TokRBrack | TokLSqBrack | TokRSqBrack | TokLCBrack | TokRCBrack

  -- Logical Op
  | TokExclamation | TokAnd | TokOr                                                                   

  -- Comparison Op
  | TokEq | TokNeq | TokLte | TokLt | TokGte | TokGt                                          

  -- Arithmetic Op
  | TokFloorDiv | TokPow | TokAdd | TokSub | TokMul | TokDiv | TokMod

  -- Binary Arithmetic Op
  | TokBinAnd | TokBinOr | TokBinXor | TokBinLShift | TokBinRShift                            

  -- Operation Assignments
  | TokFloorDivAssign | TokPowAssign | TokAddAssign | TokSubAssign | TokMulAssign | TokDivAssign | TokModAssign 
  | TokBinAndAssign | TokBinOrAssign | TokBinXorAssign | TokBinLShiftAssign | TokBinRShiftAssign
  deriving (Show, Eq)


-- Token formatter
formatToken :: Token -> String
formatToken (Token t (TokenPos l c)) = show t ++ "    (at line " ++ show l ++ " column " ++ show c ++ ")"