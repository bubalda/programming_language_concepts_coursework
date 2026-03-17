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
  = TokEOF                                                                                    -- End of program
  | TokError String                                                                           -- Error handled by lexer
  | TokIdent String                                                                           -- Variable names
  | TokType String                                                                            -- Static type declaration
  | TokInt Int | TokChar Char | TokBool Bool | TokDouble Double | TokString String | TokNull  -- Value itself
  | TokAssign | TokEscape | TokDot | TokComma | TokColon | TokSemiColon                       -- Special characters
  | TokIf | TokElse                                                                           -- Conditional
  | TokFor | TokWhile                                                                         -- Loops
  | TokFunc                                                                                   -- Abstractions
  | TokLBrack | TokRBrack | TokLSqBrack | TokRSqBrack | TokLCBrack | TokRCBrack               -- Scoping
  | TokNot | TokAnd | TokOr                                                                   -- Logical Op
  | TokEq | TokNeq | TokLte | TokLt | TokGte | TokGt                                          -- Comparison Op
  | TokFloorDiv | TokPow | TokAdd | TokSub | TokMul | TokDiv | TokMod       -- Arithmetic Op
  | TokBinAnd | TokBinOr | TokBinXor | TokBinLShift | TokBinRShift                            -- Binary Arithmetic Op

  -- Assignment shortcuts
  | TokFloorDivAssign | TokPowAssign | TokAddAssign | TokSubAssign | TokMulAssign | TokDivAssign | TokModAssign | TokBinAndAssign | TokBinOrAssign | TokBinXorAssign | TokBinLShiftAssign | TokBinRShiftAssign
  deriving (Show, Eq)


-- Token formatter
formatToken :: Token -> String
formatToken (Token t (TokenPos l c)) = show t ++ "    (at line " ++ show l ++ " column " ++ show c ++ ")"