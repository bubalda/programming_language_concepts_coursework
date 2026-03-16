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
  | TokLBrack | TokRBrack | TokLSQBrack | TokRSQBrack | TokLCBrack | TokRCBrack               -- Scoping
  | TokNot | TokAnd | TokOr                                                                   -- Logical Op
  | TokEQ | TokNEQ | TokLTE | TokLT | TokGTE | TokGT                                          -- Comparison Op
  | TokFloorDiv | TokPower | TokPlus | TokMinus | TokMultiply | TokDivision | TokModulo       -- Arithmetic Op
  | TokBinAND | TokBinOR | TokBinXOR | TokBinLShift | TokBinRShift                            -- Binary Arithmetic Op
  deriving (Show, Eq)


-- Token formatter
formatToken :: Token -> String
formatToken (Token t (TokenPos l c)) = show t ++ "    (at line " ++ show l ++ " column " ++ show c ++ ")"