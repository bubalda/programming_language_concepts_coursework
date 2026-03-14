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
  = TokEOF -- End of program
  | TokError String -- Handled by lexer
  | TokIdent String
  | TokInt Int | TokTrue | TokFalse | TokNull
  | TokAssign | TokEscape | TokDot | TokComma | TokColon | TokSemiColon
  | TokLBrack | TokRBrack | TokLSQBrack | TokRSQBrack | TokLCBrack | TokRCBrack
  | TokNot | TokAnd | TokOr
  | TokEQ | TokNEQ | TokLTE | TokLT | TokGTE | TokGT
  | TokFloorDiv | TokPower | TokPlus | TokMinus | TokMultiply | TokDivision | TokModulo 
  | TokBinAnd | TokBinOr | TokBinXOR
  | TokVar
  | TokIf
  | TokElse
  | TokFunc
  deriving (Show, Eq)


-- Token formatter
formatToken :: Token -> String
formatToken (Token t (TokenPos l c)) = show t ++ " (at line " ++ show l ++ " column " ++ show c ++ ")"
