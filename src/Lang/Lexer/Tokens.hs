module Lang.Lexer.Tokens
  ( TokenPos (..),
    TokenSpan (..),
    TokenType (..),
  )
where

data TokenPos = TokenPos
  { offset :: Int,
    line :: Int,
    column :: Int
  }
  deriving (Show, Eq)
data TokenSpan = TokenSpan
  { start :: TokenPos,
    end :: TokenPos
  }
  deriving (Show, Eq)

data TokenType
  = TokInt Int TokenSpan
  | TokIdent String TokenSpan
  | TokTrue TokenSpan
  | TokFalse TokenSpan
  | TokLT TokenSpan
  | TokLTE TokenSpan
  | TokGT TokenSpan
  | TokGTE TokenSpan
  | TokEQ TokenSpan
  | TokNEQ TokenSpan
  | TokPlus TokenSpan
  | TokMinus TokenSpan
  | TokMultiply TokenSpan
  | TokDivision TokenSpan
  | TokFloorDiv TokenSpan
  | TokModulo TokenSpan
  | TokAssign TokenSpan
  | TokEscape TokenSpan
  | TokDot TokenSpan
  | TokComma TokenSpan
  | TokColon TokenSpan
  | TokSemiColon TokenSpan
  | TokLParen TokenSpan
  | TokRParen TokenSpan
  | TokLBrack TokenSpan
  | TokRBrack TokenSpan
  | TokLSQBrack TokenSpan
  | TokRSQBrack TokenSpan
  | TokVar TokenSpan
  | TokIf TokenSpan
  | TokElse TokenSpan
  | TokFun TokenSpan
  | TokError String TokenSpan -- Handled by lexer
  | TokEOF -- End of program
  deriving (Show, Eq)
