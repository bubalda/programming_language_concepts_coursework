module Lang.Lexer.Tokens
  ( TokenPos (..),
    TokenType (..),
    Token (..),
    printTokens,
    formatToken,
    formatTokenType,
  )
where

import Lang.Repl.Helper (wrapSection)
import Text.Printf (printf)

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
  | TokInt Int
  | TokTrue
  | TokFalse
  | TokLT
  | TokLTE
  | TokGT
  | TokGTE
  | TokEQ
  | TokNEQ
  | TokPlus
  | TokMinus
  | TokMultiply
  | TokDivision
  | TokFloorDiv
  | TokModulo
  | TokAssign
  | TokEscape
  | TokNot
  | TokDot
  | TokComma
  | TokColon
  | TokSemiColon
  | TokLParen
  | TokRParen
  | TokLBrack
  | TokRBrack
  | TokLSQBrack
  | TokRSQBrack
  | TokVar
  | TokIf
  | TokElse
  | TokFun
  deriving (Show, Eq)

-- Token formatter
formatTokenType :: TokenType -> String
formatTokenType t = case t of
  TokIdent s -> "identifier `" ++ s ++ "`"
  TokInt n -> "integer `" ++ show n ++ "`"
  TokTrue -> "true"
  TokFalse -> "false"
  TokLT -> "<"
  TokLTE -> "<="
  TokGT -> ">"
  TokGTE -> ">="
  TokEQ -> "=="
  TokNEQ -> "!="
  TokPlus -> "+"
  TokMinus -> "-"
  TokMultiply -> "*"
  TokDivision -> "/"
  TokFloorDiv -> "//"
  TokModulo -> "%"
  TokAssign -> "="
  TokEscape -> "\\"
  TokNot -> "!"
  TokDot -> "."
  TokComma -> ","
  TokColon -> ":"
  TokSemiColon -> ";"
  TokLParen -> "("
  TokRParen -> ")"
  TokLBrack -> "["
  TokRBrack -> "]"
  TokLSQBrack -> "{"
  TokRSQBrack -> "}"
  TokVar -> "var"
  TokIf -> "if"
  TokElse -> "else"
  TokFun -> "fun"
  TokEOF -> "<EOF>"
  TokError s -> show s

-- Token formatter
formatToken :: Token -> String
formatToken (Token t (TokenPos l c)) = (printf "%-75s" (formatTokenType t)) ++ " (at line " ++ show l ++ " column " ++ show c ++ ")"

-- Debug printer
printTokens :: [Token] -> IO ()
printTokens tokens = do
  wrapSection "Tokens" (mapM_ printToken tokens)
  where
    printToken (Token TokEOF _) = return () -- Hide TokEOF
    printToken t@(Token (TokError _) _) =
      putStrLn $
        "Lexer Error: Could not tokenize string " ++ formatToken t
    printToken t = putStrLn $ "· " ++ formatToken t
