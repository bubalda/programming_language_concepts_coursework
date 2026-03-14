{
module Lang.Lexer.Lexer (runLexer) where
import Lang.Lexer.Tokens (TokenType (..), TokenPos (..), Token (..))
}

-- https://haskell-alex.readthedocs.io/en/latest/api.html#the-monad-wrapper
%wrapper "monad"

-- https://haskell-alex.readthedocs.io/en/latest/syntax.html#lexical-syntax
$digit      = [0-9]
$alpha      = [a-zA-Z]

-- Token matches by (Top-Down) (Long-Short)
tokens :-
  -- Ignore
  $white+                        ; -- As long there is one separating between tokens
  "///"[^\n]*                    ; -- Normal comments, I already wanted to do this a long time ago

  -- Literals
  $digit+                        { intTokenize TokInt }

  -- Special
  "="                            { simpleTokenize TokAssign }
  "\\"                           { simpleTokenize TokEscape }
  "!"                            { simpleTokenize TokNot }
  "."                            { simpleTokenize TokDot }
  ","                            { simpleTokenize TokComma }
  ":"                            { simpleTokenize TokColon }
  ";"                            { simpleTokenize TokSemiColon }

  -- Brackets
  "("                            { simpleTokenize TokLParen }
  ")"                            { simpleTokenize TokRParen }
  "["                            { simpleTokenize TokLBrack }
  "]"                            { simpleTokenize TokRBrack }
  "{"                            { simpleTokenize TokLSQBrack }
  "}"                            { simpleTokenize TokRSQBrack }

  -- Logical Operators
  "=="                           { simpleTokenize TokEQ }
  "!="                           { simpleTokenize TokNEQ }
  "<="                           { simpleTokenize TokLTE }
  "<"                            { simpleTokenize TokLT }
  ">="                           { simpleTokenize TokGTE }
  ">"                            { simpleTokenize TokGT }

  -- Arithmetic Operators
  "+"                            { simpleTokenize TokPlus }
  "-"                            { simpleTokenize TokMinus }
  "*"                            { simpleTokenize TokMultiply }
  "//"                           { simpleTokenize TokFloorDiv }
  "/"                            { simpleTokenize TokDivision }
  "%"                            { simpleTokenize TokModulo }

  -- Identifier / Keywords (check identTokenize)
  $alpha [$alpha $digit _]*      { identTokenize }

  -- Catch-all Error
  .                              { stringTokenize TokError }

{
-- Tokenize Keywords and Identifier
identTokenize :: AlexInput -> Int -> Alex Token
identTokenize inp@(_, _, _, str) len = stringTokenize (\_ -> identifier (take len str)) inp len
  where
    identifier :: String -> TokenType
    identifier s =
      case s of
        "var"   -> TokVar
        "if"    -> TokIf
        "else"  -> TokElse
        "fun"   -> TokFun
        "true"  -> TokTrue
        "false" -> TokFalse
        s       -> TokIdent s

-- End of program
alexEOF :: Alex Token
alexEOF = return $ Token TokEOF (TokenPos 0 0)

-- Tokenizer
getTokenPos :: AlexPosn -> TokenPos
getTokenPos (AlexPn _ l c) = TokenPos l c

-- Normal tokenize, parses value as string
tokenize :: (String -> TokenType) -> AlexInput -> Int -> Alex Token
tokenize f (pos, _, _, str) len = pure $ Token (f (take len str)) (getTokenPos pos)

-- For simple tokens without values (\_ p helps drop String value provided by tokenize)
simpleTokenize :: TokenType -> AlexInput -> Int -> Alex Token
simpleTokenize tt = tokenize (\_ -> tt)

-- Alias for tokenize, for readability
stringTokenize :: (String -> TokenType) -> AlexInput -> Int -> Alex Token
stringTokenize = tokenize

-- Returns value as an Int (enforced by type signature)
intTokenize :: (Int -> TokenType) -> AlexInput -> Int -> Alex Token
intTokenize tt = tokenize (\val -> tt (read val))

-- REPL
-- Generate tokens for parser / debug printer
runLexer :: String -> Either String [Token]
runLexer input = runAlex input scanTokens
  where
    scanTokens :: Alex [Token]
    scanTokens = go
      where
        go = do
          tok <- alexMonadScan
          case (tokenType tok) of
            TokEOF -> return [tok]
            _      -> do
              rest <- go
              return (tok : rest)
}