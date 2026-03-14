{
module Lang.Lexer.Lexer (runLexer, printTokens) where
import Lang.Lexer.Tokens (TokenType (..), TokenPos (..), Token (..), formatToken)
import Lang.Repl.Helper (wrapSection)
import Text.Printf (printf)
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
  "."                            { simpleTokenize TokDot }
  ","                            { simpleTokenize TokComma }
  ":"                            { simpleTokenize TokColon }    -- Note that repl commands also use : so add sth before that to test it
  ";"                            { simpleTokenize TokSemiColon }

  -- Brackets
  "("                            { simpleTokenize TokLBrack }
  ")"                            { simpleTokenize TokRBrack }
  "["                            { simpleTokenize TokLSQBrack }
  "]"                            { simpleTokenize TokRSQBrack }
  "{"                            { simpleTokenize TokLCBrack }
  "}"                            { simpleTokenize TokRCBrack }

  -- Logical Operators
  "!"                            { simpleTokenize TokNot }
  "&&"                           { simpleTokenize TokAnd }
  "||"                           { simpleTokenize TokOr }

  -- Comparison Operators
  "=="                           { simpleTokenize TokEQ }
  "!="                           { simpleTokenize TokNEQ }
  "<="                           { simpleTokenize TokLTE }
  "<"                            { simpleTokenize TokLT }
  ">="                           { simpleTokenize TokGTE }
  ">"                            { simpleTokenize TokGT }

  -- Arithmetic Operators
  "//"                           { simpleTokenize TokFloorDiv }
  "**"                           { simpleTokenize TokPower }
  "+"                            { simpleTokenize TokPlus }
  "-"                            { simpleTokenize TokMinus }
  "*"                            { simpleTokenize TokMultiply }
  "/"                            { simpleTokenize TokDivision }
  "%"                            { simpleTokenize TokModulo }

  -- Binary Operators 
  "&"                            { simpleTokenize TokBinAnd }
  "|"                            { simpleTokenize TokBinOr }
  "^"                            { simpleTokenize TokBinXOR }

  -- Identifier / Keywords (check identTokenize)
  [_ $alpha] [$alpha $digit _]*      { identTokenize }

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
        "true"  -> TokTrue
        "false" -> TokFalse
        "null"  -> TokNull
        "var"   -> TokVar
        "if"    -> TokIf
        "else"  -> TokElse
        "func"  -> TokFunc
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

-- Debug printer
printTokens :: [Token] -> IO ()
printTokens tokens = do
  wrapSection "Tokens" (mapM_ printToken tokens)
  where
    printToken (Token TokEOF _) = return () -- Hide TokEOF
    printToken t@(Token (TokError _) _) = putStrLn $ "Lexer Error: Could not tokenize string " ++ formatToken t
    printToken (Token t _) = putStrLn $ show t
}

-- concat, += -= *=
-- string quotes '' "" ``