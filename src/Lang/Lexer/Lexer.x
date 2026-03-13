{
module Lang.Lexer.Lexer where
import Lang.Lexer.Tokens
import Lang.Repl.Helper (wrapSection)
}

-- Monads with {nested comments, indentation level and import lists}
-- https://haskell-alex.readthedocs.io/en/latest/api.html#the-monaduserstate-wrapper
%wrapper "monadUserState"

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
  "true"                         { simpleTokenize TokTrue }
  "false"                        { simpleTokenize TokFalse }

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

  -- Special
  "\\"                           { simpleTokenize TokEscape }
  "="                            { simpleTokenize TokAssign }
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

  -- Keyword
  "var"                          { simpleTokenize TokVar }
  "if"                           { simpleTokenize TokIf }
  "else"                         { simpleTokenize TokElse }
  "fun"                          { simpleTokenize TokFun }

  -- Identifier
  $alpha [$alpha $digit _]*      { stringTokenize TokIdent }

  -- Catch-all Error
  .                              { stringTokenize TokError }

{
-- End of program
alexEOF :: Alex TokenType
alexEOF = return TokEOF

-- TODO Touch this on later stages
data AlexUserState = AlexUserState
  { lexerCommentDepth  :: Int
  , lexerStringValue   :: String
  }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
  { lexerCommentDepth  = 0
  , lexerStringValue   = ""
  }

-- Tokenizer
-- For error feedback and slicing purposes
getTokenSpan :: AlexInput -> Int -> TokenSpan
getTokenSpan (AlexPn o l c, _, _, _) len = TokenSpan (TokenPos o l c) (TokenPos (o + len) l (c + len))

-- Normal tokenize, parses value as string
tokenize :: (String -> TokenSpan -> TokenType) -> AlexInput -> Int -> Alex TokenType
tokenize f inp@(_, _, _, str) len = pure $ f (take len str) (getTokenSpan inp len)

-- For simple tokens without values (\_ p helps drop String value provided by tokenize)
simpleTokenize :: (TokenSpan -> TokenType) -> AlexInput -> Int -> Alex TokenType
simpleTokenize f = tokenize (\_ p -> f p)

-- Alias for tokenize, for readability
stringTokenize :: (String -> TokenSpan -> TokenType) -> AlexInput -> Int -> Alex TokenType
stringTokenize = tokenize

-- Returns value as an Int (enforced by type signature)
intTokenize :: (Int -> TokenSpan -> TokenType) -> AlexInput -> Int -> Alex TokenType
intTokenize f = tokenize (\s p -> f (read s) p)

-- REPL
-- Generate tokens for parser / debug printer
runLexer :: String -> Either String [TokenType]
runLexer input = runAlex input scanTokens
  where
    scanTokens :: Alex [TokenType]
    scanTokens = go
      where
        go = do
          tok <- alexMonadScan
          case tok of
            TokEOF -> return [TokEOF]
            _      -> do
              rest <- go
              return (tok : rest)

-- A debug printer
printTokens :: [TokenType] -> IO ()
printTokens tokens = do
  wrapSection "Tokens" (mapM_ printToken tokens)
  where
    printToken TokEOF = return () -- Hide TokEOF
    printToken (TokError s (TokenSpan (TokenPos _ l1 c1) _)) = putStrLn $ "Lexer Error: Could not tokenize string " ++ (show s) ++ " at line " ++ (show l1) ++ " column " ++ (show c1)
    printToken tok = putStrLn $ "· " ++ (show tok)
}

-- TODO try to remove the need of taking str, do slicing instead
-- Multiblock comments?

-- getLexerCommentDepth :: Alex Int
-- getLexerCommentDepth = lexerCommentDepth <$> alexGetUserState

-- setLexerCommentDepth :: Int -> Alex ()
-- setLexerCommentDepth ss = do
--   ust <- alexGetUserState
--   alexSetUserState ust{ lexerCommentDepth = ss }

-- getLexerStringValue :: Alex String
-- getLexerStringValue = lexerStringValue <$> alexGetUserState

-- setLexerStringValue :: String -> Alex ()
-- setLexerStringValue ss = do
--   ust <- alexGetUserState
--   alexSetUserState ust{ lexerStringValue = ss }

-- addCharToLexerStringValue :: Char -> Alex ()
-- addCharToLexerStringValue c = do
--   ust <- alexGetUserState
--   alexSetUserState ust{ lexerStringValue = c : lexerStringValue ust } -- Append?

-- resetLexerStringValue?