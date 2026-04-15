{
module Lang.Lexer.Lexer (runLexer) where
import Lang.Lexer.Tokens (TokenType (..), TokenPos (..), Token (..))
import Lang.Repl.Helper (formatPos)
import Data.Char (isAscii)
import Text.Read (readMaybe)
}

-- https://haskell-alex.readthedocs.io/en/latest/api.html#the-monaduserstate-wrapper
%wrapper "monadUserState"

-- https://haskell-alex.readthedocs.io/en/latest/syntax.html#lexical-syntax
$digit       = [0-9]
$alpha       = [a-zA-Z]
$char        = [^\'\\\n]
$stringChar  = [^\"\\\n]

-- https://gdevanla.github.io/posts/wya-lexer.html#numerical_values
@digitpart     =  $digit+
@fraction      =  [\.] @digitpart
-- Floating point must have values to prevent crashing Prelude.read on values like "4."
@pointfloat    =  @digitpart @fraction | @fraction
@exponent      =  [eE] ([\+\-]?) @digitpart
@floatnumber   =  (@pointfloat @exponent?) | (@digitpart @exponent)


-- Token matches by (Top-Down) (Long-Short)
-- But still, place "==" before "=" to prevent reading two "=" instead of one "=="
-- <0> are for nested comments
tokens :-
  -- Allows nested comments
  <0>             "/*"           { startComment }   -- TryThis> if x then { /* Do something */ x = 2; } else { doElse = 1; }
  <comment>       "/*"           { nestComment }    -- TryThis> /* outside /* nested */ outside */
  <comment>       "*/"           { endComment }
  <comment>       .              { skip }
  <comment>       \n             { skip }

  -- Ignore
  <0> $white+                        ; -- As long there is one separating between tokens
  <0> "//"[^\n]*                     ; -- Normal comments

  -- Literals
  -- Remove lookahead to read sucessfully read floats at the end of line
  <0> @floatnumber                   { valueTokenizeSafe TokDouble "Error: Invalid float literal" }
  <0> $digit+                        { valueTokenize TokInt }
  <0> \'($char|\\.)\'                { valueTokenize TokChar }
  <0> \"($stringChar|\\.)*\"         { valueTokenize TokString }

  -- Assignment Operators
  <0> "+="                           { simpleTokenize TokAddAssign }
  <0> "-="                           { simpleTokenize TokSubAssign }
  <0> "*="                           { simpleTokenize TokMulAssign }
  <0> "/="                           { simpleTokenize TokDivAssign }
  <0> "%="                           { simpleTokenize TokModAssign }
  <0> "&="                           { simpleTokenize TokBitAndAssign }
  <0> "|="                           { simpleTokenize TokBitOrAssign }
  <0> "^="                           { simpleTokenize TokBitXorAssign }
  <0> "<<="                          { simpleTokenize TokBitLShiftAssign }
  <0> ">>="                          { simpleTokenize TokBitRShiftAssign }
  <0> "="                            { simpleTokenize TokAssign }

  -- List
  <0> "["                            { simpleTokenize TokLSqBrack }
  <0> "]"                            { simpleTokenize TokRSqBrack }
  <0> ","                            { simpleTokenize TokComma }
  <0> ":"                            { simpleTokenize TokColon }
  <0> ".."                           { simpleTokenize TokDotDot }

  -- Logical Operators
  <0> "!"                            { simpleTokenize TokNot }
  <0> "&&"                           { simpleTokenize TokAnd }
  <0> "||"                           { simpleTokenize TokOr }

  -- Comparison Operators
  <0> "=="                           { simpleTokenize TokEq }
  <0> "!="                           { simpleTokenize TokNeq }
  <0> "<="                           { simpleTokenize TokLte }
  <0> "<"                            { simpleTokenize TokLt }
  <0> ">="                           { simpleTokenize TokGte }
  <0> ">"                            { simpleTokenize TokGt }

  -- Arithmetic Operators
  <0> "+"                            { simpleTokenize TokAdd }
  <0> "-"                            { simpleTokenize TokSub }
  <0> "*"                            { simpleTokenize TokMul }
  <0> "/"                            { simpleTokenize TokDiv }
  <0> "%"                            { simpleTokenize TokMod }

  -- Bitwise Operators 
  <0> "&"                            { simpleTokenize TokBitAnd }
  <0> "|"                            { simpleTokenize TokBitOr }
  <0> "^"                            { simpleTokenize TokBitXor }
  <0> "<<"                           { simpleTokenize TokBitLShift }
  <0> ">>"                           { simpleTokenize TokBitRShift }

  -- Brackets
  <0> "("                            { simpleTokenize TokLBrack }
  <0> ")"                            { simpleTokenize TokRBrack }

  -- If condition
  <0> "{"                            { simpleTokenize TokLCBrack }
  <0> "}"                            { simpleTokenize TokRCBrack }

  -- End of line
  <0> ";"                            { simpleTokenize TokSemiColon }

  -- Identifier / Keywords (check identTokenize)
  <0> [_ $alpha] [$alpha $digit _]*  { identTokenize }

  -- Catch-all Error
  <0> .                              { tokenize TokError }

{
-- Tokenize words
identTokenize :: AlexInput -> Int -> Alex Token
identTokenize inp@(_, _, _, str) len = tokenize (\_ -> getToken (take len str)) inp len
  where
    -- Check for matched string using `case of` instead of using `alex lexer`
    -- to prevent incorrect matches like "sinh" => "sin" + "h" due to
    -- code arrangement errors
    getToken :: String -> TokenType
    getToken s = case s of
      -- Static type declaration
      "double"  -> TokDeclDouble
      "char"    -> TokDeclChar
      "String"  -> TokDeclString
      "float"   -> TokDeclFloat
      "int"     -> TokDeclInt
      "bool"    -> TokDeclBool

      -- Constants and Literals
      "pi"       -> TokDouble pi
      "null"     -> TokNull
      "True"     -> TokBool True
      "False"    -> TokBool False

      -- Control Structures Variables
      "if"       -> TokIf
      "then"     -> TokThen
      "else"     -> TokElse
      "let"      -> TokLet
      "in"       -> TokIn

      -- Identifier = Variable / Function
      s         -> TokIdent s

-- End of program
alexEOF :: Alex Token
alexEOF = do
  s <- alexGetUserState
  if lexerCommentDepth s > 0
    then pure $ Token (TokError "Unclosed block comments detected, did you close it using '*/'?") (TokenPos 0 0)
    else return $ Token TokEOF (TokenPos 0 0)

-- Tokenizer
getTokenPos :: AlexPosn -> TokenPos
getTokenPos (AlexPn _ l c) = TokenPos l c

-- Normal tokenize, parses value as string
tokenize :: (String -> TokenType) -> AlexInput -> Int -> Alex Token
tokenize tt (pos, _, _, str) len = pure $ Token (tt (take len str)) (getTokenPos pos)

-- For simple tokens without values (\_ p helps drop String value provided by tokenize)
simpleTokenize :: TokenType -> AlexInput -> Int -> Alex Token
simpleTokenize tt = tokenize (const tt)

-- Returns value as their respective types (enforced by type signature)
valueTokenize :: Read a => (a -> TokenType) -> AlexInput -> Int -> Alex Token
valueTokenize tt = tokenize (tt . read)

valueTokenizeSafe :: Read a => (a -> TokenType) -> String -> AlexInput -> Int -> Alex Token
valueTokenizeSafe tt errMsg (pos, _, _, str) len =
  case readMaybe (take len str) of
    Just v -> pure $ Token (tt v) (getTokenPos pos)
    Nothing -> pure $ Token (TokError errMsg) (getTokenPos pos)

-- REPL
-- Generate tokens for parser / debug printer
runLexer :: String -> Either String [Token]
runLexer input = case runAlex input scanTokens of
  Left err -> Left err
  Right toks ->
    case findTokError toks of
      Just err -> Left err
      Nothing  -> Right toks
  where
    scanTokens :: Alex [Token]
    scanTokens = go
      where
        go = do
          tok <- alexMonadScan
          case (tokenType tok) of
            TokEOF -> return [tok]
            TokError _ -> return [tok]
            _      -> do
              rest <- go
              return (tok : rest)

    findTokError :: [Token] -> Maybe String
    findTokError = goErr
      where
        goErr [] = Nothing
        goErr (Token (TokError s) pos : _) =
          Just $ formatPosMsg pos s
        goErr (_:xs) = goErr xs

    formatPosMsg (TokenPos l c) s =
      formatPos l c ++ formatUnexpectedToken s

formatUnexpectedToken :: String -> String
formatUnexpectedToken s
  | any (not . isAscii) s =
      "<LEXER ERROR> -- Unexpected non-ASCII character "
        ++ show s
        ++ ". Please use supported ASCII syntax only."
  | otherwise =
      "<LEXER ERROR> -- Unexpected character "
        ++ show s
        ++ "."

-- For nested comments
data AlexUserState = AlexUserState
  { lexerCommentDepth  :: Int }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState
  { lexerCommentDepth  = 0 }

-- Called on first "/*" comment block
startComment :: AlexInput -> Int -> Alex Token
startComment _ _ = do
    -- Reset/Increment depth in UserState
    s <- alexGetUserState
    alexSetUserState (s { lexerCommentDepth = 1 })
    alexSetStartCode comment
    alexMonadScan

-- Nest a new comment inside comment
nestComment :: AlexInput -> Int -> Alex Token
nestComment _ _ = do
    s <- alexGetUserState
    alexSetUserState (s { lexerCommentDepth = lexerCommentDepth s + 1 })
    alexMonadScan

-- End one level of comment block using "*/"
endComment :: AlexInput -> Int -> Alex Token
endComment _ _ = do
    s <- alexGetUserState
    let newDepth = lexerCommentDepth s - 1
    alexSetUserState (s { lexerCommentDepth = newDepth })
    if newDepth == 0 
      then alexSetStartCode 0 
      else alexSetStartCode comment
    alexMonadScan -- looks for the next token after end comment
}
