{
module Lang.Lexer.Lexer (runLexer) where
import Lang.Lexer.Tokens (TokenType (..), TokenPos (..), Token (..))
import Lang.Repl.Helper (formatPos)
}

-- https://haskell-alex.readthedocs.io/en/latest/api.html#the-monaduserstate-wrapper
%wrapper "monadUserState"

-- https://haskell-alex.readthedocs.io/en/latest/syntax.html#lexical-syntax
$digit       = [0-9]
$alpha       = [a-zA-Z]
$char        = [^\'\\\n]
$stringChar  = [^\"\\\n]

-- https://gdevanla.github.io/posts/wya-lexer.html#numerical_values
@digitpart     =  $digit([_]|$digit)*
@fraction      =  [\.] @digitpart
@pointfloat    =  (@digitpart)* @fraction | @digitpart[\.]
@exponent      =  [eE] ([\+\-]?) @digitpart
@exponentfloat =  (@digitpart | @pointfloat)* @exponent
@floatnumber   =  @pointfloat | @exponentfloat


-- Token matches by (Top-Down) (Long-Short)
tokens :-
  -- Ignore
  $white+                        ; -- As long there is one separating between tokens
  "///"[^\n]*                    ; -- Normal comments, I already wanted to do this a long time ago
  "/*"                           { simpleTokenize TokLComment } -- Block comments
  "*/"                           { simpleTokenize TokRComment } -- Block comments

  -- Literals
  @floatnumber                   { valueTokenize TokFloat }
  $digit+                        { valueTokenize TokInt }
  \'($char|\\.)\'                { valueTokenize TokChar }
  \"($stringChar|\\.)*\"         { valueTokenize TokString }

  -- Assignment Operators
  "+="                           { simpleTokenize TokAddAssign }
  "-="                           { simpleTokenize TokSubAssign }
  "*="                           { simpleTokenize TokMulAssign }
  "/="                           { simpleTokenize TokDivAssign }
  "%="                           { simpleTokenize TokModAssign }
  "&="                           { simpleTokenize TokBinAndAssign }
  "|="                           { simpleTokenize TokBinOrAssign }
  "^="                           { simpleTokenize TokBinXorAssign }
  "<<="                          { simpleTokenize TokBinLShiftAssign }
  ">>="                          { simpleTokenize TokBinRShiftAssign }
  "="                            { simpleTokenize TokAssign }

  -- Brackets
  "("                            { simpleTokenize TokLBrack }
  ")"                            { simpleTokenize TokRBrack }
  "["                            { simpleTokenize TokLSqBrack }
  "]"                            { simpleTokenize TokRSqBrack }
  "{"                            { simpleTokenize TokLCBrack }
  "}"                            { simpleTokenize TokRCBrack }

  -- Logical Operators
  "!"                            { simpleTokenize TokNot }
  "&&"                           { simpleTokenize TokAnd }
  "||"                           { simpleTokenize TokOr }

  -- Comparison Operators
  "=="                           { simpleTokenize TokEq }
  "!="                           { simpleTokenize TokNeq }
  "<="                           { simpleTokenize TokLte }
  "<"                            { simpleTokenize TokLt }
  ">="                           { simpleTokenize TokGte }
  ">"                            { simpleTokenize TokGt }

  -- Arithmetic Operators
  "+"                            { simpleTokenize TokAdd }
  "-"                            { simpleTokenize TokSub }
  "*"                            { simpleTokenize TokMul }
  "/"                            { simpleTokenize TokDiv }
  "%"                            { simpleTokenize TokMod }

  -- Binary Operators 
  "&"                            { simpleTokenize TokBinAnd }
  "|"                            { simpleTokenize TokBinOr }
  "^"                            { simpleTokenize TokBinXor }
  "<<"                           { simpleTokenize TokBinLShift }
  ">>"                           { simpleTokenize TokBinRShift }

  -- Special
  "."                            { simpleTokenize TokDot }
  ","                            { simpleTokenize TokComma }
  ";"                            { simpleTokenize TokSemiColon }

  -- Identifier / Keywords (check identTokenize)
  [_ $alpha] [$alpha $digit _]*  { identTokenize }

  -- Catch-all Error
  .                              { tokenize TokError }

{
identTokenize :: AlexInput -> Int -> Alex Token
identTokenize inp@(_, _, _, str) len = tokenize (\_ -> getToken (take len str)) inp len
  where
    -- Check for matched string using `case of` instead of alex lexer
    -- to prevent incorrect matches like "sinh" => "sin" + "h" due to
    -- code writing errors
    getToken :: String -> TokenType
    getToken s = case s of
      -- Constants and Literals
      "pi"      -> TokFloat pi
      "null"    -> TokNull
      "true"    -> TokBool True
      "false"   -> TokBool False

      -- Control Structures Variables
      "if"      -> TokIf
      "then"    -> TokThen
      "else"    -> TokElse
      "let"     -> TokLet
      "in"      -> TokIn
      "for"     -> TokFor
      "while"   -> TokWhile
      "switch"  -> TokSwitch
      "case"    -> TokCase

      -- Identifier = Variable / Function
      s         -> TokIdent s

-- End of program
alexEOF :: Alex Token
alexEOF = return $ Token TokEOF (TokenPos 0 0)

-- Tokenizer
getTokenPos :: AlexPosn -> TokenPos
getTokenPos (AlexPn _ l c) = TokenPos l c

-- Normal tokenize, parses value as string
tokenize :: (String -> TokenType) -> AlexInput -> Int -> Alex Token
tokenize tt (pos, _, _, str) len = pure $ Token (tt (take len str)) (getTokenPos pos)

-- For simple tokens without values (\_ p helps drop String value provided by tokenize)
simpleTokenize :: TokenType -> AlexInput -> Int -> Alex Token
simpleTokenize tt = tokenize (const tt)

-- Returns value as an Int (enforced by type signature)
valueTokenize :: Read a => (a -> TokenType) -> AlexInput -> Int -> Alex Token
valueTokenize tt = tokenize (tt . read)

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
      formatPos l c
      ++ "<LEXER ERROR> -- Could not tokenize string "
      ++ show s


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
}


            -- -- List Operations
            -- List literals
            -- Ranges
            -- List indexing
            -- List length

      -- else if s `elem` functions then TokenIdent s 
