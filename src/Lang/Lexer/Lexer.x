{
module Lang.Lexer.Lexer (runLexer) where
import Lang.Lexer.Tokens (TokenType (..), TokenPos (..), Token (..))
import Lang.Repl.Helper (formatPos)
}

-- https://haskell-alex.readthedocs.io/en/latest/api.html#the-monad-wrapper
%wrapper "monad"

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

  -- Literals
  @floatnumber                   { valueTokenize TokDouble }
  $digit+                        { valueTokenize TokInt }
  \'($char|\\.)\'                { valueTokenize TokChar }
  \"($stringChar|\\.)*\"         { valueTokenize TokString }

  -- Assignment Operators
  "//="                          { simpleTokenize TokFloorDivAssign }
  "**="                          { simpleTokenize TokPowAssign }
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
  "!"                            { simpleTokenize TokExclamation }
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
  "//"                           { simpleTokenize TokFloorDiv }
  "**"                           { simpleTokenize TokPow }
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
  ":"                            { simpleTokenize TokColon }    -- Note that repl commands also use : so add sth before that to test it
  ";"                            { simpleTokenize TokSemiColon }
  "?"                            { simpleTokenize TokQuestion }

  -- Identifier / Keywords (check identTokenize)
  [_ $alpha] [$alpha $digit _]*  { identTokenize }

  -- Catch-all Error
  .                              { tokenize TokError }

-- TODO
-- terminal 'in' is unused
-- terminal ':' is unused
-- terminal '?' is unused
-- terminal '[]' is unused
-- terminal ',' is unused
-- terminal 'fun' is unused
-- terminal '.' is unused
-- terminal '++' is unused
-- terminal '--' is unused

-- Rq
-- Addition (+), Subtraction (-), Multiplication (*),Division (/), Modulo (%)
-- sqrt, cbrt, pow, exp, square, cube, exp10
-- sin, cos, tan, asin, acos, atan, atan2, sec, csc,
-- cot, versin, exsec
-- ln, log10, log2, log (general), log1p
-- fact, fact2, comb, perm, gcd, lcm, fib, gamma
-- mean, median, mode, sum, product, min, max, stddev
-- sinh, cosh, tanh, csch, sech, coth, asinh, acosh

-- Integer literals, Float literals, Boolean literals, String literals, Pi constant
-- Control Structures Variables, Let bindings, if-then-else, sequences
-- List Operations List literals, Ranges, List indexing, List length

{
-- Tokenize Keywords and Identifier
identTokenize :: AlexInput -> Int -> Alex Token
identTokenize inp@(_, _, _, str) len = tokenize (\_ -> identifier (take len str)) inp len
  where
    identifier :: String -> TokenType
    identifier s =
      case s of
        "true"    -> TokBool True
        "false"   -> TokBool False
        "null"    -> TokNull

        "if"      -> TokIf
        "else"    -> TokElse

        "for"     -> TokFor
        "while"   -> TokWhile

        "fun"     -> TokFunc

        "char"    -> TokType s
        "int"     -> TokType s
        "bool"    -> TokType s
        "double"  -> TokType s
        "String"  -> TokType s

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
}