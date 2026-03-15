-- https://haskell-happy.readthedocs.io/en/latest/using.html
{
module Lang.Parser.Parser (runParser, printAST) where
import Lang.Lexer.Tokens (TokenType(..), Token(..), formatToken)
import Lang.Parser.Eval (Expr(..), Stmt(..), evalExpr, evalStmt)
import Lang.Repl.Helper (wrapSection)
}

%name parse
%tokentype { Token }
%monad { Either String } { >>= } { return }
%error { parseError }

%token
  int                            { Token (TokInt $$)        _ }
  ident                          { Token (TokIdent $$)      _ }
  'true'                         { Token TokTrue            _ }
  'false'                        { Token TokFalse           _ }
  'null'                         { Token TokNull            _ }
  
  '='                            { Token TokAssign          _ }
  '\\'                           { Token TokEscape          _ }
  '.'                            { Token TokDot             _ }
  ','                            { Token TokComma           _ }
  ':'                            { Token TokColon           _ }
  ';'                            { Token TokSemiColon       _ }

  '('                            { Token TokLBrack          _ }
  ')'                            { Token TokRBrack          _ }
  '['                            { Token TokLSQBrack        _ }
  ']'                            { Token TokRSQBrack        _ }
  '{'                            { Token TokLCBrack         _ }
  '}'                            { Token TokRCBrack         _ }

  '!'                            { Token TokNot             _ }
  '&&'                           { Token TokAnd             _ }
  '||'                           { Token TokOr              _ }

  '=='                           { Token TokEQ              _ }
  '!='                           { Token TokNEQ             _ }
  '<='                           { Token TokLTE             _ }
  '<'                            { Token TokLT              _ }
  '>='                           { Token TokGTE             _ }
  '>'                            { Token TokGT              _ }

  '//'                           { Token TokFloorDiv        _ }
  '**'                           { Token TokPower           _ }
  '+'                            { Token TokPlus            _ }
  '-'                            { Token TokMinus           _ }
  '*'                            { Token TokMultiply        _ }
  '/'                            { Token TokDivision        _ }
  '%'                            { Token TokModulo          _ }

  '&'                            { Token TokBinAND          _ }
  '|'                            { Token TokBinOR           _ }
  '^'                            { Token TokBinXOR          _ }
  '<<'                           { Token TokBinLShift       _ }
  '>>'                           { Token TokBinRShift       _ }

  -- 'var'                          { Token TokVar             _ }
  'if'                           { Token TokIf              _ }
  'else'                         { Token TokElse            _ }
  'fun'                          { Token TokFunc            _ }


-- https://en.cppreference.com/w/c/language/operator_precedence.html
-- https://haskell-happy.readthedocs.io/en/latest/using.html#context-dependent-precedence
-- This declares both association and precedence so no need to declare multiple exprs for (+ -) < (* /)
%left '||'
%left '&&'
%left '|'
%left '^'
%left '&'
%left '==' '!='
%left '<' '<=' '>' '>='           -- 3 < n < 5 => (3 < n) < 5
%left '<<' '>>'
%left '+' '-'                     -- 3 + 4 > 5 => (3 + 4) > 5
%left '*' '**' '/' '//' '%'       -- 3 + 4 * 5 => 3 + (4 * 5)
%right NOT
%left NEG
-- right for += or ternary

%%

Stmt
  : ident '=' Expr          { Assign $1 $3 }
  | Expr                    { ExprStmt $1 }

Expr
  : Expr '||' Expr          { Or $1 $3 }
  | Expr '&&' Expr          { And $1 $3 }

  | Expr '|' Expr           { BinOR  $1 $3 }
  | Expr '^' Expr           { BinXOR $1 $3 }
  | Expr '&' Expr           { BinAND $1 $3 }
  
  | Expr '==' Expr          { Eq $1 $3 }
  | Expr '!=' Expr          { Neq $1 $3 }

  | Expr '<' Expr           { Lt $1 $3 }
  | Expr '<=' Expr          { Lte $1 $3 }
  | Expr '>' Expr           { Gt $1 $3 }
  | Expr '>=' Expr          { Gte $1 $3 }

  | Expr '*' Expr           { Mul $1 $3 }
  | Expr '/' Expr           { Div $1 $3 }
  | Expr '%' Expr           { Mod $1 $3 }
  | Expr '**' Expr          { Pow $1 $3 }
  | Expr '//' Expr          { FloorDiv $1 $3 }

  | Expr '<<' Expr          { BinLShift $1 $3 }
  | Expr '>>' Expr          { BinRShift $1 $3 }

  | Expr '+' Expr           { Add $1 $3 }
  | Expr '-' Expr           { Sub $1 $3 }

  | '!' Expr %prec NOT      { Not $2 }
  | '(' Expr ')'            { Brack $2 }
  | '-' Expr %prec NEG      { Negate $2 }

  | 'null'                  { NullLit }
  | 'true'                  { BoolLit True }
  | 'false'                 { BoolLit False }
  | int                     { IntLit $1 }
  | ident                   { Var $1 }


{
parseError :: [Token] -> Either String a
parseError [] = Left "No tokens to parse"             -- Should be handled by REPL (ignore and reprompt)
parseError (t:toks) =
    Left $ "Parse error at token: " ++ formatToken t  -- Show error when parsing, check if you initialized it in the parser
    ++ ".\nFor context, the next few tokens are: " 
    ++ unwords (map (show . tokenType) (take 5 toks)) -- Help navigate to part of code 
    
runParser :: [Token] -> Either String Stmt
runParser toks = parse (filter (not . parserIgnore . tokenType) toks)
  where
    parserIgnore :: TokenType -> Bool
    parserIgnore TokEOF         = True
    parserIgnore (TokError _)   = True
    parserIgnore _              = False

printAST :: Stmt -> IO ()
printAST ast = wrapSection "Abstract Syntax Tree (AST)" (putStrLn (show ast))
}