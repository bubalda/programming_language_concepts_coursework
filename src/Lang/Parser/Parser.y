-- https://haskell-happy.readthedocs.io/en/latest/using.html
{
module Lang.Parser.Parser (runParser) where
import Lang.Lexer.Tokens (Token(..), TokenType(..), TokenPos(..))
import Lang.Parser.Helper (parserIgnore, formatRenderError)
import Lang.Parser.Expr (Expr(..), Stmt(..))
import Lang.Repl.Helper (formatPos)
}

%name parse
%tokentype { Token }
%monad { Either String } { >>= } { return }
%error { parseError }

%token
  int                            { Token (TokInt $$)        _ }
  char                           { Token (TokChar $$)       _ }
  bool                           { Token (TokBool $$)       _ }
  double                         { Token (TokDouble $$)     _ }
  string                         { Token (TokString $$)     _ }
  'null'                         { Token TokNull            _ }
  
  ident                          { Token (TokIdent $$)      _ }
  
  '//='                          { Token TokFloorDivAssign  _ }
  '**='                          { Token TokPowAssign       _ }
  '+='                           { Token TokAddAssign       _ }
  '-='                           { Token TokSubAssign       _ }
  '*='                           { Token TokMulAssign       _ }
  '/='                           { Token TokDivAssign       _ }
  '%='                           { Token TokModAssign       _ }
  '&='                           { Token TokBinAndAssign    _ }
  '|='                           { Token TokBinOrAssign     _ }
  '^='                           { Token TokBinXorAssign    _ }
  '<<='                          { Token TokBinLShiftAssign _ }
  '>>='                          { Token TokBinRShiftAssign _ }
  '='                            { Token TokAssign          _ }

  '.'                            { Token TokDot             _ }
  ','                            { Token TokComma           _ }
  ':'                            { Token TokColon           _ }
  ';'                            { Token TokSemiColon       _ }
  '?'                            { Token TokQuestion        _ }

  '('                            { Token TokLBrack          _ }
  ')'                            { Token TokRBrack          _ }
  '['                            { Token TokLSqBrack        _ }
  ']'                            { Token TokRSqBrack        _ }
  '{'                            { Token TokLCBrack         _ }
  '}'                            { Token TokRCBrack         _ }

  '!'                            { Token TokExclamation     _ }
  '&&'                           { Token TokAnd             _ }
  '||'                           { Token TokOr              _ }

  '=='                           { Token TokEq              _ }
  '!='                           { Token TokNeq             _ }
  '<='                           { Token TokLte             _ }
  '<'                            { Token TokLt              _ }
  '>='                           { Token TokGte             _ }
  '>'                            { Token TokGt              _ }

  '//'                           { Token TokFloorDiv        _ }
  '**'                           { Token TokPow             _ }
  '+'                            { Token TokAdd             _ }
  '-'                            { Token TokSub             _ }
  '*'                            { Token TokMul             _ }
  '/'                            { Token TokDiv             _ }
  '%'                            { Token TokMod             _ }

  '&'                            { Token TokBinAnd          _ }
  '|'                            { Token TokBinOr           _ }
  '^'                            { Token TokBinXor          _ }
  '<<'                           { Token TokBinLShift       _ }
  '>>'                           { Token TokBinRShift       _ }

  type                           { Token (TokType $$)       _ }

  'if'                           { Token TokIf              _ }
  'else'                         { Token TokElse            _ }

  'for'                          { Token TokFor             _ }
  'while'                        { Token TokWhile           _ }
  
  'fun'                          { Token TokFunc            _ }


-- https://en.cppreference.com/w/c/language/operator_precedence.html
-- https://haskell-happy.readthedocs.io/en/latest/using.html#context-dependent-precedence
-- This declares both association and precedence so no need to declare multiple exprs for (+ -) < (* /)
%right '//=' '**=' '+=' '-=' '*=' '/=' '%=' '&=' '|=' '^=' '<<=' '>>='
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
%nonassoc 'if'
%nonassoc 'else'

%%

Program
  : Stmts                             { $1 }

Stmts -- Statement should end using a semicolon (;)
  : Stmt ';' Stmts                              { $1 : $3 }
  | Stmt ';'                                    { [$1] }
  | Stmt                                        { [$1] }

Stmt
  : 'if' '(' Expr ')' Block ElseBlock           { If $3 $5 $6 }
  | 'for' '(' Stmt ';' Expr ';' Stmt ')' Block  { For $3 $5 $7 $9 }
  | 'while' '(' Expr ')' Block                  { While $3 $5 }
  | type ident '=' Expr                         { AssignWithType $1 $2 $4 }
  | ident '=' Expr                              { Assign $1 $3 }
  | ident '//=' Expr                            { Assign $1 (FloorDiv  (Var $1) $3) }
  | ident '**=' Expr                            { Assign $1 (Pow       (Var $1) $3) }
  | ident '+=' Expr                             { Assign $1 (Add       (Var $1) $3) }
  | ident '-=' Expr                             { Assign $1 (Sub       (Var $1) $3) }
  | ident '*=' Expr                             { Assign $1 (Mul       (Var $1) $3) }
  | ident '/=' Expr                             { Assign $1 (Div       (Var $1) $3) }
  | ident '%=' Expr                             { Assign $1 (Mod       (Var $1) $3) }
  | ident '&=' Expr                             { Assign $1 (BinAnd    (Var $1) $3) }
  | ident '|=' Expr                             { Assign $1 (BinOr     (Var $1) $3) }
  | ident '^=' Expr                             { Assign $1 (BinXor    (Var $1) $3) }
  | ident '<<=' Expr                            { Assign $1 (BinLShift (Var $1) $3) }
  | ident '>>=' Expr                            { Assign $1 (BinRShift (Var $1) $3) }
  | Expr                                        { ExprStmt $1 }

Block
  : '{' Stmts '}'                               { $2 }
  | Stmt ';'                                    { [$1] }
  | '{' '}'                                     { [] }

ElseBlock
  : 'else' '{' Stmts '}'                        { Just $3 }
  | 'else' Stmt                                 { Just [$2] }
  |                                             { Nothing }

Expr
  : Expr '||' Expr                              { Or $1 $3 }
  | Expr '&&' Expr                              { And $1 $3 }
  | Expr '|' Expr                               { BinOr  $1 $3 }
  | Expr '^' Expr                               { BinXor $1 $3 }
  | Expr '&' Expr                               { BinAnd $1 $3 }
  | Expr '==' Expr                              { Eq $1 $3 }
  | Expr '!=' Expr                              { Neq $1 $3 }
  | Expr '<=' Expr                              { Lte $1 $3 }
  | Expr '<' Expr                               { Lt $1 $3 }
  | Expr '>=' Expr                              { Gte $1 $3 }
  | Expr '>' Expr                               { Gt $1 $3 }
  | Expr '*' Expr                               { Mul $1 $3 }
  | Expr '/' Expr                               { Div $1 $3 }
  | Expr '%' Expr                               { Mod $1 $3 }
  | Expr '**' Expr                              { Pow $1 $3 }
  | Expr '//' Expr                              { FloorDiv $1 $3 }
  | Expr '<<' Expr                              { BinLShift $1 $3 }
  | Expr '>>' Expr                              { BinRShift $1 $3 }
  | Expr '+' Expr                               { Add $1 $3 }
  | Expr '-' Expr                               { Sub $1 $3 }
  
  | '!' Expr %prec NOT                          { Not $2 }
  
  | '(' Expr ')'                                { Brack $2 }
  | '[' Expr ']'                                { SqBrack $2 }
  
  | '-' Expr %prec NEG                          { Negate $2 }
  
  | 'null'                                      { NullLit }
  | int                                         { IntLit $1 }
  | char                                        { CharLit $1 }
  | bool                                        { BoolLit $1 }
  | double                                      { DoubleLit $1 }
  | string                                      { StringLit $1 }
  | ident                                       { Var $1 }


{
-- Show error when parsing, check if you initialized it in the parser
parseError :: [Token] -> Either String a
parseError [] = Left "<PARSER ERROR> -- Unexpected end of input"
parseError ((Token tt (TokenPos l c)):_) = Left $ formatPos l c ++ "<PARSER ERROR> -- Unexpected token `" ++ show tt ++ "`"

runParser :: String -> [Token] -> Either String [Stmt]
runParser src toks = case parse (filter (not . parserIgnore . tokenType) toks) of
  Right ast -> Right ast
  Left err -> formatRenderError src err
}
