-- https://haskell-happy.readthedocs.io/en/latest/using.html
{
module Lang.Parser.Parser (runParser) where
import Lang.Lexer.Tokens (Token(..), TokenType(..), TokenPos(..))
import Lang.Parser.Helper (parserIgnore, formatRenderError)
import Lang.Parser.Expr (Expr(..), Stmt(..), AssignOperator(..), TwoExprOperator(..))
import Lang.Repl.Helper (formatPos)
}

%name parse
%tokentype { Token }
%monad { Either String } { >>= } { return }
%error { parseError }

%token
  -- Constants and Literals
  null                           { Token TokNull            _ }
  bool                           { Token (TokBool $$)       _ }
  int                            { Token (TokInt $$)        _ }
  float                          { Token (TokFloat $$)      _ }
  char                           { Token (TokChar $$)       _ }
  string                         { Token (TokString $$)     _ }
  ident                          { Token (TokIdent $$)      _ } -- Identifier / Variable / Functions
  
  -- Assignment Operators
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
  
  -- Special characters
  '.'                            { Token TokDot             _ }
  ','                            { Token TokComma           _ }
  ';'                            { Token TokSemiColon       _ }

  -- Brackets
  '('                            { Token TokLBrack          _ }
  ')'                            { Token TokRBrack          _ }
  '['                            { Token TokLSqBrack        _ }
  ']'                            { Token TokRSqBrack        _ }
  '{'                            { Token TokLCBrack         _ }
  '}'                            { Token TokRCBrack         _ }

  -- Logical Operators
  '!'                            { Token TokNot             _ }
  '&&'                           { Token TokAnd             _ }
  '||'                           { Token TokOr              _ }

  -- Comparison Operators
  '=='                           { Token TokEq              _ }
  '!='                           { Token TokNeq             _ }
  '<='                           { Token TokLte             _ }
  '<'                            { Token TokLt              _ }
  '>='                           { Token TokGte             _ }
  '>'                            { Token TokGt              _ }

  -- Arithmetic Operators
  '+'                            { Token TokAdd             _ }
  '-'                            { Token TokSub             _ }
  '*'                            { Token TokMul             _ }
  '/'                            { Token TokDiv             _ }
  '%'                            { Token TokMod             _ }

  -- Binary Arithmetic Operators
  '&'                            { Token TokBinAnd          _ }
  '|'                            { Token TokBinOr           _ }
  '^'                            { Token TokBinXor          _ }
  '<<'                           { Token TokBinLShift       _ }
  '>>'                           { Token TokBinRShift       _ }

  -- Control Structures Variables
  'if'                           { Token TokIf              _ }
  'then'                         { Token TokThen            _ }
  'else'                         { Token TokElse            _ }
  'let'                          { Token TokLet             _ }
  'in'                           { Token TokIn              _ }
  'for'                          { Token TokFor             _ }
  'while'                        { Token TokWhile           _ }
  'switch'                       { Token TokSwitch          _ }
  'case'                         { Token TokCase            _ }


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
%nonassoc 'if' 'then' 'else'

%%

Program
  : Stmts                             { $1 }

-- Statement should separated using a semicolon (;) 
-- (;) is optional for last statement for easy repl
Stmts 
  : Stmt ';' Stmts                              { $1 : $3 }
  | Stmt ';'                                    { [$1] }
  | Stmt                                        { [$1] }

Stmt
  : 'if' '(' Expr ')' Block ElseBlock           { If $3 $5 $6 }                       -- if cond then doIf else doElse
  | 'for' '(' Stmt ';' Expr ';' Stmt ')' Block  { For $3 $5 $7 $9 }                   -- for (start; step; stop) {doFor}
  | 'while' '(' Expr ')' Block                  { While $3 $5 }                       -- while (cond) {doWhile}
  -- | 'switch' Expr CaseBlock                  { While $3 $5 }                       -- switch x: case 1:

  | ident '=' Expr                              { Assign $1 $3 }                      -- x = 10 (Dynamic read type)
  | ident AssignOp Expr                         { AssignOp $2 $1 $3 }                 -- x += 10, x *= 6 etc.
  | ident '(' Args ')'                          { Call $1 $3 }                        -- sin(x)
  | Expr                                        { ExprStmt $1 }                       -- x

Block
  : '{' Stmts '}'                               { $2 }        -- { do1; do2; do3 } | { do1 }
  | Stmt ';'                                    { [$1] }      -- cond;
  | '{' '}'                                     { [] }        -- {}

ElseBlock
  : 'else' '{' Stmts '}'                        { Just $3 }   -- else { do1; do2; }
  | 'else' Stmt                                 { Just [$2] } -- else if | else do1;
  |                                             { Nothing }   -- else {}

Args
  : Expr                                        { [$1] }      -- One / Last Param
  | Expr ',' Args                               { $1 : $3 }   -- >1 Params
  |                                             { [] }        -- No params

Expr
  : Expr '+' Expr   { BinOp Add       $1 $3} -- x + y
  | Expr '-' Expr   { BinOp Sub       $1 $3} -- x - y
  | Expr '*' Expr   { BinOp Mul       $1 $3} -- x * y
  | Expr '/' Expr   { BinOp Div       $1 $3} -- x / y
  | Expr '%' Expr   { BinOp Mod       $1 $3} -- x % y
  | Expr '&' Expr   { BinOp BinAnd    $1 $3} -- x & y
  | Expr '|' Expr   { BinOp BinOr     $1 $3} -- x | y
  | Expr '^' Expr   { BinOp BinXor    $1 $3} -- x ^ y
  | Expr '<<' Expr  { BinOp BinLShift $1 $3} -- x << y
  | Expr '>>' Expr  { BinOp BinRShift $1 $3} -- x >> y
  | Expr '==' Expr  { BinOp Eq        $1 $3} -- x == y
  | Expr '!=' Expr  { BinOp Neq       $1 $3} -- x != y
  | Expr '<=' Expr  { BinOp Lte       $1 $3} -- x <= y
  | Expr '<' Expr   { BinOp Lt        $1 $3} -- x < y
  | Expr '>=' Expr  { BinOp Gte       $1 $3} -- x >= y
  | Expr '>' Expr   { BinOp Gt        $1 $3} -- x > y
  | Expr '&&' Expr  { BinOp And       $1 $3} -- x && y
  | Expr '||' Expr  { BinOp Or        $1 $3} -- x || y
  
  | '(' Expr ')'                                { $2 }             -- Precedence
  | '[' Expr ']'                                { SqBrack $2 }     -- List
  
  | '!' Expr %prec NOT                          { Not $2 }        -- Not bool
  | '-' Expr %prec NEG                          { Negate $2 }     -- Negate int/float
  
  -- Type of values
  | null                                        { NullLit }
  | int                                         { IntLit $1 }
  | char                                        { CharLit $1 }
  | bool                                        { BoolLit $1 }
  | float                                       { FloatLit $1 }
  | string                                      { StringLit $1 }
  
  | ident                                       { Var $1 }        -- Identifier


-- Couple logic together since resulting output for BNF is similar
-- Generate eval logic at eval
AssignOp
  : '+='   { AddEq }
  | '-='   { SubEq }
  | '*='   { MulEq }
  | '/='   { DivEq }
  | '%='   { ModEq }
  | '&='   { BinAndEq }
  | '|='   { BinOrEq }
  | '^='   { BinXorEq }
  | '<<='  { BinLShiftEq }
  | '>>='  { BinRShiftEq }

{
-- Show error when parsing, check if you initialized it in the parser
parseError :: [Token] -> Either String a
parseError [] = Left "<PARSER ERROR> -- Unexpected end of input. Did you provide enough arguments?"
parseError ((Token tt (TokenPos l c)):_) = Left $ formatPos l c ++ "<PARSER ERROR> -- Unexpected token `" ++ show tt ++ "`"

runParser :: String -> [Token] -> Either String [Stmt]
runParser src toks = case parse (filter (not . parserIgnore . tokenType) toks) of
  Right ast -> Right ast
  Left err -> formatRenderError src err
}


-- terminal '.' is unused
-- terminal ',' is unused
-- terminal 'then' is unused
-- terminal 'let' is unused
-- terminal 'in' is unused
-- terminal 'switch' is unused
-- terminal 'case' is unused