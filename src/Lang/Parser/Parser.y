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
  -- Constants and Literals
  'null'                         { Token TokNull            _ }
  bool                           { Token (TokBool $$)       _ }
  int                            { Token (TokInt $$)        _ }
  float                          { Token (TokFloat $$)      _ }
  char                           { Token (TokChar $$)       _ }
  string                         { Token (TokString $$)     _ }

  -- Static type declaration
  type                           { Token (TokType $$)       _ }

  -- Identifier
  ident                          { Token (TokIdent $$)      _ }
  
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

  -- Control Structures Variables`
  'if'                           { Token TokIf              _ }
  'then'                         { Token TokThen            _ }
  'else'                         { Token TokElse            _ }
  'let'                          { Token TokLet             _ }
  'in'                           { Token TokIn              _ }
  'for'                          { Token TokFor             _ }
  'while'                        { Token TokWhile           _ }
  'switch'                       { Token TokWhile           _ }
  'case'                         { Token TokWhile           _ }

  -- Hyperbolic Functions
  'sinh'                         { Token TokSinh            _ }
  'cosh'                         { Token TokCosh            _ }
  'tanh'                         { Token TokTanh            _ }
  'csch'                         { Token TokCsch            _ }
  'sech'                         { Token TokSech            _ }
  'coth'                         { Token TokCoth            _ }
  'asinh'                        { Token TokAsinh           _ }
  'acosh'                        { Token TokAcosh           _ }

  -- Statistical Functions
  'mean'                         { Token TokMean            _ } 
  'median'                       { Token TokMedian          _ } 
  'mode'                         { Token TokMode            _ } 
  'sum'                          { Token TokSum             _ } 
  'product'                      { Token TokProduct         _ } 
  'min'                          { Token TokMin             _ } 
  'max'                          { Token TokMax             _ } 
  'stddev'                       { Token TokStddev          _ } 

  -- Power and Root Functions
  'sqrt'                         { Token TokSqrt            _ } 
  'cbrt'                         { Token TokCbrt            _ } 
  'pow'                          { Token TokPow             _ } 
  'exp'                          { Token TokExp             _ } 
  'square'                       { Token TokSquare          _ } 
  'cube'                         { Token TokCube            _ } 
  'exp10'                        { Token TokExp10           _ } 

  -- Trigonometric Functions
  'sin'                          { Token TokSin             _ } 
  'cos'                          { Token TokCos             _ } 
  'tan'                          { Token TokTan             _ } 
  'asin'                         { Token TokAsin            _ } 
  'acos'                         { Token TokAcos            _ } 
  'atan'                         { Token TokAtan            _ } 
  'atan2'                        { Token TokAtan2           _ } 
  'sec'                          { Token TokSec             _ } 
  'csc'                          { Token TokCsc             _ } 
  'cot'                          { Token TokCot             _ } 
  'versin'                       { Token TokVersin          _ } 
  'exsec'                        { Token TokExsec           _ } 

  -- Logarithmic Functions
  'ln'                           { Token TokLn              _ } 
  'log10'                        { Token TokLog10           _ } 
  'log2'                         { Token TokLog2            _ } 
  'log'                          { Token TokLog             _ } 
  'log1p'                        { Token TokLog1p           _ } 

  -- Combinatorial Functions
  'fact'                         { Token TokFact            _ } 
  'fact2'                        { Token TokFact2           _ } 
  'comb'                         { Token TokComb            _ } 
  'perm'                         { Token TokPerm            _ } 
  'gcd'                          { Token TokGcd             _ } 
  'lcm'                          { Token TokLcm             _ } 
  'fib'                          { Token TokFib             _ } 
  'gamma'                        { Token TokGamma           _ } 

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

-- Statement should separated using a semicolon (;) 
-- (;) is optional for last statement for easy repl
Stmts 
  : Stmt ';' Stmts                              { $1 : $3 }
  | Stmt ';'                                    { [$1] }
  | Stmt                                        { [$1] }

Stmt
  : 'if' '(' Expr ')' Block ElseBlock           { If $3 $5 $6 }                       -- if (cond) {doIf} else if (cond2) {doElif} else {doElse}
  | 'for' '(' Stmt ';' Expr ';' Stmt ')' Block  { For $3 $5 $7 $9 }                   -- for (start; step; stop) {doFor}
  | 'while' '(' Expr ')' Block                  { While $3 $5 }                       -- while 
  | type ident '=' Expr                         { AssignWithType $1 $2 $4 }           -- double x = "hello" => Error "Could not assign string "hello" to type "double""
  | ident '=' Expr                              { Assign $1 $3 }                      -- x = 10 (Dynamic read type)
  | ident '+=' Expr                             { Assign $1 (Add       (Var $1) $3) } -- x = x + y
  | ident '-=' Expr                             { Assign $1 (Sub       (Var $1) $3) } -- x = x - y
  | ident '*=' Expr                             { Assign $1 (Mul       (Var $1) $3) } -- x = x * y
  | ident '/=' Expr                             { Assign $1 (Div       (Var $1) $3) } -- x = x / y
  | ident '%=' Expr                             { Assign $1 (Mod       (Var $1) $3) } -- x = x % y
  | ident '&=' Expr                             { Assign $1 (BinAnd    (Var $1) $3) } -- x = x & y
  | ident '|=' Expr                             { Assign $1 (BinOr     (Var $1) $3) } -- x = x | y
  | ident '^=' Expr                             { Assign $1 (BinXor    (Var $1) $3) } -- x = x ^ y
  | ident '<<=' Expr                            { Assign $1 (BinLShift (Var $1) $3) } -- x = x << y
  | ident '>>=' Expr                            { Assign $1 (BinRShift (Var $1) $3) } -- x = x >> y
  | Expr                                        { ExprStmt $1 }                       -- x

Block
  : '{' Stmts '}'                               { $2 }        -- { do1; do2; do3 } | { do1 }
  | Stmt ';'                                    { [$1] }      -- cond;
  | '{' '}'                                     { [] }        -- {}

ElseBlock
  : 'else' '{' Stmts '}'                        { Just $3 }   -- else { do1; do2; }
  | 'else' Stmt                                 { Just [$2] } -- else if | else do1;
  |                                             { Nothing }   -- else {}

Expr
  : Expr '||' Expr                              { Or $1 $3 }          -- x || y      
  | Expr '&&' Expr                              { And $1 $3 }         -- x && y
  | Expr '|' Expr                               { BinOr  $1 $3 }      -- x | y
  | Expr '^' Expr                               { BinXor $1 $3 }      -- x ^ y
  | Expr '&' Expr                               { BinAnd $1 $3 }      -- x & y
  | Expr '==' Expr                              { Eq $1 $3 }          -- x == y
  | Expr '!=' Expr                              { Neq $1 $3 }         -- x != y
  | Expr '<=' Expr                              { Lte $1 $3 }         -- x <= y
  | Expr '<' Expr                               { Lt $1 $3 }          -- x < y
  | Expr '>=' Expr                              { Gte $1 $3 }         -- x >= y
  | Expr '>' Expr                               { Gt $1 $3 }          -- x > y
  | Expr '*' Expr                               { Mul $1 $3 }         -- x * y
  | Expr '/' Expr                               { Div $1 $3 }         -- x / y
  | Expr '%' Expr                               { Mod $1 $3 }         -- x % y
  | Expr '<<' Expr                              { BinLShift $1 $3 }   -- x << y
  | Expr '>>' Expr                              { BinRShift $1 $3 }   -- x >> y
  | Expr '+' Expr                               { Add $1 $3 }         -- x + y
  | Expr '-' Expr                               { Sub $1 $3 }         -- x - y
  
  | 'sinh' Expr                                 { Sinh    $2    } -- sinh x
  | 'cosh' Expr                                 { Cosh    $2    } -- cosh x
  | 'tanh' Expr                                 { Tanh    $2    } -- tanh x
  | 'csch' Expr                                 { Csch    $2    } -- csch x
  | 'sech' Expr                                 { Sech    $2    } -- sech x
  | 'coth' Expr                                 { Coth    $2    } -- coth x
  | 'asinh' Expr                                { Asinh   $2    } -- asinh x
  | 'acosh' Expr                                { Acosh   $2    } -- acosh x 
  | 'mean' Expr                                 { Mean    $2    } -- average [x]
  | 'median' Expr                               { Median  $2    } -- median [x]
  | 'mode' Expr                                 { Mode    $2    } -- mode [x]
  | 'sum' Expr                                  { Sum     $2    } -- foldr (+) [x]
  | 'product' Expr                              { Product $2    } -- foldr (*) [x]
  | 'min' Expr                                  { Min     $2    } -- min [x]
  | 'max' Expr                                  { Max     $2    } -- max [x]
  | 'stddev' Expr                               { Stddev  $2    } -- std [x]
  | 'sqrt' Expr                                 { Sqrt    $2    } -- x^(1/2)
  | 'cbrt' Expr                                 { Cbrt    $2    } -- x^(1/3)
  | 'pow' Expr Expr                             { Pow     $2 $3 } -- x^n
  | 'exp' Expr                                  { Exp     $2    } -- e^x
  | 'square' Expr                               { Square  $2    } -- x^2
  | 'cube' Expr                                 { Cube    $2    } -- x^3
  | 'exp10' Expr                                { Exp10   $2    } -- 10^x
  | 'sin' Expr                                  { Sin     $2    } -- sin x 
  | 'cos' Expr                                  { Cos     $2    } -- cos x
  | 'tan' Expr                                  { Tan     $2    } -- tan x
  | 'asin' Expr                                 { Asin    $2    } -- asin x
  | 'acos' Expr                                 { Acos    $2    } -- acos x
  | 'atan' Expr                                 { Atan    $2    } -- atan x
  | 'atan2' Expr Expr                           { Atan2   $2 $3 } -- atan2 x y
  | 'sec' Expr                                  { Sec     $2    } -- sec x 
  | 'csc' Expr                                  { Csc     $2    } -- csc x 
  | 'cot' Expr                                  { Cot     $2    } -- cot x 
  | 'versin' Expr                               { Versin  $2    } -- versin x 
  | 'exsec' Expr                                { Exsec   $2    } -- exsec x 
  | 'ln' Expr                                   { Ln      $2    } -- ln x
  | 'log10' Expr                                { Log10   $2    } -- log10 x
  | 'log2' Expr                                 { Log2    $2    } -- log2 x
  | 'log' Expr                                  { Log     $2    } -- log x
  | 'log1p' Expr                                { Log1p   $2    } -- log1p x
  | 'fact' Expr                                 { Fact    $2    } -- fact x
  | 'fact2' Expr                                { Fact2   $2    } -- factSquared x
  | 'comb' Expr Expr                            { Comb    $2 $3 } -- comb r n 
  | 'perm' Expr Expr                            { Perm    $2 $3 } -- perm r n 
  | 'gcd' Expr                                  { Gcd     $2    } -- gcd [x] 
  | 'lcm' Expr                                  { Lcm     $2    } -- lcm [x]
  | 'fib' Expr                                  { Fib     $2    } -- fib x
  | 'gamma' Expr                                { Gamma   $2    } -- gamma x
  
  | '(' Expr ')'                                { Brack $2 }      -- Precedence
  | '[' Expr ']'                                { SqBrack $2 }    -- List
  
  | '!' Expr %prec NOT                          { Not $2 }        -- Not bool
  | '-' Expr %prec NEG                          { Negate $2 }     -- Negate int/float
  
  | 'null'                                      { NullLit }
  | int                                         { IntLit $1 }
  | char                                        { CharLit $1 }
  | bool                                        { BoolLit $1 }
  | float                                       { FloatLit $1 }
  | string                                      { StringLit $1 }

  | ident                                       { Var $1 }        -- Identifier


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