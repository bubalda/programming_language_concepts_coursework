-- https://haskell-happy.readthedocs.io/en/latest/using.html
{
module Lang.Parser.Parser (runParser) where
import Lang.Lexer.Tokens (Token(..), TokenType(..), TokenPos(..))
import Lang.Parser.Helper (parserIgnore, formatRenderError)
import Lang.Parser.Expr (Expr(..), Stmt(..), AssignOperator(..), TwoExprOperator(..), Slice(..), Type(..))
import Lang.Repl.Helper (formatPos)
}

%name parse                                   -- My parse function name
%tokentype { Token }                          -- My token's type name
%monad { Either String } { >>= } { return }   -- Returns a monadic Either (ValueType, ErrorString)
%error { parseError }                         -- My parse error handler name

%token
  -- Static type declaration
  t_double                       { Token TokDeclDouble      _ }        
  t_char                         { Token TokDeclChar        _ }        
  t_string                       { Token TokDeclString      _ }        
  t_float                        { Token TokDeclFloat       _ }       
  t_int                          { Token TokDeclInt         _ }       
  t_bool                         { Token TokDeclBool        _ }        

  -- Constants and Literals
  null                           { Token TokNull            _ }
  bool                           { Token (TokBool $$)       _ }
  int                            { Token (TokInt $$)        _ }
  float                          { Token (TokFloat $$)      _ }
  double                         { Token (TokDouble $$)     _ }
  char                           { Token (TokChar $$)       _ }
  string                         { Token (TokString $$)     _ }

  -- Identifier = Variable / Function name
  ident                          { Token (TokIdent $$)      _ }
  
  -- Assignment Operators
  '+='                           { Token TokAddAssign       _ }
  '-='                           { Token TokSubAssign       _ }
  '*='                           { Token TokMulAssign       _ }
  '/='                           { Token TokDivAssign       _ }
  '%='                           { Token TokModAssign       _ }
  '&='                           { Token TokBitAndAssign    _ }
  '|='                           { Token TokBitOrAssign     _ }
  '^='                           { Token TokBitXorAssign    _ }
  '<<='                          { Token TokBitLShiftAssign _ }
  '>>='                          { Token TokBitRShiftAssign _ }
  '='                            { Token TokAssign          _ }
  
  -- End of line
  ';'                            { Token TokSemiColon       _ }

  -- List Operations
  ','                            { Token TokComma           _ }
  ':'                            { Token TokColon           _ }
  '..'                           { Token TokDotDot          _ }
  '['                            { Token TokLSqBrack        _ }
  ']'                            { Token TokRSqBrack        _ }

  -- Brackets (Precedence)
  '('                            { Token TokLBrack          _ }
  ')'                            { Token TokRBrack          _ }

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
  '&'                            { Token TokBitAnd          _ }
  '|'                            { Token TokBitOr           _ }
  '^'                            { Token TokBitXor          _ }
  '<<'                           { Token TokBitLShift       _ }
  '>>'                           { Token TokBitRShift       _ }

  -- Control Structures Variables
  'if'                           { Token TokIf              _ }
  'then'                         { Token TokThen            _ }
  'else'                         { Token TokElse            _ }
  'let'                          { Token TokLet             _ }
  'in'                           { Token TokIn              _ }


-- https://en.cppreference.com/w/c/language/operator_precedence.html
-- https://haskell-happy.readthedocs.io/en/latest/using.html#context-dependent-precedence
-- Precedence can also be handled here so no need to do manual structural precedence
-- Low precedence -> High precedence 
%nonassoc LET
%right '=' '+=' '-=' '*=' '/=' '%=' '&=' '|=' '^=' '<<=' '>>='
%left '||'
%left '&&'
%left '|'
%left '^'
%left '&'
%left '==' '!='
%left '<' '<=' '>' '>='           -- 3 < n < 5 => (3 < n) < 5
%left '<<' '>>'
%left '+' '-'                     -- 3 + 4 > 5 => (3 + 4) > 5
%left '*' '/' '%'                 -- 3 + 4 * 5 => 3 + (4 * 5)
%right NOT
%right NEG
%nonassoc NO_ELSE
%nonassoc 'else'

%%

Program
  : Stmts                                       { $1 }

-- Allows repl to support one-liners like this -- x = 7; x += 1; x *= 2; x
Stmts 
  : Stmts Stmt                                  { $1 ++ [$2] }
  | Stmt                                        { [$1] }

Stmt
  -- if conditions
  : 'if' Expr 'then' Stmt %prec NO_ELSE         { If $2 $4 Nothing } 
  | 'if' Expr 'then' Stmt 'else' Stmt           { If $2 $4 (Just $6) }

  -- Statement should separated using a semicolon (;) 
  | ident '=' Expr ';'                           { Assign $1 $3 }                      -- x = 10 (Dynamic read type)
  | ident AssignOp Expr ';'                      { AssignOp $2 $1 $3 }                 -- x += 10, x *= 6 etc.
  | Expr ';'                                     { ExprStmt $1 }                       -- x
  | Type ident '=' Expr ';'                      { Decl $1 $2 $4 }                     -- double x = 1.23


-- Couple logic together since parameters and precedence is similar
AssignOp
  : '+='   { AddEq }
  | '-='   { SubEq }
  | '*='   { MulEq }
  | '/='   { DivEq }
  | '%='   { ModEq }
  | '&='   { BitAndEq }
  | '|='   { BitOrEq }
  | '^='   { BitXorEq }
  | '<<='  { BitLShiftEq }
  | '>>='  { BitRShiftEq }


-- Let the %left/%right table handle the hierarchy
Expr
  : 'let' ident '=' Expr 'in' Expr %prec LET      { Let $2 $4 $6 }
  | Expr '||' Expr                                { BinOp Or $1 $3 }
  | Expr '&&' Expr                                { BinOp And $1 $3 }
  | Expr '|'  Expr                                { BinOp BitOr $1 $3 }
  | Expr '^'  Expr                                { BinOp BitXor $1 $3 }
  | Expr '&'  Expr                                { BinOp BitAnd $1 $3 }
  | Expr '==' Expr                                { BinOp Eq $1 $3 }
  | Expr '!=' Expr                                { BinOp Neq $1 $3 }
  | Expr '<'  Expr                                { BinOp Lt $1 $3 }
  | Expr '<=' Expr                                { BinOp Lte $1 $3 }
  | Expr '>'  Expr                                { BinOp Gt $1 $3 }
  | Expr '>=' Expr                                { BinOp Gte $1 $3 }
  | Expr '<<' Expr                                { BinOp BitLShift $1 $3 }
  | Expr '>>' Expr                                { BinOp BitRShift $1 $3 }
  | Expr '+'  Expr                                { BinOp Add $1 $3 }
  | Expr '-'  Expr                                { BinOp Sub $1 $3 }
  | Expr '*'  Expr                                { BinOp Mul $1 $3 }
  | Expr '/'  Expr                                { BinOp Div $1 $3 }
  | Expr '%'  Expr                                { BinOp Mod $1 $3 }
  | Unary                                         { $1 }

-- Enforce structural precedence and prevent shift / reduce errors
Unary
  : '!' Unary %prec NOT     { Not $2 }
  | '-' Unary %prec NEG     { Negate $2 }
  | Postfix                 { $1 }

-- Enforce structural precedence and prevent shift / reduce errors
Postfix
  : Postfix '[' Expr ']'           { ListIndex $1 $3 }
  | Postfix '[' Slice ']'          { ListSlice $1 $3 }
  | Postfix '(' FunctionArgs ')'   { Call $1 $3 }
  | Primary                        { $1 }
  
-- Types of values
Primary
  : '(' Expr ')'              { $2 }
  | null                      { NullLit }
  | int                       { IntLit $1 }
  | char                      { CharLit $1 }
  | bool                      { BoolLit $1 }
  | float                     { FloatLit $1 }
  | double                    { DoubleLit $1 }
  | string                    { StringLit $1 }
  | '[' ListElems ']'         { ListLit $2 }
  | '[' Expr '..' Expr ']'    { ListRange $2 $4 }
  | ident                     { Var $1 }        -- Identifier

-- More efficient by using a left-recursive list
-- As mentioned by https://haskell-happy.readthedocs.io/en/latest/tips.html#performance-tips
ListElems
  : ListElemsList     { reverse $1 }
  |                   { [] }

ListElemsList
  : Expr                      { [$1] }
  | ListElemsList ',' Expr    { $3 : $1 }

Slice
  : MaybeExpr ':' MaybeExpr              { Slice $1 $3 Nothing }
  | MaybeExpr ':' MaybeExpr ':' MaybeExpr { Slice $1 $3 $5 }

MaybeExpr
  : Expr    { Just $1 }
  |         { Nothing }

FunctionArgs
  : FunctionArgsList { reverse $1 }
  |                  { [] }

-- More efficient by using a left-recursive list and reverse after
FunctionArgsList
  : Expr                        { [$1] }
  | FunctionArgsList ',' Expr   { $3 : $1 }

Type
  : t_int          { TInt }
  | t_float        { TFloat }
  | t_double       { TDouble }
  | t_bool         { TBool }
  | t_char         { TChar }
  | t_string       { TString }

{
-- Show error when parsing, check if you initialized it in the parser
parseError :: [Token] -> Either String a
parseError [] = Left "<PARSER ERROR> -- Unexpected end of input. Did you end with a ';'?"
parseError ((Token tt (TokenPos l c)):_) = Left $ formatPos l c ++ "<PARSER ERROR> -- Unexpected token `" ++ show tt ++ "`"

-- Run the happy generated parser to get the ast
runParser :: String -> [Token] -> Either String [Stmt]
runParser src toks = case parse (filter (not . parserIgnore . tokenType) toks) of
  Right ast -> Right ast
  Left err -> formatRenderError src err
}