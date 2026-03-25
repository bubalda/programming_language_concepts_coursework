{
module Parser where
import Syntax
import Lexer
}

%name parse
%tokentype {Token}
%error {parseError}

%token
    int       {TokenInt $$}
    var       {TokenVar $$}
    '+'       {TokenPlus}
    '-'       {TokenMinus}
    '*'       {TokenTimes}
    '/'       {TokenDiv}
    '='       {TokenEq}
    '('       {TokenLParen}
    ')'       {TokenRParen}
    'let'     {TokenLet}
    'in'      {TokenIn}
    'if'      {TokenIf}
    'then'    {TokenThen}
    'else'    {TokenElse}
    'true'    {TokenTrue}
    'false'   {TokenFalse}
    '<'       {TokenLess}
    '<='      {TokenLessEq}
    '>'       {TokenGreater}
    '>='      {TokenGreaterEq}
    '=='      {TokenEqEq}
    '!='      {TokenNotEq}
    '.'       {TokenInvalid $$}

%left '<' '<=' '>' '>=' '==' '!='
%left '+' '-'
%left '*' '/'
%nonassoc 'if' 'then' 'else'

%%

Program : Expr                            { Program $1 }

Expr : 'let' var '=' Expr 'in' Expr       { Let $2 $4 $6 }
     | 'if' Expr 'then' Expr 'else' Expr  { If $2 $4 $6 }
     | Expr1                              { $1 }
     | '.'                                { Invalid } 
     | '.' Expr                           { Invalid }

Expr1 : Expr2                             { $1 }
      | Expr1 '+' Expr2                   { Binop Plus $1 $3 }
      | Expr1 '-' Expr2                   { Binop Minus $1 $3 }

Expr2 : Expr3                             { $1 }
      | Expr2 '*' Expr3                   { Binop Times $1 $3 }
      | Expr2 '/' Expr3                   { Binop Div $1 $3 }

Expr3 : Expr4                             { $1 }
      | Expr3 '<' Expr4                   { Binop Less $1 $3 }
      | Expr3 '<=' Expr4                  { Binop LessEq $1 $3 }
      | Expr3 '>' Expr4                   { Binop Greater $1 $3 }
      | Expr3 '>=' Expr4                  { Binop GreaterEq $1 $3 }
      | Expr3 '==' Expr4                  { Binop Eq $1 $3 }
      | Expr3 '!=' Expr4                  { Binop NotEq $1 $3 }

Expr4 : int                               { IntLit $1 }
      | var                               { Var $1 }
      | 'true'                            { BoolLit True }
      | 'false'                           { BoolLit False }
      | '(' Expr ')'                      { $2 }

{
parseError :: [Token] -> a
parseError [] = error "Parse error: unexpected end of input."
parseError tokens = error $ "Parse error near: " ++ show (take 10 tokens)
}
