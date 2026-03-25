{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$white = [\ \t]
$newline = [\n\r]

tokens :-
  $white+                           ;
  $newline+                         ;
  "--".*                            ;
  $digit+                           { \s -> TokenInt (read s) }
    

  -- Keywords

  "let"                             { \s -> TokenLet }
  "in"                              { \s -> TokenIn }
  "if"                              { \s -> TokenIf }
  "then"                            { \s -> TokenThen }
  "else"                            { \s -> TokenElse }
  "true"                            { \s -> TokenTrue }
  "false"                           { \s -> TokenFalse }

  -- Operators

  "+"                               { \s -> TokenPlus }
  "-"                               { \s -> TokenMinus }
  "*"                               { \s -> TokenTimes }
  "/"                               { \s -> TokenDiv }
  
  "="                               { \s -> TokenEq }
  "("                               { \s -> TokenLParen }
  ")"                               { \s -> TokenRParen }
  
  "<"                               { \s -> TokenLess }
  "<="                              { \s -> TokenLessEq }
  ">"                               { \s -> TokenGreater }
  ">="                              { \s -> TokenGreaterEq }
  "=="                              { \s -> TokenEqEq }
  "!="                              { \s -> TokenNotEq }

  -- Variables must appear after keywords so that reserved words match first.
  $alpha [$alpha $digit \']*        { \s -> TokenVar s }
  .                                 { \s -> TokenInvalid s }
{
data Token = TokenInt Int
           | TokenVar String
           | TokenPlus | TokenMinus | TokenTimes | TokenDiv
           | TokenEq
           | TokenLParen | TokenRParen
           | TokenLet | TokenIn
           | TokenIf | TokenThen | TokenElse
           | TokenTrue | TokenFalse
           | TokenLess | TokenLessEq | TokenGreater | TokenGreaterEq
           | TokenEqEq | TokenNotEq | TokenInvalid String
           deriving (Show, Eq)
}
