{
module Lang.Parser.Parser where
import Lang.Lexer.Tokens (TokenType(..), Token(..), formatToken, formatTokenType)
import Lang.Parser.Eval (Expr(..), Stmt(..), evalExpr, evalStmt)
}

%name parseStmt
%tokentype { Token }
%monad { Either String } { >>= } { return }
%error { parseError }

%token
    ident                          { Token (TokIdent $$)      _ }
    int                            { Token (TokInt $$)        _ }
    'true'                         { Token TokTrue            _ }
    'false'                        { Token TokFalse           _ }
    '<'                            { Token TokLT              _ }
    '<='                           { Token TokLTE             _ }
    '>'                            { Token TokGT              _ }
    '>='                           { Token TokGTE             _ }
    '=='                           { Token TokEQ              _ }
    '!='                           { Token TokNEQ             _ }
    '+'                            { Token TokPlus            _ }
    '-'                            { Token TokMinus           _ }
    '*'                            { Token TokMultiply        _ }
    '/'                            { Token TokDivision        _ }
    '//'                           { Token TokFloorDiv        _ }
    '%'                            { Token TokModulo          _ }
    '='                            { Token TokAssign          _ }
    '\\'                           { Token TokEscape          _ }
    '!'                            { Token TokNot             _ }
    '.'                            { Token TokDot             _ }
    ','                            { Token TokComma           _ }
    ':'                            { Token TokColon           _ }
    ';'                            { Token TokSemiColon       _ }
    '('                            { Token TokLParen          _ }
    ')'                            { Token TokRParen          _ }
    '['                            { Token TokLBrack          _ }
    ']'                            { Token TokRBrack          _ }
    '{'                            { Token TokLSQBrack        _ }
    '}'                            { Token TokRSQBrack        _ }
    'var'                          { Token TokVar             _ }
    'if'                           { Token TokIf              _ }
    'else'                         { Token TokElse            _ }
    'fun'                          { Token TokFun             _ }

%left '+' '-'
%left '*' '/'

%%

Stmt
  : ident '=' Expr    { Assign $1 $3 }
  | Expr              { ExprStmt $1 }
  
Expr
    : Expr '+' Expr   { Add $1 $3 }
    | Expr '-' Expr   { Sub $1 $3 }
    | Expr '*' Expr   { Mul $1 $3 }
    | Expr '/' Expr   { Div $1 $3 }
    | '(' Expr ')'    { $2 }
    | int             { IntLit $1 }
    | ident           { Var $1 }


{
parserIgnore :: TokenType -> Bool
parserIgnore TokEOF         = True
parserIgnore (TokError _) = True
parserIgnore _              = False

parseError :: [Token] -> Either String a
parseError [] = Left "No tokens to parse"
parseError toks =
    Left $ "Parse error at token: " ++ formatToken (head toks)                         -- Show error parsing
    ++ ".\nFor context: " ++ unwords (map (formatTokenType . tokenType) (take 5 toks)) -- Help navigate to code part 
    
runParser :: [Token] -> Either String Stmt
runParser toks = parseStmt (filter (not . parserIgnore . tokenType) toks)
}