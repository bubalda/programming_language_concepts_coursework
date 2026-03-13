{
module Lang.Parser.Parser where
import Lang.Lexer.Lexer (Alex, runAlex)
import Lang.Lexer.Tokens (TokenType(..))
import Lang.Parser.Eval (Expr(..), Stmt(..), evalExpr, evalStmt)
}

%name parseStmt
%tokentype { TokenType }
%monad { Either String } { >>= } { return }
%error { parseError }

%token
    ident                          { TokIdent $$      _ }

    int                            { TokInt $$        _ }
    'true'                         { TokTrue          _ }
    'false'                        { TokFalse         _ }

    '<'                            { TokLT            _ }
    '<='                           { TokLTE           _ }
    '>'                            { TokGT            _ }
    '>='                           { TokGTE           _ }
    '=='                           { TokEQ            _ }
    '!='                           { TokNEQ           _ }

    '+'                            { TokPlus          _ }
    '-'                            { TokMinus         _ }
    '*'                            { TokMultiply      _ }
    '/'                            { TokDivision      _ }
    '//'                           { TokFloorDiv      _ }
    '%'                            { TokModulo        _ }

    '='                            { TokAssign        _ }
    '\\'                           { TokEscape        _ }
    '.'                            { TokDot           _ }
    ','                            { TokComma         _ }
    ':'                            { TokColon         _ }
    ';'                            { TokSemiColon     _ }

    '('                            { TokLParen        _ }
    ')'                            { TokRParen        _ }
    '['                            { TokLBrack        _ }
    ']'                            { TokRBrack        _ }
    '{'                            { TokLSQBrack      _ }
    '}'                            { TokRSQBrack      _ }

    'var'                          { TokVar           _ }
    'if'                           { TokIf            _ }
    'else'                         { TokElse          _ }
    'fun'                          { TokFun           _ }

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
parserIgnore (TokError _ _) = True
parserIgnore _              = False

parseError :: [TokenType] -> Either String a
parseError [] = Left "No tokens to parse"
parseError toks = Left $ "Parse error at token: " ++ show (head toks)

runParser :: [TokenType] -> Either String Stmt
runParser toks = 
    let validTokens = filter (not . parserIgnore) toks 
    in case parseStmt validTokens of
        Left err -> Left ("Parser error: " ++ show err)
        Right val -> Right val
}