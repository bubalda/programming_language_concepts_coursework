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

  '&'                            { Token TokBinAnd          _ }
  '|'                            { Token TokBinOr           _ }
  '^'                            { Token TokBinXOR          _ }

  'var'                          { Token TokVar             _ }
  'if'                           { Token TokIf              _ }
  'else'                         { Token TokElse            _ }
  'func'                         { Token TokFunc            _ }

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
parseError :: [Token] -> Either String a
parseError [] = Left "No tokens to parse"
parseError toks =
    Left $ "Parse error at token: " ++ formatToken (head toks)              -- Show error parsing
    ++ ".\nFor context: " ++ unwords (map (show . tokenType) (take 5 toks)) -- Help navigate to code part 
    
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