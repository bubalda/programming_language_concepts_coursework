module Lang.Parser.Helper (parserIgnore, renderError, formatRenderError, printAST) where

import Lang.Lexer.Tokens (TokenType (TokEOF))
import Lang.Parser.Expr ( Stmt )
import Lang.Repl.Helper (wrapSection, formatPos)

parserIgnore :: TokenType -> Bool
parserIgnore TokEOF = True
parserIgnore _ = False


renderError :: String -> Int -> Int -> String -> String
renderError src line col msg =
  "\n[!] " ++ formatPos line col ++ msg ++ "\n" ++
  "    | Line " ++ show line ++ ", Column " ++ show col ++ "\n" ++
  "    |\n" ++
  "    | " ++ srcLine ++ "\n" ++
  "    | " ++ replicate (col - 1) ' ' ++ "^^^^^^\n"
  where
    -- Guard against empty strings or out of bounds
    srcLines = lines src
    srcLine = if line <= length srcLines then srcLines !! (line - 1) else "<source unavailable>"


formatRenderError :: String -> String -> Either String [Stmt]
formatRenderError src err =
    case break (== ':') err of
      (lineStr, ':' : rest) ->
        case break (== ':') rest of
          (colStr, ':' : msg) ->
            let l = read lineStr
                c = read colStr
             in Left $ renderError src l c msg
          _ -> Left err
      _ -> Left err

printAST :: [Stmt] -> IO ()
printAST asts = wrapSection "Abstract Syntax Tree (AST)" (mapM_ (putStrLn . show) asts)