module Lang.Lexer.Helper (printTokens) where

import Lang.Repl.Helper (wrapSection, formatPos)
import Lang.Lexer.Tokens (Token(..), TokenType (TokEOF, TokError), TokenPos(..))

-- Debug printer
printTokens :: [Token] -> IO ()
printTokens tokens = wrapSection "Tokens" (mapM_ printToken tokens)
  where
    printToken :: Token -> IO ()
    printToken (Token TokEOF _) = return () -- Hide TokEOF
    printToken (Token (TokError s) p) = putStrLn $ formatTokenPos p ++ "<LEXER ERROR> -- Could not tokenize string " ++ show s
    printToken t = putStrLn $ formatToken t

    formatToken :: Token -> String
    formatToken (Token t p) = formatTokenPos p ++ show t ++ ""

    formatTokenPos :: TokenPos -> String
    formatTokenPos (TokenPos l c) = formatPos l c