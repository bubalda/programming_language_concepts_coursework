module Lang.Repl.Helper
  ( replWelcome,
    replPrompt,
    replEOFExit,
    isBlankLine,
    duplicate,
    sectionHalfHeader,
    wrapSection,
  )
where

import System.Exit (exitSuccess)

-- Repl Prints and Checks
replWelcome :: IO ()
replWelcome = putStrLn $ "Use :q or EOF (Ctrl+D) to quit, and Ctrl+C to force stop the program."

replPrompt :: String
replPrompt = "c2> "

replEOFExit :: IO ()
replEOFExit = do
  putStrLn "Received EOF, leaving C2Repl."
  exitSuccess

isBlankLine :: (Foldable t) => t Char -> Bool
isBlankLine line = all (`elem` " \t\n\r") line

-- Pretty Print
duplicate :: String -> Int -> String
duplicate s n = concat (replicate n s)

sectionHalfHeader :: String
sectionHalfHeader = duplicate "=" 60

wrapSection :: String -> IO () -> IO ()
wrapSection title content = do
  putStrLn $ sectionHalfHeader ++ " " ++ title ++ " " ++ sectionHalfHeader
  content
  putStrLn ""
