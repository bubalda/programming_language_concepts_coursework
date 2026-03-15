module Lang.Repl.Helper
  ( replWelcome,
    replPrompt,
    replEOFExit,
    isBlankLine,
    wrapSection,
    uppercase,
    putStrLnRepl,
  )
where

import System.Exit (exitSuccess)
import Data.Char (toUpper)
import Control.Monad.IO.Class (liftIO)
import System.Console.Haskeline (InputT)

-- Repl Prints and Checks
replWelcome :: IO ()
replWelcome = putStrLn $ "Use :q or EOF (Ctrl+D) to quit, and Ctrl+C to force stop the program."

replPrompt :: String
replPrompt = "c2> "

replEOFExit :: IO ()
replEOFExit = putStrLn "Received EOF, leaving C2Repl." >> exitSuccess

isBlankLine :: (Foldable t) => t Char -> Bool
isBlankLine line = all (`elem` " \t\n\r") line

putStrLnRepl :: String -> InputT IO ()
putStrLnRepl = liftIO . putStrLn

-- Pretty Print
-- The title bar length
headerWidth :: Int
headerWidth = 120

duplicate :: String -> Int -> String
duplicate s n = concat (replicate n s)


wrapSection :: String -> IO () -> IO ()
wrapSection title content = do
  putStrLn ""
  putStrLn header
  content
  where
    middle = " " ++ title ++ " "
    padding = headerWidth - length middle
    leftPad = padding `div` 2
    rightPad = padding - leftPad
    header = duplicate "=" leftPad ++ middle ++ duplicate "=" rightPad


uppercase :: String -> String
uppercase = map toUpper