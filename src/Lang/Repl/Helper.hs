module Lang.Repl.Helper
  ( replWelcome,
    replPrompt,
    replContinuationPrompt,
    replEOFExit,
    isBlankLine,
    wrapSection,
    uppercase,
    lowerFirst,
    putStrLnRepl,
    putInfoRepl,
    putSuccessRepl,
    putErrorRepl,
    putInfoLn,
    putSuccessLn,
    putErrorLn,
    colorize,
    normalizeLeadingColon,
    formatPos,
  )
where

import Control.Monad.IO.Class (liftIO)
import Data.Char (toUpper, toLower)
import System.Console.Haskeline (InputT)
import System.Exit (exitSuccess)

-- Repl prints and checks.
replWelcome :: IO ()
replWelcome = do
  putInfoLn "Welcome to C2Repl."
  putStrLn "Use :? or :help for commands."
  putStrLn "Use :q or EOF (Ctrl+Z then Enter on Windows) to quit."

replPrompt :: String
replPrompt = "c2> "

replContinuationPrompt :: String
replContinuationPrompt = "... "

replEOFExit :: IO ()
replEOFExit = putSuccessLn "Received EOF, leaving C2Repl." >> exitSuccess

isBlankLine :: Foldable t => t Char -> Bool
isBlankLine line = all (`elem` " \t\n\r") line

putStrLnRepl :: String -> InputT IO ()
putStrLnRepl = liftIO . putStrLn

putInfoRepl :: String -> InputT IO ()
putInfoRepl = liftIO . putInfoLn

putSuccessRepl :: String -> InputT IO ()
putSuccessRepl = liftIO . putSuccessLn

putErrorRepl :: String -> InputT IO ()
putErrorRepl = liftIO . putErrorLn

putInfoLn :: String -> IO ()
putInfoLn = putStrLn . colorize cyanCode

putSuccessLn :: String -> IO ()
putSuccessLn = putStrLn . colorize greenCode

putErrorLn :: String -> IO ()
putErrorLn = putStrLn . colorize redCode . ("Error: " ++)

-- Pretty print.
headerWidth :: Int
headerWidth = 120

duplicate :: String -> Int -> String
duplicate s n = concat (replicate n s)

wrapSection :: String -> IO () -> IO ()
wrapSection title content = do
  putStrLn header
  content
  putStrLn ""
  where
    middle = " " ++ title ++ " "
    padding = headerWidth - length middle
    leftPad = padding `div` 2
    rightPad = padding - leftPad
    header = duplicate "=" leftPad ++ middle ++ duplicate "=" rightPad

uppercase :: String -> String
uppercase = map toUpper

colorize :: String -> String -> String
colorize code text = "\ESC[" ++ code ++ "m" ++ text ++ "\ESC[0m"

normalizeLeadingColon :: String -> String
normalizeLeadingColon ('\xff1a' : rest) = ':' : rest
normalizeLeadingColon line = line

redCode, greenCode, cyanCode :: String
redCode = "31;1"
greenCode = "32;1"
cyanCode = "36;1"

-- Allows VSCode shortcut to code position.
formatPos :: Int -> Int -> String
formatPos l c = show l ++ ":" ++ show c ++ ": "

lowerFirst :: String -> String
lowerFirst [] = []
lowerFirst (x:xs) = toLower x : xs