module Lang.Repl.Repl (repl) where

import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf)
import Lang.Repl.Commands (commandPrefix, handleCommand)
import Lang.Repl.Env (ReplEnv, historyFilePath, rememberHistory, saveReplState)
import Lang.Repl.Helper
  ( isBlankLine,
    normalizeLeadingColon,
    replContinuationPrompt,
    replEOFExit,
    replPrompt,
    replWelcome,
  )
import Lang.Repl.Runner (runLine)
import System.Console.Haskeline
  ( InputT,
    Settings (..),
    defaultSettings,
    getInputLine,
    runInputT,
  )

-- Allows users to type commands and run it interactively.
repl :: ReplEnv -> IO ()
repl replEnv = do
  histPath <- historyFilePath
  replWelcome
  runInputT (defaultSettings {historyFile = Just histPath}) (loop replEnv)
  where
    loop :: ReplEnv -> InputT IO ()
    loop rEnv = do
      minput <- readReplInput replPrompt
      case minput of
        Nothing -> liftIO replEOFExit
        Just rawLine
          | isBlankLine rawLine -> loop rEnv
          | commandPrefix `isPrefixOf` line -> do
              nextEnv <- handleCommand rEnv line
              liftIO (saveReplState nextEnv)
              loop nextEnv
          | otherwise -> do
              let rEnv' = rememberHistory line rEnv
              nextEnv <- runLine rEnv' line
              liftIO (saveReplState nextEnv)
              loop nextEnv
          where
            line = normalizeLeadingColon rawLine

readReplInput :: String -> InputT IO (Maybe String)
readReplInput prompt = do
  minput <- getInputLine prompt
  case minput of
    Nothing -> return Nothing
    Just line
      | needsContinuation line -> collectMultiline [removeContinuationMarker line]
      | otherwise -> return (Just line)

collectMultiline :: [String] -> InputT IO (Maybe String)
collectMultiline linesSoFar = do
  minput <- getInputLine replContinuationPrompt
  case minput of
    Nothing -> return (Just (unlines linesSoFar))
    Just line
      | needsContinuation line ->
          collectMultiline (linesSoFar ++ [removeContinuationMarker line])
      | otherwise ->
          return (Just (unlines (linesSoFar ++ [line])))

needsContinuation :: String -> Bool
needsContinuation line =
  case reverse (dropWhile (`elem` [' ', '\t', '\n', '\r']) line) of
    '\\' : _ -> True
    _ -> False

removeContinuationMarker :: String -> String
removeContinuationMarker line =
  reverse $
    case reverse (dropWhile (`elem` [' ', '\t', '\n', '\r']) line) of
      '\\' : rest -> rest
      trimmed -> trimmed
