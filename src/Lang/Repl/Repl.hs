module Lang.Repl.Repl (repl) where

import Control.Monad.IO.Class (liftIO)
import Data.List (isPrefixOf)
import Lang.Repl.Commands (commandPrefix, handleCommand)
import Lang.Repl.Env (ReplEnv (..))
import Lang.Repl.Helper (isBlankLine, replEOFExit, replPrompt, replWelcome)
import Lang.Repl.Runner (runLine)
import System.Console.Haskeline
  ( InputT,
    defaultSettings,
    getInputLine,
    runInputT,
  )

repl :: ReplEnv -> IO ()
repl replEnv = do
  replWelcome
  runInputT defaultSettings (loop replEnv)
  where
    loop :: ReplEnv -> InputT IO ()
    loop rEnv = do
      minput <- getInputLine replPrompt
      case minput of
        Nothing -> liftIO replEOFExit -- Exit on EOF
        Just line
          | isBlankLine line -> loop rEnv -- Ignore blank lines
          | commandPrefix `isPrefixOf` line -> handleCommand loop rEnv line
          | otherwise -> do
              let addEndSemiColon = if last line /= ';' then line ++ ";" else line
              rEnv' <- runLine rEnv addEndSemiColon
              loop rEnv'