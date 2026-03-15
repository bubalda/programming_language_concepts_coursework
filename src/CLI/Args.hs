module CLI.Args where

import Lang.Repl.Env (ReplFlags, debugFlags, releaseFlags)
import System.Environment (getArgs)

getReplFlag :: IO ReplFlags
getReplFlag = do
  args <- getArgs
  let flags
        | "-d" `elem` args || "--debug" `elem` args = debugFlags
        | otherwise = releaseFlags
  return flags