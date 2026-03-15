module Main (main) where

import CLI.Args (getReplFlag)
import qualified Data.Map as Map
import Lang.Repl.Env (ReplEnv (..))
import Lang.Repl.Repl (repl)

-- Change to debug flags if requiring debug mode (Shows tokens and ast, and pretty prints stuff)
-- Run Commands
-- `stack run -- --debug`  (set debug flags, -d is also debug)
-- `stack run`             (default release flags)
main :: IO ()
main = do
  flag <- getReplFlag
  repl (ReplEnv {programEnv = Map.empty, replFlags = flag})