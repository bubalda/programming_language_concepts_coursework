module Main (main) where

import Lang.Repl.Repl (repl)
import Lang.Repl.Env (debugRepl)

main :: IO ()
main = do repl debugRepl