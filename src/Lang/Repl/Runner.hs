module Lang.Repl.Runner (runLine) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Lang.Lexer.Lexer (runLexer)
import Lang.Lexer.Tokens (printTokens)
import Lang.Parser.Eval (evalStmt)
import Lang.Parser.Parser (runParser)
import Lang.Repl.Env (ReplEnv (flags, programEnv), ReplFlags (showTokens))
import System.Console.Haskeline (InputT)

runLine :: ReplEnv -> String -> InputT IO ReplEnv
runLine rEnv line = do
  -- Lex --
  case runLexer line of
    Left err -> do
      liftIO $ putStrLn err -- Lexer error
      return rEnv -- Reprompt a new line with current env
    Right tokens -> do
      -- Show read tokens. Hide / Display this by setting flag ":tokens on/off"
      when (showTokens (flags rEnv)) $ liftIO $ printTokens tokens

      -- Parse --
      case runParser tokens of
        Left err -> do
          liftIO $ putStrLn err -- Parse error
          return rEnv -- Reprompt a new line with current env
        Right stmt -> do
          -- Evaluate --
          let (env2, val) = evalStmt (programEnv rEnv) stmt
          liftIO $ putStrLn (show val ++ "\n")
          return rEnv {programEnv = env2} -- Pass new env to next iteration