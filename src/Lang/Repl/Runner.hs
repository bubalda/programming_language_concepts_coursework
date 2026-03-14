module Lang.Repl.Runner (runLine) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Lang.Lexer.Lexer (runLexer, printTokens)
import Lang.Parser.Eval (evalStmt)
import Lang.Parser.Parser (runParser, printAST)
import Lang.Repl.Env (ReplEnv (replFlags, programEnv), ReplFlags (showTokens, showAST))
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
      when (showTokens (replFlags rEnv)) $ liftIO $ printTokens tokens

      -- Parse --
      case runParser tokens of
        Left err -> do
          liftIO $ putStrLn err -- Parse error
          return rEnv -- Reprompt a new line with current env
        Right ast -> do
          when (showAST (replFlags rEnv)) $ liftIO $ printAST ast
          -- Evaluate --
          let (env2, val) = evalStmt (programEnv rEnv) ast
          liftIO $ putStrLn (show val ++ "\n")
          return rEnv {programEnv = env2} -- Pass new env to next iteration

