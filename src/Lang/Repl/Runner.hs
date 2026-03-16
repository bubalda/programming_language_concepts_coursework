module Lang.Repl.Runner (runLine) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Lang.Eval.Eval (evalStmt, runEval)
import Lang.Eval.Print (printEval, printEvalPretty)
import Lang.Lexer.Lexer (printTokens, runLexer)
import Lang.Parser.Parser (printAST, runParser)
import Lang.Repl.Env (ReplEnv (programEnv, replFlags), ReplFlags (..))
import Lang.Repl.Helper (putStrLnRepl)
import System.Console.Haskeline (InputT)

runLine :: ReplEnv -> String -> InputT IO ReplEnv
runLine rEnv line = do
  -- Lex --
  case runLexer line of
    Left err -> errReturn err rEnv -- Lexer error
    Right tokens -> do
      -- Show read tokens. Hide / Display this by setting flag ":tokens on/off"
      when ((showTokens . replFlags) rEnv) $ liftIO $ printTokens tokens

      -- Parse --
      case runParser tokens of
        Left err -> errReturn err rEnv -- Parse error
        Right ast -> do
          when ((showAST . replFlags) rEnv) $ liftIO $ printAST ast

          -- Evaluate --
          case runEval (evalStmt (programEnv rEnv) ast) of
            Left err -> errReturn err rEnv -- Evaluator error
            Right (env', val) -> do
              if (prettyEval . replFlags) rEnv
                then liftIO $ printEvalPretty val >> putStrLn ""
                else liftIO $ printEval val
              return rEnv {programEnv = env'} -- Pass new env to next iteration
  where
    -- Show error, and reprompt a new line with current env
    errReturn :: String -> ReplEnv -> InputT IO ReplEnv
    errReturn err env = putStrLnRepl err >> return env