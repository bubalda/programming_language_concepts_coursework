module Lang.Repl.Runner (runLine) where

import Control.Monad (when)
import Control.Monad.IO.Class (liftIO)
import Lang.Eval.Eval (evalStmt, runEval)
import Lang.Eval.Print (printEval, printEvalPretty)
import Lang.Lexer.Helper (printTokens)
import Lang.Lexer.Lexer (runLexer)
import Lang.Lexer.Tokens (Token (..), TokenPos (..), TokenType (TokSemiColon))
import Lang.Parser.Expr (Stmt)
import Lang.Parser.Helper (printAST)
import Lang.Parser.Parser (runParser)
import Lang.Repl.Env (ReplEnv (programEnv, replFlags), ReplFlags (..))
import Lang.Repl.Helper (putStrLnRepl)
import System.Console.Haskeline (InputT)

-- Runs the user input
runLine :: ReplEnv -> String -> InputT IO ReplEnv
runLine rEnv execLine = do
  -- Lex --
  case runLexer execLine of
    Left err -> errReturn err rEnv -- Lexer error
    Right tokens -> do
      -- Show read tokens. Hide / Display this by setting flag ":tokens on/off"
      when ((showTokens . replFlags) rEnv) $ liftIO $ printTokens tokens

      -- Tokens will always have TokEOF so for [Tokens] not to be empty for parsing this must be done
      if (length tokens < 2)
        then return rEnv
        else
          -- Provides a ending semicolon to mimic python's repl
          -- Parse --
          case runParser execLine (endSemicolon tokens) of
            Left err -> errReturn err rEnv -- Parse error
            Right stmts -> do
              when ((showAST . replFlags) rEnv) $ liftIO $ printAST stmts
              evalLoop rEnv stmts 1
  where
    -- Show error, and reprompt a new line with current env
    errReturn :: String -> ReplEnv -> InputT IO ReplEnv
    errReturn err rEnv = putStrLnRepl err >> return rEnv

    endSemicolon tokens =
      let lastTok = last tokens
          lastPos = tokenPos lastTok
       in if (tokenType lastTok /= TokSemiColon && (semicolonEnd . replFlags) rEnv)
            then tokens ++ [Token {tokenType = TokSemiColon, tokenPos = (TokenPos {line = (line lastPos), column = (column lastPos + 1)})}]
            else tokens

    -- Evaluate --
    evalLoop :: ReplEnv -> [Stmt] -> Int -> InputT IO ReplEnv
    evalLoop rEnv [] _ = return rEnv
    evalLoop rEnv (s : ss) acc =
      case runEval (evalStmt (programEnv rEnv) s) of
        Left err -> errReturn err rEnv
        Right (env', val) -> do
          if (prettyEval . replFlags) rEnv
            then liftIO $ printEvalPretty val acc >> putStrLn ""
            else liftIO $ printEval val
          evalLoop (rEnv {programEnv = env'}) ss (acc + 1) -- Pass new env to next iteration