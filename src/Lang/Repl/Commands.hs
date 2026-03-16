module Lang.Repl.Commands
  ( handleCommand,
    commandPrefix,
    setFlag,
    displayHelp,
  )
where

import Control.Monad.IO.Class (liftIO)
import Lang.Repl.Env (ReplEnv (replFlags), ReplFlags (..), debugFlags, releaseFlags)
import Lang.Repl.Helper (putStrLnRepl, uppercase, wrapSection)
import System.Console.Haskeline (InputT)
import System.Exit (exitSuccess)

-- Debug mode also switches :tokens and :ast to true
handleCommand :: (ReplEnv -> InputT IO ()) -> ReplEnv -> String -> InputT IO ()
handleCommand loop rEnv line = do
  case words line of
    [":?"] -> liftIO displayHelp >> loop rEnv
    [":q"] -> liftIO exitSuccess
    [":debug"] -> showFlag (\f -> debugMode f) "Debug Mode" loop rEnv
    [":debug", val] -> setFlag (\_ b -> if b then debugFlags else releaseFlags) "Debug Mode" val loop rEnv
    [":tokens"] -> showFlag (\f -> showTokens f) "Show Tokens" loop rEnv
    [":tokens", val] -> setFlag (\f b -> f {showTokens = b}) "Show Tokens" val loop rEnv
    [":ast"] -> showFlag (\f -> showAST f) "Show AST" loop rEnv
    [":ast", val] -> setFlag (\f b -> f {showAST = b}) "Show AST" val loop rEnv
    [":evalPretty"] -> showFlag (\f -> prettyEval f) "Prettify Evaluator Result" loop rEnv
    [":evalPretty", val] -> setFlag (\f b -> f {prettyEval = b}) "Prettify Evaluator Result" val loop rEnv
    _ -> putStrLnRepl ("Unknown command. Check :? for help.") >> loop rEnv

-- Repl Commands (TODO mapper)
-- Constants
commandPrefix :: String
commandPrefix = ":"

-- Flag value should be all uppercase (input doesn't have to)
onFlag :: String
onFlag = "ON"

offFlag :: String
offFlag = "OFF"

-- Setter
showFlag :: (ReplFlags -> Bool) -> String -> (ReplEnv -> InputT IO ()) -> ReplEnv -> InputT IO ()
showFlag getter title loop rEnv = putStrLnRepl (title ++ ": " ++ status ++ "\n") >> loop rEnv
  where
    status = if (getter . replFlags) rEnv then "ON" else "OFF"

setFlag :: (ReplFlags -> Bool -> ReplFlags) -> String -> String -> (ReplEnv -> InputT IO ()) -> ReplEnv -> InputT IO ()
setFlag setter title v loop rEnv
  | val == onFlag = putStrLnRepl (title ++ ": " ++ onFlag ++ "\n") >> loop rEnv {replFlags = setter (replFlags rEnv) True}
  | val == offFlag = putStrLnRepl (title ++ ": " ++ offFlag ++ "\n") >> loop rEnv {replFlags = setter (replFlags rEnv) False}
  | otherwise = putStrLnRepl ("Usage: :command <" ++ onFlag ++ " | " ++ offFlag ++ ">" ++ "\n") >> loop rEnv
  where
    val = uppercase v -- Uppercase user input for comparison

-- Pages
displayHelp :: IO ()
displayHelp = do
  wrapSection "Help" helpSection
  where
    helpSection = do
      putStrLn "TODO: Help Page here..."
