module Lang.Repl.Commands
  ( handleCommand,
    commandPrefix,
    setFlag,
    displayHelp,
  )
where

import Control.Monad.IO.Class (liftIO)
import Lang.Repl.Env (ReplEnv (replFlags), ReplFlags (showTokens, showAST))
import Lang.Repl.Helper (uppercase, wrapSection)
import System.Console.Haskeline (InputT)
import System.Exit (exitSuccess)

handleCommand :: (ReplEnv -> InputT IO ()) -> ReplEnv -> String -> InputT IO ()
handleCommand loop rEnv line = do
  case words line of
    [":?"] -> liftIO displayHelp >> loop rEnv
    [":q"] -> liftIO exitSuccess
    [":tokens"] -> showFlag (\f -> showTokens f) "Show Tokens" loop rEnv
    [":tokens", val] -> setFlag (\f b -> f {showTokens = b}) "Show Tokens" val loop rEnv
    [":ast"] -> showFlag (\f -> showAST f) "Show AST" loop rEnv
    [":ast", val] -> setFlag (\f b -> f {showAST = b}) "Show AST" val loop rEnv
    _ -> liftIO (putStrLn ("Unknown command. Check :? for help.")) >> loop rEnv

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
showFlag getter title loop rEnv = liftIO (putStrLn (title ++ ": " ++ status ++ "\n")) >> loop rEnv
  where
    status = if (getter . replFlags) rEnv then "ON" else "OFF"

setFlag :: (ReplFlags -> Bool -> ReplFlags) -> String -> String -> (ReplEnv -> InputT IO ()) -> ReplEnv -> InputT IO ()
setFlag setter title v loop rEnv
  | val == onFlag = liftIO (putStrLn (title ++ ": " ++ onFlag ++ "\n")) >> loop rEnv {replFlags = setter (replFlags rEnv) True}
  | val == offFlag = liftIO (putStrLn (title ++ ": " ++ offFlag ++ "\n")) >> loop rEnv {replFlags = setter (replFlags rEnv) False}
  | otherwise = liftIO (putStrLn ("Usage: :tokens <" ++ onFlag ++ " | " ++ offFlag ++ ">" ++ "\n")) >> loop rEnv
  where
    val = uppercase v -- Uppercase user input for comparison

-- Pages
displayHelp :: IO ()
displayHelp = do
  wrapSection "Help" helpSection
  where
    helpSection = do
      putStrLn "TODO: Help Page here..."
