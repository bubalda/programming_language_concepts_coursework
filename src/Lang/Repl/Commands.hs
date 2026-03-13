module Lang.Repl.Commands
  ( handleCommand,
    commandPrefix,
    setFlag,
    displayHelp,
  )
where

import Control.Monad.IO.Class (liftIO)
import Lang.Repl.Env (ReplEnv (flags), ReplFlags (showTokens))
import Lang.Repl.Helper (wrapSection)
import System.Console.Haskeline (InputT)
import System.Exit (exitSuccess)

handleCommand :: (ReplEnv -> InputT IO ()) -> ReplEnv -> String -> InputT IO ()
handleCommand loop rEnv line = do
  case words line of
    [":?"] -> liftIO displayHelp >> loop rEnv
    [":q"] -> liftIO exitSuccess
    [":tokens", val] -> setFlag (\f b -> f {showTokens = b}) "Tokens" val rEnv loop
    _ -> liftIO (putStrLn ("Unknown command. Check :? for help.")) >> loop rEnv

-- Repl Commands (TODO mapper)
-- Constants
commandPrefix :: String
commandPrefix = ":"

onFlag :: String
onFlag = "on"

offFlag :: String
offFlag = "off"

-- Setter
setFlag :: (ReplFlags -> Bool -> ReplFlags) -> String -> String -> ReplEnv -> (ReplEnv -> InputT IO ()) -> InputT IO ()
setFlag setter title val rEnv loop
  | val == onFlag = liftIO (putStrLn (title ++ ": ON")) >> loop rEnv {flags = setter (flags rEnv) True}
  | val == offFlag = liftIO (putStrLn (title ++ ": OFF")) >> loop rEnv {flags = setter (flags rEnv) False}
  | otherwise = liftIO (putStrLn ("Usage: :tokens <" ++ onFlag ++ " | " ++ offFlag ++ ">")) >> loop rEnv

-- Pages
displayHelp :: IO ()
displayHelp = do
  wrapSection "Help" helpSection
  where
    helpSection = do
      putStrLn "TODO: Help Page here..."
