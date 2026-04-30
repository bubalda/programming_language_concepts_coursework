module Lang.Repl.Commands
  ( handleCommand,
    commandPrefix,
    setFlag,
    displayHelp,
  )
where

import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map
import Lang.Eval.Print (renderEval)
import Lang.Repl.Env
  ( ReplEnv (..),
    ReplFlags (..),
    deleteTempState,
    debugFlags,
    releaseFlags,
    rememberHistory,
    resetReplState,
    saveReplState,
  )
import Lang.Repl.Helper (putErrorRepl, putInfoRepl, putSuccessRepl, uppercase, lowerFirst)
import System.Console.Haskeline (InputT)
import System.Exit (exitSuccess)
import Lang.Syntax.Syntax
import Lang.TypeChecker.Types

-- Repl commands (like ghci)
-- Debug mode also switches :tokens and :ast to true.
handleCommand :: ReplEnv -> String -> InputT IO ReplEnv
handleCommand rEnv line = do
  let rEnv' = rememberHistory line rEnv
  case words line of
    [":?"] -> displayHelp >> return rEnv'
    [":help"] -> displayHelp >> return rEnv'
    [":q"] -> quitRepl rEnv'
    [":quit"] -> quitRepl rEnv'
    [":debug"] -> showFlag (\f -> debugMode f) "Debug Mode" rEnv'
    [":debug", val] -> setFlag (\_ b -> if b then debugFlags else releaseFlags) "Debug Mode" val rEnv'
    [":tokens"] -> showFlag (\f -> showTokens f) "Show Tokens" rEnv'
    [":tokens", val] -> setFlag (\f b -> f {showTokens = b}) "Show Tokens" val rEnv'
    [":ast"] -> showFlag (\f -> showAST f) "Show AST" rEnv'
    [":ast", val] -> setFlag (\f b -> f {showAST = b}) "Show AST" val rEnv'
    [":evalPretty"] -> showFlag (\f -> prettyEval f) "Prettify Evaluator Result" rEnv'
    [":evalPretty", val] -> setFlag (\f b -> f {prettyEval = b}) "Prettify Evaluator Result" val rEnv'
    [":env"] -> displayEnv rEnv' >> return rEnv'
    [":history"] -> displayHistory rEnv' >> return rEnv'
    [":reset"] -> putSuccessRepl "Cleared saved variables and recorded history." >> return (resetReplState rEnv')
    _ -> putErrorRepl "Unknown command. Check :? for help." >> return rEnv'

commandPrefix :: String
commandPrefix = ":"

onFlag :: String
onFlag = "ON"

offFlag :: String
offFlag = "OFF"

showFlag :: (ReplFlags -> Bool) -> String -> ReplEnv -> InputT IO ReplEnv
showFlag getter title rEnv = putInfoRepl (title ++ ": " ++ status) >> return rEnv
  where
    status = if (getter . replFlags) rEnv then onFlag else offFlag

setFlag :: (ReplFlags -> Bool -> ReplFlags) -> String -> String -> ReplEnv -> InputT IO ReplEnv
setFlag setter title v rEnv
  | val == onFlag = putInfoRepl (title ++ ": " ++ onFlag) >> return rEnv {replFlags = setter (replFlags rEnv) True}
  | val == offFlag = putInfoRepl (title ++ ": " ++ offFlag) >> return rEnv {replFlags = setter (replFlags rEnv) False}
  | otherwise = putErrorRepl ("Usage: :command <" ++ onFlag ++ " | " ++ offFlag ++ ">") >> return rEnv
  where
    val = uppercase v

displayHelp :: InputT IO ()
displayHelp = do
  putInfoRepl ""
  putInfoRepl "Available commands:"
  mapM_
    putInfoRepl
    [ "  :? / :help                Show this help page",
      "  :q / :quit                Exit the current REPL session",
      "  :debug [on|off]           Toggle debug mode",
      "  :tokens [on|off]          Show lexer output",
      "  :ast [on|off]             Show parser output",
      "  :evalPretty [on|off]      Pretty print evaluation results",
      "  :env                      Show saved variables",
      "  :history                  Show command history",
      "  :reset                    Clear saved variables and recorded history",
      "  Use a trailing \\ to continue onto the next line, and end each statement with ;"
    ]
  putInfoRepl ""

displayEnv :: ReplEnv -> InputT IO ()
displayEnv rEnv
  | Map.null (programEnv rEnv) = putInfoRepl "No saved variables."
  | otherwise = do
      putInfoRepl "Saved variables:"
      mapM_ (putInfoRepl . renderEnvEntry) (Map.toList (programEnv rEnv))
  where
    renderEnvEntry (name, value) = 
      let typeStr = case Map.lookup name (typeEnv rEnv) of
                Just TDynamic -> "dynamic "   
                Just t        -> lowerFirst (prettyPrintType t) ++ " " 
                Nothing       -> ""           
         in "  " ++ typeStr ++ name ++ " = " ++ renderEval value

displayHistory :: ReplEnv -> InputT IO ()
displayHistory rEnv
  | null (replHistory rEnv) = putInfoRepl "History is empty."
  | otherwise = do
      putInfoRepl "History:"
      mapM_ (putInfoRepl . renderHistory) (zip [(1 :: Int) ..] (replHistory rEnv))
  where
    renderHistory (idx, entry) = "  " ++ show idx ++ ". " ++ unwords (words entry)

quitRepl :: ReplEnv -> InputT IO ReplEnv
quitRepl rEnv = do
  putSuccessRepl "Session closed. Goodbye!"
  liftIO (saveReplState rEnv)
  liftIO deleteTempState
  liftIO exitSuccess
