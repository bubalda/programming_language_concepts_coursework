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

-- TODO @jinyi
-- 1. 现在你看内个 handleCommand 的 case 里面几乎每个 command 都有两行 (setFlag / showFlag), 可以把他们浓缩成一行吗？
-- 2. 那个 help page (:?) 需要写一下
-- 3. 我想把 commandPrefix 和 handleCommand 帮我连在一起， 像是 commandPrefix ++ "debug" => :debug，
--    但是在 runtime 做的话会很费 time/space, 所以可以在程序开始时就直接 init 一个 List / Map 
--    来存取那些 command 吗？
-- 4. 现在的话如果输入 x = 10 eval 会 print x 的 value，不过可以不让他这样做吗？
--    就是让 x = 7; x += 1; x *= 2; x 这整段只返回一行 16
--    然后 x = 7; x += 1; x *= 2;     什么都不返回，因为 x 才会触发 print
-- 5. Autocomplete and persistent history for repl (那边好像有写，又好像没有不过感觉会很不错所以有时间就做吧)

-- Debug mode also switches :tokens and :ast to true
handleCommand :: (ReplEnv -> InputT IO ()) -> ReplEnv -> String -> InputT IO ()
handleCommand loop rEnv line = do
  case words line of
    [":?"] -> liftIO displayHelp >> loop rEnv -- Help page
    [":q"] -> liftIO exitSuccess -- Quits the repl gracefully (Ctrl+D also does the same thing)

    -- Turn on debug mode which turns on :tokens, :ast and :evalPretty
    [":debug"] -> showFlag (\f -> debugMode f) "Debug Mode" loop rEnv
    [":debug", val] -> setFlag (\_ b -> if b then debugFlags else releaseFlags) "Debug Mode" val loop rEnv

    -- Print tokens for debug
    [":tokens"] -> showFlag (\f -> showTokens f) "Show Tokens" loop rEnv
    [":tokens", val] -> setFlag (\f b -> f {showTokens = b}) "Show Tokens" val loop rEnv

    -- Print AST for debug
    [":ast"] -> showFlag (\f -> showAST f) "Show AST" loop rEnv
    [":ast", val] -> setFlag (\f b -> f {showAST = b}) "Show AST" val loop rEnv

    -- Prettify evaluator results
    [":evalPretty"] -> showFlag (\f -> prettyEval f) "Prettify Evaluator Result" loop rEnv
    [":evalPretty", val] -> setFlag (\f b -> f {prettyEval = b}) "Prettify Evaluator Result" val loop rEnv

    -- To mimic normal repl (Py as reference) behaviour (default on)
    [":semicolonEnd"] -> showFlag (\f -> semicolonEnd f) "Auto apply ending semicolon on command line" loop rEnv
    [":semicolonEnd", val] -> setFlag (\f b -> f {semicolonEnd = b}) "Auto apply ending semicolon on command line" val loop rEnv
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
