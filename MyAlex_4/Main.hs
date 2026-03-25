module Main where

import Lexer
import Parser
import Syntax
import Control.Exception (ErrorCall, evaluate, try)
import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.List (intercalate, isPrefixOf)
import System.Directory (doesFileExist)
import System.Environment (getExecutablePath)
import System.Exit (exitSuccess)
import System.FilePath ((</>), takeDirectory)
import System.IO (hFlush, stdout, isEOF)
import Text.Read (readMaybe)

main :: IO ()
main = do
    state <- loadState
    printBanner state
    loop state

data ReplState = ReplState
    { bindings :: ValEnv
    , historyEntries :: [String]
    }

envFileName :: FilePath
envFileName = ".repl-env"

historyFileName :: FilePath
historyFileName = ".repl-history"

loop :: ReplState -> IO ()
loop state = do
    nextInput <- readInput
    case nextInput of
        Nothing -> do
            saveState state
            putSuccess "Session saved. Goodbye!"
            exitSuccess
        Just input
            | all isSpace input -> loop state
            | otherwise -> do
                nextState <- handleInput state input
                saveState nextState
                loop nextState

handleInput :: ReplState -> String -> IO ReplState
handleInput state rawInput
    | command `elem` [":quit", ":exit", "quit", "exit", "q"] = do
        saveState updatedState
        putSuccess "Session saved. Goodbye!"
        exitSuccess
    | command == ":help" = do
        printHelp
        pure updatedState
    | command == ":history" = do
        printHistory updatedState
        pure updatedState
    | command == ":env" = do
        printBindings updatedState
        pure updatedState
    | command == ":reset" = do
        let clearedState = ReplState [] []
        putSuccess "Cleared saved variables and history."
        pure clearedState
    | Just expr <- stripCommand ":tokens" command = do
        printTokens expr
        pure updatedState
    | Just expr <- stripCommand ":ast" command = do
        printAst expr
        pure updatedState
    | Just expr <- stripCommand ":debug" command = do
        printDebug updatedState expr
        pure updatedState
    | Just assignment <- stripCommand ":let" command = handleBinding updatedState rawInput assignment
    | otherwise = do
        processExpression updatedState rawInput command
  where
    command = trim rawInput
    updatedState = rememberInput rawInput state

processExpression :: ReplState -> String -> String -> IO ReplState
processExpression state _ input = do
    result <- runExpression (bindings state) input
    case result of
        Left message -> putError message
        Right value -> putSuccess ("Result: " ++ renderValue value)
    pure state

handleBinding :: ReplState -> String -> String -> IO ReplState
handleBinding state rawInput assignment =
    case break (== '=') assignment of
        (namePart, '=' : exprPart) -> do
            let name = trim namePart
                exprText = trim exprPart
            if not (isValidIdentifier name)
                then do
                    putError "Invalid variable name. Use letters, digits, or apostrophes, starting with a letter."
                    pure state
                else if null exprText
                    then do
                        putError "Missing expression after '=' in :let command."
                        pure state
                    else do
                        result <- runExpression (bindings state) exprText
                        case result of
                            Left message -> do
                                putError message
                                pure state
                            Right value -> do
                                let nextBindings = upsertBinding name value (bindings state)
                                    nextState = state {bindings = nextBindings}
                                putSuccess (name ++ " = " ++ renderValue value)
                                pure nextState
        _ -> do
            putError "Use :let <name> = <expression> to store a variable in the REPL session."
            pure state

runExpression :: ValEnv -> String -> IO (Either String Value)
runExpression env input = do
    let tokens = alexScanTokens input
    case invalidTokenMessage tokens of
        Just message -> pure (Left message)
        Nothing -> do
            parsedProgram <- parseProgram tokens
            case parsedProgram of
                Left message -> pure (Left message)
                Right program -> pure (evalProgram env program)

printTokens :: String -> IO ()
printTokens input = do
    let tokens = alexScanTokens input
    putInfo "Tokens:"
    putStrLn (indent (show tokens))
    maybe (pure ()) putError (invalidTokenMessage tokens)

printAst :: String -> IO ()
printAst input = do
    let tokens = alexScanTokens input
    case invalidTokenMessage tokens of
        Just message -> putError message
        Nothing -> do
            parsedProgram <- parseProgram tokens
            case parsedProgram of
                Left message -> putError message
                Right program -> do
                    putInfo "Abstract Syntax Tree:"
                    putStrLn (indent (show program))

printDebug :: ReplState -> String -> IO ()
printDebug state input = do
    let tokens = alexScanTokens input
    putInfo "Tokens:"
    putStrLn (indent (show tokens))
    case invalidTokenMessage tokens of
        Just message -> putError message
        Nothing -> do
            parsedProgram <- parseProgram tokens
            case parsedProgram of
                Left message -> putError message
                Right program -> do
                    putInfo "Abstract Syntax Tree:"
                    putStrLn (indent (show program))
                    case evalProgram (bindings state) program of
                        Left message -> putError message
                        Right value -> putSuccess ("Result: " ++ renderValue value)

parseProgram :: [Token] -> IO (Either String Program)
parseProgram tokens = do
    parsed <- try (evaluate (parse tokens)) :: IO (Either ErrorCall Program)
    pure $
        case parsed of
            Left err -> Left (displayErrorMessage (show err))
            Right program -> Right program

evalProgram :: ValEnv -> Program -> Either String Value
evalProgram env (Program expr) = evalExpr env expr

evalExpr :: ValEnv -> Expr -> Either String Value
evalExpr _ (IntLit n) = Right (VInt n)
evalExpr _ (BoolLit b) = Right (VBool b)
evalExpr env (Var name) =
    case lookup name env of
        Just value -> Right value
        Nothing -> Left ("Unknown variable: " ++ name)
evalExpr env (Let name expr body) = do
    value <- evalExpr env expr
    evalExpr ((name, value) : env) body
evalExpr env (If condExpr thenExpr elseExpr) = do
    condValue <- evalExpr env condExpr
    case condValue of
        VBool True -> evalExpr env thenExpr
        VBool False -> evalExpr env elseExpr
        _ -> Left "The condition of an if-expression must be a boolean value."
evalExpr env (Binop op leftExpr rightExpr) = do
    leftValue <- evalExpr env leftExpr
    rightValue <- evalExpr env rightExpr
    evalBinop op leftValue rightValue
evalExpr _ Invalid =
    Left "Invalid expression. Please check the input and try again."

evalBinop :: Op -> Value -> Value -> Either String Value
evalBinop Plus (VInt a) (VInt b) = Right (VInt (a + b))
evalBinop Minus (VInt a) (VInt b) = Right (VInt (a - b))
evalBinop Times (VInt a) (VInt b) = Right (VInt (a * b))
evalBinop Div (VInt _) (VInt 0) = Left "Division by zero is not allowed."
evalBinop Div (VInt a) (VInt b) = Right (VInt (a `div` b))
evalBinop Less (VInt a) (VInt b) = Right (VBool (a < b))
evalBinop LessEq (VInt a) (VInt b) = Right (VBool (a <= b))
evalBinop Greater (VInt a) (VInt b) = Right (VBool (a > b))
evalBinop GreaterEq (VInt a) (VInt b) = Right (VBool (a >= b))
evalBinop Eq (VInt a) (VInt b) = Right (VBool (a == b))
evalBinop Eq (VBool a) (VBool b) = Right (VBool (a == b))
evalBinop NotEq (VInt a) (VInt b) = Right (VBool (a /= b))
evalBinop NotEq (VBool a) (VBool b) = Right (VBool (a /= b))
evalBinop op leftValue rightValue =
    Left
        ( "Type error in operator "
            ++ show op
            ++ ": cannot apply it to "
            ++ renderValue leftValue
            ++ " and "
            ++ renderValue rightValue
        )

printBanner :: ReplState -> IO ()
printBanner state = do
    putInfo "=== Task 6 REPL ==="
    putStrLn "Type an expression to evaluate it."
    putStrLn "Commands: :help, :let, :env, :history, :tokens, :ast, :debug, :reset, :quit"
    putStrLn
        ( "Loaded "
            ++ show (length (bindings state))
            ++ " saved variable(s) and "
            ++ show (length (historyEntries state))
            ++ " history entry/entries."
        )
    putStrLn ""

printHelp :: IO ()
printHelp = do
    putInfo "Available commands:"
    mapM_
        putStrLn
        [ "  :help                 Show this help message"
        , "  :let x = expr         Store a variable across the session"
        , "  :env                  Show saved variables"
        , "  :history              Show previous commands"
        , "  :tokens expr          Show lexer output"
        , "  :ast expr             Show parser output"
        , "  :debug expr           Show tokens, AST, and final result"
        , "  :reset                Clear saved variables and history"
        , "  :quit                 Save state and exit"
        , "  Use a trailing \\      Continue onto the next line"
        ]

printHistory :: ReplState -> IO ()
printHistory state
    | null (historyEntries state) = putInfo "History is empty."
    | otherwise = do
        putInfo "History:"
        mapM_
            putStrLn
            [ "  " ++ show index ++ ". " ++ oneLine entry
            | (index, entry) <- zip [(1 :: Int) ..] (historyEntries state)
            ]

printBindings :: ReplState -> IO ()
printBindings state
    | null (bindings state) = putInfo "No saved variables."
    | otherwise = do
        putInfo "Saved variables:"
        mapM_
            putStrLn
            [ "  " ++ name ++ " = " ++ renderValue value
            | (name, value) <- bindings state
            ]

readInput :: IO (Maybe String)
readInput = do
    putStr (colorize blueCode "calc> ")
    hFlush stdout
    eof <- isEOF
    if eof
        then pure Nothing
        else do
            line <- getLine
            collectLines [removeContinuationMarker line]
                (needsContinuation line)

collectLines :: [String] -> Bool -> IO (Maybe String)
collectLines linesSoFar False = pure (Just (intercalate "\n" linesSoFar))
collectLines linesSoFar True = do
    putStr (colorize blueCode "...  ")
    hFlush stdout
    eof <- isEOF
    if eof
        then pure (Just (intercalate "\n" linesSoFar))
        else do
            line <- getLine
            collectLines
                (linesSoFar ++ [removeContinuationMarker line])
                (needsContinuation line)

needsContinuation :: String -> Bool
needsContinuation line =
    case reverse (trimRight line) of
        '\\' : _ -> True
        _ -> False

removeContinuationMarker :: String -> String
removeContinuationMarker line =
    case reverse (trimRight line) of
        '\\' : rest -> reverse rest
        _ -> line

rememberInput :: String -> ReplState -> ReplState
rememberInput rawInput state =
    state {historyEntries = historyEntries state ++ [rawInput]}

upsertBinding :: String -> Value -> ValEnv -> ValEnv
upsertBinding name value env =
    (name, value) : filter ((/= name) . fst) env

loadState :: IO ReplState
loadState = do
    envPath <- stateFilePath envFileName
    historyPath <- stateFilePath historyFileName
    savedBindings <- loadSerialized envPath []
    savedHistory <- loadSerialized historyPath []
    pure (ReplState savedBindings savedHistory)

saveState :: ReplState -> IO ()
saveState state = do
    envPath <- stateFilePath envFileName
    historyPath <- stateFilePath historyFileName
    writeFile envPath (show (bindings state))
    writeFile historyPath (show (historyEntries state))

stateFilePath :: FilePath -> IO FilePath
stateFilePath fileName = do
    executablePath <- getExecutablePath
    pure (takeDirectory executablePath </> fileName)

loadSerialized :: Read a => FilePath -> a -> IO a
loadSerialized path fallback = do
    exists <- doesFileExist path
    if not exists
        then pure fallback
        else do
            content <- readFile path
            pure (maybe fallback id (readMaybe content))

invalidTokenMessage :: [Token] -> Maybe String
invalidTokenMessage [] = Nothing
invalidTokenMessage (TokenInvalid bad : _) =
    Just ("Invalid token: " ++ show bad)
invalidTokenMessage (_ : rest) = invalidTokenMessage rest

displayErrorMessage :: String -> String
displayErrorMessage message =
    case takeWhile (/= '\n') (trim message) of
        [] -> "Unknown error."
        trimmedMessage -> trimmedMessage

renderValue :: Value -> String
renderValue (VInt n) = show n
renderValue (VBool True) = "true"
renderValue (VBool False) = "false"
renderValue (VError message) = "error: " ++ message

isValidIdentifier :: String -> Bool
isValidIdentifier [] = False
isValidIdentifier (firstChar : restChars) =
    isAlpha firstChar && all isIdentifierChar restChars

isIdentifierChar :: Char -> Bool
isIdentifierChar c = isAlphaNum c || c == '\''

stripCommand :: String -> String -> Maybe String
stripCommand name input
    | input == name = Just ""
    | (name ++ " ") `isPrefixOf` input = Just (trim (drop (length name) input))
    | otherwise = Nothing

trim :: String -> String
trim = trimRight . dropWhile isSpace

trimRight :: String -> String
trimRight = reverse . dropWhile isSpace . reverse

indent :: String -> String
indent text = unlines ["  " ++ line | line <- lines text]

oneLine :: String -> String
oneLine = unwords . words

putInfo :: String -> IO ()
putInfo = putStrLn . colorize cyanCode

putSuccess :: String -> IO ()
putSuccess = putStrLn . colorize greenCode

putError :: String -> IO ()
putError = putStrLn . colorize redCode . ("Error: " ++)

colorize :: String -> String -> String
colorize code text = "\ESC[" ++ code ++ "m" ++ text ++ "\ESC[0m"

redCode, greenCode, blueCode, cyanCode :: String
redCode = "31;1"
greenCode = "32;1"
blueCode = "34;1"
cyanCode = "36;1"
