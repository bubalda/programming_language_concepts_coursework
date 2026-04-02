module Lang.Repl.Env
  ( ReplEnv (..),
    ReplFlags (..),
    debugFlags,
    releaseFlags,
    loadReplEnv,
    saveReplState,
    rememberHistory,
    resetReplState,
  )
where

import qualified Data.Map as Map
import Lang.Eval.Types (ProgramEnv)
import System.IO.Error (catchIOError)
import Text.Read (readMaybe)

-- Used by REPL.
data ReplEnv = ReplEnv
  { programEnv :: ProgramEnv,
    replFlags :: ReplFlags,
    replHistory :: [String]
  }

data ReplFlags = ReplFlags
  { debugMode :: Bool,
    showTokens :: Bool,
    showAST :: Bool,
    prettyEval :: Bool
  }

-- Settings used for debugging.
debugFlags :: ReplFlags
debugFlags =
  ReplFlags
    { debugMode = True,
      showTokens = True,
      showAST = True,
      prettyEval = True
    }

-- Settings used for release.
releaseFlags :: ReplFlags
releaseFlags =
  ReplFlags
    { debugMode = False,
      showTokens = False,
      showAST = False,
      prettyEval = False
    }

envFileName :: FilePath
envFileName = ".c2repl-env"

loadReplEnv :: ReplFlags -> IO ReplEnv
loadReplEnv flag = do
  savedEnv <- loadSerialized envFileName Map.empty
  pure
    ReplEnv
      { programEnv = savedEnv,
        replFlags = flag,
        replHistory = []
      }

saveReplState :: ReplEnv -> IO ()
saveReplState rEnv = writeFile envFileName (show (programEnv rEnv))

rememberHistory :: String -> ReplEnv -> ReplEnv
rememberHistory line rEnv =
  rEnv {replHistory = replHistory rEnv ++ [line]}

resetReplState :: ReplEnv -> ReplEnv
resetReplState rEnv =
  rEnv
    { programEnv = Map.empty,
      replHistory = []
    }

loadSerialized :: Read a => FilePath -> a -> IO a
loadSerialized path fallback =
  catchIOError
    (do
      content <- readFile path
      length content `seq` pure ()
      pure (maybe fallback id (readMaybe content)))
    (\_ -> pure fallback)
