module Lang.Repl.Env (ReplEnv (..), ReplFlags (..), debugRepl, releaseRepl) where

import qualified Data.Map as Map
import Lang.Parser.Eval (ProgramEnv)

-- Used by REPL
data ReplEnv = ReplEnv
  { programEnv :: ProgramEnv,
    flags :: ReplFlags
  }

data ReplFlags = ReplFlags
  { showTokens :: Bool,
    showAST :: Bool
  }

-- Settings used for debugging
debugRepl :: ReplEnv
debugRepl =
  ReplEnv
    { programEnv = Map.empty,
      flags =
        ReplFlags
          { showTokens = True,
            showAST = True
          }
    }

-- Settings used for release
releaseRepl :: ReplEnv
releaseRepl =
  ReplEnv
    { programEnv = Map.empty,
      flags =
        ReplFlags
          { showTokens = False,
            showAST = False
          }
    }
