module Lang.Repl.Env (ReplEnv (..), ReplFlags (..), debugFlags, releaseFlags) where

import Lang.Eval.Types (ProgramEnv)

-- Used by REPL
data ReplEnv = ReplEnv
  { programEnv :: ProgramEnv,
    replFlags :: ReplFlags
  }

data ReplFlags = ReplFlags
  { debugMode :: Bool,
    showTokens :: Bool,
    showAST :: Bool,
    prettyEval :: Bool
  }

-- Settings used for debugging
debugFlags :: ReplFlags
debugFlags =
  ReplFlags
    { debugMode = True,
      showTokens = False, -- TODO switch this to true on submit
      showAST = True,
      prettyEval = True
    }

-- Settings used for release
releaseFlags :: ReplFlags
releaseFlags =
  ReplFlags
    { debugMode = False,
      showTokens = False,
      showAST = False,
      prettyEval = False
    }
