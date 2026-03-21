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
    prettyEval :: Bool,
    semicolonEnd :: Bool
  }

-- Settings used for debugging
debugFlags :: ReplFlags
debugFlags =
  ReplFlags
    { debugMode = True,
      showTokens = True, -- TODO switch this off using `:tokens off`
      showAST = True,
      prettyEval = True,
      semicolonEnd = True
    }

-- Settings used for release
releaseFlags :: ReplFlags
releaseFlags =
  ReplFlags
    { debugMode = False,
      showTokens = False,
      showAST = False,
      prettyEval = False,
      semicolonEnd = True
    }
