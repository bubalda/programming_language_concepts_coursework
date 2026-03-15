module Lang.Repl.Env (ReplEnv (..), ReplFlags (..), debugFlags, releaseFlags) where

import Lang.Parser.Eval (ProgramEnv)

-- Used by REPL
data ReplEnv = ReplEnv
  { programEnv :: ProgramEnv,
    replFlags :: ReplFlags
  }

data ReplFlags = ReplFlags
  { debug :: Bool,
    showTokens :: Bool,
    showAST :: Bool,
    prettyEval :: Bool
  }

-- Settings used for debugging
debugFlags :: ReplFlags
debugFlags =
  ReplFlags
    { debug = True,
      showTokens = True,
      showAST = True,
      prettyEval = True
    }

-- Settings used for release
releaseFlags :: ReplFlags
releaseFlags =
  ReplFlags
    { debug = False,
      showTokens = False,
      showAST = False,
      prettyEval = False
    }
