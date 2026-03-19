module Lang.Lexer.Keywords (functions) where

-- Check readme on references of function arguments
-- Should be resolved in eval stage on object `Call FunctionName [ArgsExpr]`
functions :: [String]
functions =
  [ -- List length
    "len",
    
    -- Hyperbolic Functions
    "sinh",
    "cosh",
    "tanh",
    "csch",
    "sech",
    "coth",
    "asinh",
    "acosh",
    -- Statistical Functions
    "mean",
    "median",
    "mode",
    "sum",
    "product",
    "min",
    "max",
    "stddev",
    -- Power and Root Functions
    "sqrt",
    "cbrt",
    "pow",
    "exp",
    "square",
    "cube",
    "exp10",
    -- Trigonometric Functions
    "sin",
    "cos",
    "tan",
    "asin",
    "acos",
    "atan",
    "atan2",
    "sec",
    "csc",
    "cot",
    "versin",
    "exsec",
    -- Logarithmic Functions
    "ln",
    "log10",
    "log2",
    "log",
    "log1p",
    -- Combinatorial Functions
    "fact",
    "fact2",
    "comb",
    "perm",
    "gcd",
    "lcm",
    "fib",
    "gamma"
  ]