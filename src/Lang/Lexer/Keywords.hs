module Lang.Lexer.Keywords (functions) where

-- Check readme on references of function arguments
-- Should be resolved in eval stage on object `Call FunctionName [ArgsExpr]`
functions :: [String]
functions =
  [ -- List / String length
    "length",

    -- Hyperbolic Functions
    "sinh", -- sinh x
    "cosh", -- cosh x
    "tanh", -- tanh x
    "csch", -- csch x
    "sech", -- sech x
    "coth", -- coth x
    "asinh", -- asinh x
    "acosh", -- acosh x

    -- Statistical Functions
    "mean", -- average [x]
    "median", -- median [x]
    "mode", -- mode [x]
    "sum", -- foldr (+) [x'
    "product", -- foldr (*) [x]
    "min", -- min [x]
    "max", -- max [x]
    "stddev", -- std [x]

    -- Power and Root Functions
    "sqrt", -- x^(1/2)
    "cbrt", -- x^(1/3)
    "pow", -- x^n
    "exp", -- e^x
    "square", -- x^2
    "cube", -- x^3
    "exp10", -- 10^x

    -- Trigonometric Functions
    "sin", -- sin x
    "cos", -- cos x
    "tan", -- tan x
    "asin", -- asin x
    "acos", -- acos x
    "atan", -- atan x
    "atan2", -- atan2 x y
    "sec", -- sec x
    "csc", -- csc x
    "cot", -- cot x
    "versin", -- versin x
    "exsec", -- exsec x

    -- Logarithmic Functions
    "ln", -- ln x
    "log10", -- log10 x
    "log2", -- log2 x
    "log", -- log x
    "log1p", -- log1p x

    -- Combinatorial Functions
    "fact", -- fact x
    "fact2", -- factSquared x
    "comb", -- combination r n
    "perm", -- permutation r n
    "gcd", -- gcd [x]
    "lcm", -- lcm [x]
    "fib", -- fib x
    "gamma" -- gamma x
  ]