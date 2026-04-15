# Coursework

1. Install external deps 
```sh
stack install alex happy
```

2. Run program
```sh
stack run -- --debug
```

3. If VSCode intellisense won't work that means your hls / ghc version doesn't match. Try
- HLS 2.13.0.0
- GHC 9.10.3.0
- And restart intellisense

4. Check Lang.Lexer.Keywords for available functions

5. SemiColons are line delimiters (;) to support one-liners

# Check for
- Comments: "String x = "Hello World"; // int x = 0; // x = 0 does not run"
- Nested comments: x  /* /* */ */ = 13; // Should work, /* /* */ does not work
- Space and newlines are skipped
- Prints simple values: 1, 1.23, "hello", 'h', True
- Arithmetic / Bitwise operators: +, -, *, /, %, &, |, ^, <<, >>
- Arithmetic Assignment operators: +=, -=, *=, /=, %=, &=, |=, ^=, <<=, >>=, =
- Lists: [1, 2, 3]
- List slices (Like Python): [1, 2, 3][0], [1, 2, 3][1:], [1, 2, 3][start:stop:step]
- Texas ranges (sequences): [1..100]
- Boolean operators: !, &&, ||
- Comparison operators: ==, !=, <=, <, >=, >
- Bracket precedence: (1 + 2) ^ 3
- if-then-else condition (May remove the keyword then): if (1 == 1) then {r = 2; r *= 5;} else {r = 3;}, if (1 == 0) then {r = 2; r *= 5;}
- Static type declaration: double x = 10, CHECK ALL TYPES (double, char, String, float, int, bool)
- Scope: let x = 10 in x * 2
- Constants: pi, null, True, False
- Variable name format: Starts with _ or alphabet, ends with _, alphabet or digits
- Function call: function_name(params);
    @jinyi 可以填一下他们是怎么实现的吗？意思到位其他人可以理解就行 (像 std 是 standard deviation 一样)
    - length // length(array); 
    - sinh // sinh(x);
    - cosh
    - tanh
    - csch
    - sech
    - coth
    - asinh
    - acosh
    - mean
    - median
    - mode
    - sum
    - product
    - min
    - max
    - stddev // standard deviation
    - sqrt
    - cbrt
    - pow
    - exp
    - square
    - cube
    - exp10
    - sin
    - cos
    - tan
    - asin
    - acos
    - atan
    - atan2
    - sec
    - csc
    - cot
    - versin
    - exsec
    - ln
    - log10
    - log2
    - log
    - log1p
    - fact
    - fact2
    - comb
    - perm
    - gcd
    - lcm
    - fib
    - gamma
- REPL
    - Repl Commands (Check :? / displayHelp function and remember to test the commands)
        - :?"
        - :help
        - :q
        - :quit
        - :debug
        - :tokens
        - :ast
        - :evalPretty
        - :env
        - :history
        - :reset
    - UP and DOWN arrows for history
    - repl-env @jinyi 这个其实是啥情况啊我不大理解
    - \\ backslash for new line in repl
        - c2> x = 8;\
        - ... x * 2;