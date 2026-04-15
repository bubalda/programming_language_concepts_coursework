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
- Texas ranges: [1..100]
- Boolean operators: !, &&, ||
- Comparison operators: ==, !=, <=, <, >=, >
- Bracket precedence: (1 + 2) ^ 3
- if-else condition : if (1 == 1) {r = 2; r *= 5;} else {r = 3;}, if (1 == 0) {r = 2; r *= 5;}
- Dynamic type declaration: x = 10 // Gets 10 :: int
- Static type declaration: double x = 10, CHECK ALL TYPES (double, char, String, float, int, bool)
- Scope: let x = 10 in x * 2
- Constants: pi, null, True, False
- Variable name format: Starts with _ or alphabet, ends with _, alphabet or digits
- Function call: function_name(params);
    - length // length(array); 
    - sinh // sinh(x);
    - cosh // hyperbolic cosine
    - tanh // hyperbolic tangent
    - csch // hyperbolic cosecant = 1 / sinh(x)
    - sech // hyperbolic secant = 1 / cosh(x)
    - coth // hyperbolic cotangent = 1 / tanh(x)
    - asinh // inverse hyperbolic sine
    - acosh // inverse hyperbolic cosine, domain: x >= 1
    - mean // arithmetic average
    - median // middle value after sorting
    - mode // most frequent value
    - sum // total of all values
    - product // multiplication of all values
    - min // smallest value
    - max // largest value
    - stddev // standard deviation
    - sqrt // square root, domain: x >= 0
    - cbrt // cube root
    - pow // power: pow(base, exponent)
    - exp // e^x
    - square // x^2
    - cube // x^3
    - exp10 // 10^x, e.g. exp10(3) = 1000
    - sin // sine
    - cos // cosine
    - tan // tangent
    - asin // inverse sine, domain: x in [-1, 1]
    - acos // inverse cosine, domain: x in [-1, 1]
    - atan // inverse tangent
    - atan2 // atan2(y, x), returns the angle with correct quadrant info
    - sec // secant = 1 / cos(x)
    - csc // cosecant = 1 / sin(x)
    - cot // cotangent = 1 / tan(x)
    - versin // versed sine = 1 - cos(x)
    - exsec // exsecant = sec(x) - 1
    - ln // natural logarithm, domain: x > 0
    - log10 // base-10 logarithm, domain: x > 0
    - log2 // base-2 logarithm, domain: x > 0
    - log // log10(x) or log(base, x)
    - log1p // ln(1 + x), domain: x > -1
    - fact // factorial: n! for non-negative integers
    - fact2 // double factorial: n * (n-2) * (n-4) * ... (not (n!)^2)
    - comb // combinations C(n, r)
    - perm // permutations P(n, r)
    - gcd // greatest common divisor
    - lcm // least common multiple
    - fib // nth Fibonacci number
    - gamma // gamma function, extends factorial to non-integers
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
    - .c2repl-env can be written manually to provide preset variables on the start of the program
    - \\ backslash for new line in repl
        - c2> x = 8;\
        - ... x * 2;
