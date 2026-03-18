# c2-compiler

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

4. About how functions should be
- sinh x
- cosh x
- tanh x
- csch x
- sech x
- coth x
- asinh x
- acosh x 
- average [x]
- median [x]
- mode [x]
- foldr (+) [x]
- foldr (*) [x]
- min [x]
- max [x]
- std [x]
- x^(1/2)
- x^(1/3)
- x^n
- e^x
- x^2
- x^3
- 10^x
- sin x 
- cos x
- tan x
- asin x
- acos x
- atan x
- atan2 x y
- sec x 
- csc x 
- cot x 
- versin x 
- exsec x 
- ln x
- log10 x
- log2 x
- log x
- log1p x
- fact x
- factSquared x
- comb r n 
- perm r n 
- gcd [x] 
- lcm [x]
- fib x
- gamma x