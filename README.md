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