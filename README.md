# Programming Language Concepts Coursework

## Overview

This project implements a small custom programming language in Haskell. The goal was to build a complete pipeline starting from raw input all the way to execution, including lexical analysis, parsing, type checking, and evaluation.

The system is structured similarly to a simple compiler/interpreter. It takes user input, converts it into tokens using a lexer, builds an abstract syntax tree (AST) using a parser, checks types, and then evaluates the program.

The project also includes a REPL for interactive execution and a test suite to verify correctness.

## Features

* Variable declarations using `let`
* Arithmetic expressions and operators
* Type checking before evaluation
* Expression evaluation
* Interactive REPL
* Error handling for invalid input


## Architecture

The program follows a clear pipeline:

```
Input → Lexer → Tokens → Parser → AST → Type Checker → Evaluator → Output
```

### Components

* **Lexer** (`Lexer.x`, `Tokens.hs`, `Keywords.hs`)
  Converts raw input into tokens using Alex.

* **Parser** (`Parser.y`, `Expr.hs`)
  Builds the AST using Happy based on grammar rules.

* **Syntax / AST** (`Syntax.hs`)
  Defines the structure of expressions and statements.

* **Type Checker** (`TypeChecker.hs`, `Types.hs`)
  Ensures expressions are type-correct before execution.

* **Evaluator** (`Eval.hs`, `Op.hs`)
  Executes the program and produces results.

* **REPL** (`Repl.hs`, `Runner.hs`, `Commands.hs`)
  Allows interactive input and execution.

* **CLI** (`Main.hs`, `CLI/Args.hs`)
  Handles program entry and arguments.

## Installation and Running

Make sure you have Stack installed.

### Build the project

```
stack build
```

### Run the program

```
stack run
```

### Run tests

```
stack test
```

## Usage

When running the program, you can enter expressions or statements directly through the REPL.

Example:

```
let x = 5;
```

The program will:

1. Tokenise the input
2. Parse it into an AST
3. Type check it
4. Evaluate the result

---

## Example

Input:

```
let x = 5 + 3;
```

Process:

* Tokens: `let`, `x`, `=`, `5`, `+`, `3`
* AST: assignment with a binary operation
* Type checking: valid (integer expression)
* Evaluation: computes result

Output:

```
x = 8
```

## Testing

The project includes a test suite located in `test/Spec.hs`.

Tests cover:

* Parsing correctness
* Expression evaluation
* Type checking

All tests pass successfully with no failures.

## Technologies Used

* Haskell
* Alex (lexer generator)
* Happy (parser generator)

## Notes

The project is designed with a modular structure, separating each stage of the pipeline. This makes it easier to debug, extend, and test individual components.

Type checking is performed before evaluation to prevent invalid programs from executing.

