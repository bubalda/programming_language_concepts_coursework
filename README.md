# Programming Language Concepts Coursework

## Overview

This project implements a small custom programming language in Haskell, with a syntax **based on C**. The goal was to build a complete pipeline starting from raw input all the way to execution, including lexical analysis, parsing, type checking, and evaluation.

The system is structured similarly to a simple compiler/interpreter. It takes user input, converts it into tokens using a lexer, builds an abstract syntax tree (AST) using a parser, checks types, and then evaluates the program.

The project also includes a REPL for interactive execution and a test suite to verify correctness.

---

## Features

- Static type declarations (`int`, `double`, `float`, `bool`, `char`, `String`)
- Dynamic type inference — variables assigned without a type keyword are inferred automatically:
  - `x = 10.0` → inferred as `double`
  - `y = "hello"` → inferred as `String`
- Arithmetic, logical, comparison, and bitwise operators
- Compound assignment operators (`+=`, `-=`, `*=`, `/=`, `%=`, `&=`, `|=`, `^=`, `<<=`, `>>=`)
- `if-then-else` conditional expressions
- `let ... in` expression-scoped bindings
- List literals, ranges, indexing, and slicing
- Built-in mathematical and statistical functions (`sin`, `cos`, `sqrt`, `mean`, `fact`, etc.)
- Type checking before evaluation
- Interactive REPL with command history and session-local environment state
- Debug mode for inspecting the internal pipeline
- Error reporting with line/column numbers and visual source pointers

---

## Architecture

The program follows a clear pipeline:

```
Input → Lexer → Tokens → Parser → AST → Type Checker → Evaluator → Output
```

### Components

- **Lexer** (`Lexer.x`, `Tokens.hs`, `Keywords.hs`)
  Converts raw input into tokens using Alex.

- **Parser** (`Parser.y`, `Expr.hs`)
  Builds the AST using Happy based on grammar rules.

- **Syntax / AST** (`Syntax.hs`)
  Defines the structure of expressions and statements.

- **Type Checker** (`TypeChecker.hs`, `Types.hs`)
  Ensures expressions are type-correct before execution.

- **Evaluator** (`Eval.hs`, `Op.hs`)
  Executes the program and produces results.

- **REPL** (`Repl.hs`, `Runner.hs`, `Commands.hs`)
  Allows interactive input and execution with session-local state.

- **CLI** (`Main.hs`, `CLI/Args.hs`)
  Handles program entry and command-line arguments.

---

## Installation

### 1. Install GHCup

GHCup is the recommended way to install Haskell tooling including GHC and Stack. Visit [https://www.haskell.org/ghcup/](https://www.haskell.org/ghcup/) and follow the instructions for your operating system, or run:

```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

Make sure to install **Stack** when prompted during GHCup setup.

### 2. Verify Stack is installed

```bash
stack --version
```

### 3. Navigate to the project directory

```bash
cd path/to/project
```

---

## Building and Running

### Build the project

```bash
stack build
```

### Run the REPL

```bash
stack run
```

### Run in debug mode

Debug mode shows the token output, AST, and evaluation details at each step:

```bash
stack run -- --debug
```

The short form `-d` also works:

```bash
stack run -- -d
```

### Run tests

```bash
stack test
```

### Clean build artifacts

```bash
stack clean
```

---

## REPL Commands

Once inside the REPL, type `:?` or `:help` to display all available commands.

| Command | Description |
|---------|-------------|
| `:?` / `:help` | Show the help page |
| `:q` / `:quit` | Exit the current REPL session |
| `:debug [ON\|OFF]` | Toggle debug mode (shows tokens, AST, and evaluation) |
| `:tokens [ON\|OFF]` | Show lexer token output |
| `:ast [ON\|OFF]` | Show parser AST output |
| `:evalPretty [ON\|OFF]` | Pretty-print evaluation results |
| `:env` | Display all currently saved variables |
| `:history` | Show command history |
| `:reset` | Clear all saved variables and recorded history |

Use a trailing `\` to continue a statement onto the next line. Each statement must end with `;`

The REPL keeps history only for the current session. It reads the baseline environment from `.c2repl-env` at startup, writes the current session state to `.c2repl-env-temp` while running, and deletes the temp file when the session exits. Both files store the REPL state as `(programEnv, typeEnv)`, for example `(fromList [("a", VInt 1)], fromList [("a", TInt)])`.

When running the program, enter expressions or statements directly through the REPL.

Example:

```
int x = 5;
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
int x = 5 + 3;
```

Process:

- Tokens: `int`, `x`, `=`, `5`, `+`, `3`, `;`
- AST: declaration with a binary addition expression
- Type checking: valid (`int` expression)
- Evaluation: computes result

Output:

```
x = 8
```

---

## Testing

The project includes a test suite located in `test/Spec.hs`.

Tests cover:

- Parsing correctness
- Expression evaluation
- Type checking

All tests pass successfully with no failures.

---

## Technologies Used

- Haskell
- Alex (lexer generator)
- Happy (parser generator)
- Stack (build tool)
- GHCup (Haskell toolchain installer)

---

## Notes

- The language syntax is based on C, making it familiar and consistent with standard conventions
- The project is designed with a modular structure, separating each stage of the pipeline
- Type checking is performed before evaluation to prevent invalid programs from executing
- Debug mode (`--debug` or `-d`) is useful for inspecting the internal pipeline at each stage
- The REPL keeps state during the current session only. Use `:env` to view saved variables and `:reset` to clear them
