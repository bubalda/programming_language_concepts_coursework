# Error Handling

## Overview

This document describes how errors are handled throughout the system. Errors can occur at different stages of the language pipeline, including lexical analysis, parsing, type checking, and evaluation.

The project is designed to detect errors as early as possible, preventing invalid programs from being executed.

---

## Error Reporting

When an error occurs, the system reports:

- The **type** of error (lexical, syntax, type, or runtime)
- The **line and column number** where the error was detected
- A **visual position indicator** that highlights exactly where in the source code the error occurred

### Error Output Format

```
Line 3, Column 7:
    sin(;
          ^
Error: Unexpected token ';'
```

The `^` character points directly to the position of the fault in the source line. For longer invalid spans, multiple carets are used:

```
Line 1, Column 9:
    let x = @value;
            ^^^^^^
Error: Unexpected character '@'.
```

This format makes it immediately clear where the error occurred, significantly reducing debugging time.

---

## Types of Errors

### 1. Lexical Errors

**Where:** Lexer (`Lexer.x`)

These occur when the input contains characters or patterns that cannot be recognised as valid tokens.

#### Causes

- Unrecognised characters (e.g. `@`, `#`)
- Non-ASCII characters in source input
- Unclosed block comments (e.g. `/* opened but never closed`)

#### Example

```
Input:  let x = 5 @;
```

#### Output

```
Line 1, Column 11:
    let x = 5 @;
              ^
Error: Unexpected character '@'.
```

#### Behaviour

- The lexer fails to match the invalid token
- An error is reported with line, column, and a visual pointer
- Non-ASCII characters are detected and reported with a specific message


### 2. Syntax Errors

**Where:** Parser (`Parser.y`)

These occur when the sequence of tokens does not follow the grammar rules of the language.

#### Causes

- Missing operands (e.g. `let x = ;`)
- Unclosed brackets or braces
- Unexpected end of input

#### Example

```
Input:  Print(;
```

#### Output

```
Line 1, Column 7:
    Print(;
          ^
Error: Unexpected token ';'
```

#### Behaviour

- The parser encounters an unexpected token or end of input
- Parsing stops and an error is reported with line, column, and visual indicator


### 3. Type Errors

**Where:** Type Checker (`TypeChecker.hs`)

These occur when expressions violate the type rules of the language.

#### Causes

- Type mismatch in a declaration (e.g. assigning a `String` to an `int` variable)
- Using an undefined variable
- Applying an operator to incompatible types

#### Example

```
Input:  int x = "hello";
```

#### Output

```
Error: Type error: Expected type Int, but got "hello" :: String
```

#### Behaviour

- The type checker detects the mismatch
- An error message is returned with details of the expected and actual types
- Evaluation is not performed


### 4. Runtime Errors

**Where:** Evaluator (`Eval.hs`, `Eval/Errors.hs`)

These occur during execution when an operation cannot be completed even though it passed type checking.

#### Causes

- Division by zero
- Modulo by zero
- Accessing an undefined variable at runtime
- List index out of bounds
- Invalid function arguments (e.g. `sqrt(-1)`)

#### Example

```
Input:  10 / 0;
```

#### Output

```
Error: Division by zero
```

#### Behaviour

- Evaluation halts immediately
- An error is returned describing the cause
- No partial result is produced

---

## Error Handling Strategy

The project follows a staged approach, catching errors as early as possible:

1. **Lexer** — rejects invalid or unrecognised characters immediately, before any parsing begins
2. **Parser** — ensures the token sequence conforms to the language grammar
3. **Type Checker** — validates type correctness of all expressions and declarations
4. **Evaluator** — executes only programs that have passed all prior stages

This pipeline ensures:

- Errors are caught at the earliest possible stage
- Invalid programs do not proceed to later stages
- The programmer receives precise, actionable feedback including the exact source location

---

## Full Examples

### Example 1: Lexical Error (unrecognised character)

```
Input:  let x = 5 @;
```

Output:

```
Line 1, Column 11:
    let x = 5 @;
              ^
Error: Unexpected character '@'.
```

### Example 2: Lexical Error (non-ASCII character)

```
Input:  let x = 5£;
```

Output:

```
Line 1, Column 11:
    let x = 5£;
              ^
Error: Unexpected non-ASCII character "£". Please use supported ASCII syntax only.
```

### Example 3: Lexical Error (unclosed block comment)

```
Input:  /* this comment was never closed
```

Output:

```
Error: Unclosed block comments detected, did you close it using '*/'?
```

### Example 4: Syntax Error

```
Input:  Print(;
```

Output:

```
Line 1, Column 7:
    Print(;
          ^
Error: Unexpected token ';'
```

### Example 5: Type Error

```
Input:  int x = "hello";
```

Output:

```
Error: Type error: Expected type Int, but got "hello" :: String
```

### Example 6: Runtime Error (division by zero)

```
Input:  10 / 0;
```

Output:

```
Error: Division by zero
```

### Example 7: Runtime Error (undefined variable)

```
Input:  x + 5;
```

Output:

```
Error: Undefined identifier: x
```

### Example 8: Runtime Error (list index out of bounds)

```
Input:  [1, 2, 3][10];
```

Output:

```
Error: List index out of bounds
```

---

## Summary

Error handling is implemented across all four stages of the pipeline. Every error includes a clear message describing the cause. Lexical and syntax errors additionally include the line and column number of the fault along with a visual source pointer (`^`) that highlights exactly where in the code the issue occurred. This staged approach ensures robustness, clarity of feedback, and prevents invalid programs from executing.
