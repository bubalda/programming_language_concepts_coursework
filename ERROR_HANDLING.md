# Error Handling

## Overview

This document describes how errors are handled throughout the system. Errors can occur at different stages of the language pipeline, including lexical analysis, parsing, type checking, and evaluation.

The project is designed to detect errors as early as possible, preventing invalid programs from being executed.


## Types of Errors

### 1. Lexical Errors

**Where:** Lexer (`Lexer.x`)

These occur when the input contains characters or patterns that cannot be recognised as valid tokens.

#### Examples

```
Input:  let x = 5 @
```

#### Behaviour

* The lexer fails to match the invalid character
* An error is reported indicating an unknown token


### 2. Syntax Errors

**Where:** Parser (`Parser.y`)

These occur when the sequence of tokens does not follow the grammar rules of the language.

#### Examples

```
Input:  let x = ;
```

#### Behaviour

* The parser encounters an unexpected token
* Parsing stops and an error is reported


### 3. Type Errors

**Where:** Type Checker (`TypeChecker.hs`)

These occur when expressions violate type rules.

#### Examples

```
Input:  x + 5
```

(where `x` is undefined or not an integer)

#### Behaviour

* The type checker detects the mismatch
* Evaluation is not performed
* An error message is returned


### 4. Runtime Errors

**Where:** Evaluator (`Eval.hs`, `Eval/Errors.hs`)

These occur during execution when an operation cannot be completed.

#### Examples

```
Input:  10 / 0
```

#### Behaviour

* Evaluation fails
* An error is returned instead of a result

## Error Reporting

Errors are reported with clear messages indicating the issue and where it occurred.

Typical format:

```
Error: Unexpected token '='
Error: Type mismatch in expression
Error: Division by zero
```

The system stops further processing once an error is detected to avoid cascading failures.

## Error Handling Strategy

The project follows a staged approach:

1. **Lexer** – rejects invalid characters early
2. **Parser** – ensures structure is correct
3. **Type Checker** – validates correctness of expressions
4. **Evaluator** – executes only valid programs

This ensures:

* Errors are caught as early as possible
* Invalid programs do not proceed to later stages
* The system remains stable and predictable

## Examples

### Example 1: Syntax Error

```
Input:  let x =
```

Output:

```
Error: Unexpected end of input
```

### Example 2: Type Error

```
Input:  x + 5
```

Output:

```
Error: Undefined variable or type mismatch
```

### Example 3: Runtime Error

```
Input:  10 / 0
```

Output:

```
Error: Division by zero
```

## Summary

Error handling is implemented across all stages of the pipeline to ensure robustness. By separating error detection into lexical, syntactic, semantic, and runtime stages, the system provides clear feedback and prevents invalid programs from executing.
