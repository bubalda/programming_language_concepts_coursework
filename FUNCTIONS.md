# Language Design

## Overview

This document describes the design of the custom programming language implemented in this project. The language is designed to be simple but structured, focusing on expressions, variable declarations, and basic evaluation.

The goal was to keep the syntax minimal while still demonstrating key concepts such as lexical analysis, parsing, type checking, and evaluation.


## Tokens

The language is first broken down into tokens during lexical analysis. These are defined using Alex.

### Token Types

* **Keywords**

  * `let`

* **Identifiers**

  * Variable names (e.g. `x`, `value`)

* **Literals**

  * Integer values (e.g. `5`, `10`)

* **Operators**

  * `+`, `-`, `*`, `/`
  * `=`

* **Symbols**

  * `;`


## Grammar

The syntax of the language is defined using Happy. The grammar describes how tokens are combined to form valid programs.

### Example Grammar Rules

```id="z6jv7v"
Stmt → let IDENTIFIER = Expr ;
Expr → Expr + Term
     | Expr - Term
     | Term
Term → Term * Factor
     | Term / Factor
     | Factor
Factor → NUMBER
       | IDENTIFIER
```

Operator precedence is handled so that:

* `*` and `/` bind tighter than `+` and `-`
* Expressions are evaluated left to right where applicable


## Abstract Syntax Tree (AST)

After parsing, the program is represented as an AST defined in `Syntax.hs`.

### Main Node Types

* **Variable**
* **Literal**
* **Binary Operation**
* **Assignment**

Each node represents a structured part of the program, making it easier to process during type checking and evaluation.

## Semantics

The semantics define how the program behaves during execution.

### Variable Declaration

* Variables are introduced using `let`
* Values are stored in an environment

Example:

```id="sp1p4z"
let x = 5;
```


### Expressions

* Arithmetic expressions are evaluated using standard rules
* Operators behave as expected:

  * `+` addition
  * `-` subtraction
  * `*` multiplication
  * `/` division

Example:

```id="9rl3h1"
5 + 3 * 2
```

This evaluates to:

```id="hsm7x9"
11
```


## Type System

The language includes a basic type system implemented in `TypeChecker.hs`.

### Supported Types

* Integer

### Type Rules

* Arithmetic operations require integer operands
* Assignments must match the expected type
* Invalid operations are rejected before evaluation

Type checking is performed before execution to ensure safety.


## Examples

### Valid Program

```id="y4x9wr"
let x = 10;
let y = x + 5;
```


### Invalid Program

```id="5huxrs"
let x = 10 + ;
```

This results in a syntax error during parsing.


## Design Decisions

* The language is intentionally simple to focus on core concepts
* A clear separation between lexer, parser, and evaluator was maintained
* Type checking is done before evaluation to prevent runtime errors
* Operator precedence is handled in the parser rather than during evaluation


## Summary

The language demonstrates the full pipeline of a programming language, from syntax definition to execution. While minimal, it captures the essential components needed to understand how languages are implemented.
