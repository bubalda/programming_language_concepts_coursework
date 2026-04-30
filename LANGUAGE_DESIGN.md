# Language Design

## Overview

This document describes the design of the custom programming language implemented in this project. The language is designed to be simple but structured, with a syntax **inspired by C**, focusing on expressions, variable declarations, control flow, and basic evaluation.

The goal was to keep the syntax minimal while still demonstrating key concepts such as lexical analysis, parsing, type checking, and evaluation.

---

## Data Types

The language supports the following static data types, which are declared explicitly:

| Type | Keyword | Description | Example |
|------|---------|-------------|---------|
| Integer | `int` | Whole numbers | `int x = 5;` |
| Double-precision float | `double` | Double-precision decimal | `double y = 3.14;` |
| Single-precision float | `float` | Single-precision decimal | `float z = 1.5;` |
| Boolean | `bool` | `True` or `False` | `bool b = True;` |
| Character | `char` | Single ASCII character | `char c = 'a';` |
| String | `String` | Sequence of characters | `String s = "hello";` |

### Special Values

| Value | Description |
|-------|-------------|
| `null` | Represents the absence of a value |
| `True` | Boolean true literal |
| `False` | Boolean false literal |
| `pi` | Built-in constant for π (`3.14159...`) |
| `e` | Built-in constant for Euler's number (`2.71828...`) |

---

## Tokens

The language is first broken down into tokens during lexical analysis, defined using Alex.

### Keywords

| Keyword | Purpose |
|---------|---------|
| `let` | Expression-scoped variable binding |
| `in` | Body of a `let` expression |
| `if` | Conditional expression |
| `else` | Alternative branch of a conditional |
| `int` | Declare an integer variable |
| `double` | Declare a double variable |
| `float` | Declare a float variable |
| `bool` | Declare a boolean variable |
| `char` | Declare a character variable |
| `String` | Declare a string variable |

### Identifiers

Variable and function names consisting of letters, digits, and underscores, starting with a letter or underscore. Examples: `x`, `myVar`, `count_1`.

### Literals

| Literal | Examples |
|---------|---------|
| Integer | `5`, `10`, `42` |
| Double / Float | `3.14`, `2.0e5`, `.5`, `1e-3` |
| Character | `'a'`, `'\n'`, `'\\'` |
| String | `"hello"`, `"world\n"` |
| Boolean | `True`, `False` |

---

## Operators

### Arithmetic Operators

| Operator | Description | Example |
|----------|-------------|---------|
| `+` | Addition | `x + 1` |
| `-` | Subtraction | `x - 1` |
| `*` | Multiplication | `x * 2` |
| `/` | Division | `x / 2` |
| `%` | Modulo | `x % 3` |

### Arithmetic Assignment Operators

These operators update a variable in place and are equivalent to writing the full expression longhand:

| Operator | Equivalent To | Example |
|----------|---------------|---------|
| `+=` | `x = x + val` | `x += 5;` |
| `-=` | `x = x - val` | `x -= 3;` |
| `*=` | `x = x * val` | `x *= 2;` |
| `/=` | `x = x / val` | `x /= 4;` |
| `%=` | `x = x % val` | `x %= 3;` |

### Comparison Operators

Comparison operators always return a `bool`:

| Operator | Description | Example |
|----------|-------------|---------|
| `==` | Equal to | `x == 5` |
| `!=` | Not equal to | `x != 0` |
| `<` | Less than | `x < 10` |
| `<=` | Less than or equal to | `x <= 10` |
| `>` | Greater than | `x > 0` |
| `>=` | Greater than or equal to | `x >= 0` |

### Logical Operators

Logical operators work on `bool` values. `&&` and `||` use **short-circuit evaluation**:

| Operator | Description | Example |
|----------|-------------|---------|
| `&&` | Logical AND | `a && b` |
| `\|\|` | Logical OR | `a \|\| b` |
| `!` | Logical NOT | `!flag` |

### Bitwise Operators

Bitwise operators work on `int` values only:

| Operator | Description | Example |
|----------|-------------|---------|
| `&` | Bitwise AND | `x & 0xFF` |
| `\|` | Bitwise OR | `x \| mask` |
| `^` | Bitwise XOR | `x ^ y` |
| `<<` | Left shift | `x << 2` |
| `>>` | Right shift | `x >> 1` |

### Bitwise Assignment Operators

| Operator | Equivalent To | Example |
|----------|---------------|---------|
| `&=` | `x = x & val` | `x &= 0xFF;` |
| `\|=` | `x = x \| val` | `x \|= mask;` |
| `^=` | `x = x ^ val` | `x ^= bits;` |
| `<<=` | `x = x << val` | `x <<= 2;` |
| `>>=` | `x = x >> val` | `x >>= 1;` |

### Assignment

| Operator | Description | Example |
|----------|-------------|---------|
| `=` | Assign a value to a variable | `x = 10` |

### Operator Precedence

Precedence follows C conventions (highest to lowest):

| Precedence | Operators |
|------------|-----------|
| Highest | Unary: `!`, `-` |
| | `*`, `/`, `%` |
| | `+`, `-` |
| | `<<`, `>>` |
| | `<`, `<=`, `>`, `>=` |
| | `==`, `!=` |
| | `&` |
| | `^` |
| | `\|` |
| | `&&` |
| Lowest | `\|\|` |

---

## Statements and Structure

### Line Separation — Semicolons

Every statement must end with a semicolon (`;`). This separates statements and signals the end of an expression to the parser, consistent with C-style syntax.

```
int x = 5;
int y = x + 3;
```

Multi-line statements can be continued using a trailing `\` before the newline in the REPL.

### Sequences

Multiple statements are written in sequence, each terminated by `;`:

```
int a = 1;
int b = 2;
int c = a + b;
```

### Variable Declaration

Variables are declared with an explicit type and assigned a value at the same time:

```
int x = 10;
double y = 3.14;
String name = "Alice";
bool active = True;
```

Type casting is performed automatically when the declared type and the value type are compatible numeric types (e.g. assigning an `int` expression to a `double` variable).

### Variable Assignment

Once declared, a variable can be reassigned using `=` or any assignment operator:

```
x = 20;
x += 5;
```

### Let Expressions

Variables can also be bound in an expression-scoped context using `let ... in`:

```
let x = 5 in x + 1
```

The variable `x` only exists within the `in` body and is not added to the surrounding environment.

### If-Then-Else Conditions

Conditional logic uses `if`, `then` and `else` with braces `{}`:

```
if (x > 0) then { \
    String result = "Positive"; \
} else { \
    String result = "Non-positive"; \
}
```

- The condition must evaluate to a `bool`
- Both branches use `{}` to delimit their bodies
- The `else` branch is optional — if omitted and the condition is false, the block evaluates to `null`

### Lists

The language supports list literals using square brackets:

```
[1, 2, 3]
```

List ranges use `..` to generate a sequence:

```
[1..5]
```

This produces `[1, 2, 3, 4, 5]`. Ranges also work in reverse — if the start is greater than the end, the list counts downward.

List elements can be accessed by index using `[]`:

```
int first = myList[0];
```

Negative indices count from the end:

```
int last = myList[-1];
```

Lists can also be sliced:

```
myList[1:3];
myList[::2];
```

---

## Grammar

The syntax is defined using Happy. The grammar describes how tokens combine into valid programs.

### Example Grammar Rules

```
Stmt → TYPE IDENTIFIER = Expr ;
     | IDENTIFIER = Expr ;
     | IDENTIFIER AssignOp Expr ;
     | if ( Expr ) then { Stmts } else { Stmts }
     | if ( Expr ) then { Stmts }

Expr → Expr BinOp Expr
     | ! Expr
     | - Expr
     | let IDENTIFIER = Expr in Expr
     | function_name ( Args )
     | IDENTIFIER [ Expr ]
     | [ Exprs ]
     | [ Expr .. Expr ]
     | Literal
     | IDENTIFIER
     | ( Expr )
```

---

## Abstract Syntax Tree (AST)

After parsing, the program is represented as an AST defined in `Syntax.hs`.

### Main Node Types

- **Variable** — identifier reference
- **Literal** — integer, float, double, bool, char, string, or null value
- **Binary Operation** — two operands with an infix operator
- **Unary Operation** — single operand with `!` (not) or `-` (negate)
- **Assignment** — variable reassignment
- **Declaration** — typed variable declaration
- **AssignOp** — compound assignment (`+=`, `-=`, etc.)
- **If Expression** — conditional with optional else branch
- **Let Expression** — expression-scoped binding
- **Function Call** — built-in function applied to arguments
- **List Literal** — list of expressions
- **List Range** — range expression `[start..end]`
- **List Index** — element access by position
- **List Slice** — sub-list extraction

---

## Type System

The language includes a static type system implemented in `TypeChecker.hs`.

### Supported Types

`int`, `double`, `float`, `bool`, `char`, `String`

### Type Rules

- Arithmetic operators (`+`, `-`, `*`, `/`, `%`) require numeric operands (`int`, `float`, or `double`)
- Bitwise operators (`&`, `|`, `^`, `<<`, `>>`) require `int` operands
- Logical operators (`&&`, `||`, `!`) require `bool` operands
- Comparison operators return `bool`
- Declarations must match the declared type (with automatic numeric casting where applicable)
- Using an undefined variable is a type error
- Invalid operations are rejected before evaluation

Type checking is performed before execution to catch errors early and prevent invalid programs from running.

---

## Comments

The language supports two comment styles, consistent with C:

```
// This is a single-line comment

/* This is a
   multi-line comment */

/* Nested /* comments */ are also supported */
```

Block comments can be nested to any depth.

---

## Examples

### Variable Declaration and Arithmetic

```
int x = 10;
int y = x + 5;
double z = 3.14 * 2.0;
```

### If-Then-Else

```
int score = 85; \
if (score >= 50) then { \
    String result = "Pass"; \
} else { \
    String result = "Fail"; \
}
```

### Let Expression

```
let x = 5 in x * 2
```

### Boolean Logic with Short-Circuit Evaluation

```
bool a = True;
bool b = False;
bool c = a && !b;
bool d = False || True;
```

### Arithmetic Assignment Operators

```
int x = 10;
x += 5;
x *= 2;
```

### Bitwise Operations

```
int flags = 0;
flags |= 1;
flags &= 0xFF;
```

### Lists

```
[1, 2, 3, 4, 5]
[1..10]
```

---

## Design Decisions

- The language syntax is **based on C**, making it familiar and consistent with standard conventions
- A clear separation between lexer, parser, type checker, and evaluator is maintained
- Type checking is done before evaluation to prevent runtime errors
- Operator precedence follows C conventions and is handled in the parser
- Semicolons are required as statement terminators, consistent with C
- `&&` and `||` use short-circuit (lazy) evaluation — the right-hand side is only evaluated if necessary
- Nested block comments (`/* /* */ */`) are supported for convenience

---

## Summary

The language implements a full pipeline from source text to execution. It supports static typing, arithmetic and logical expressions, bitwise operations, conditional control flow, compound assignment operators, list operations, and built-in mathematical functions — all with a syntax grounded in C conventions.
