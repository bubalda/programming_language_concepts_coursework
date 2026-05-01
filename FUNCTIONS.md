# Functions

## Overview

This document describes the built-in functions available in the language. These functions are provided as part of the standard environment and can be called directly without any imports or declarations.

Functions are called using the following syntax:

```
function_name(param);
```

For functions that take multiple parameters:

```
function_name(param1, param2);
```

Function names are **case-insensitive** — `Sin(x)`, `SIN(x)`, and `sin(x)` are all valid.

---

## Built-in Constants

The following constants are available without any function call:

| Constant | Value | Description |
|----------|-------|-------------|
| `pi` | `3.14159265358979...` | Mathematical constant π |
| `e` | `2.71828182845904...` | Euler's number |

---

## Power and Root Functions

| Function | Arguments | Description | Example |
|----------|-----------|-------------|---------|
| `sqrt(x)` | 1 numeric | Square root of `x` (x ≥ 0) | `sqrt(9.0);` → `3.0` |
| `cbrt(x)` | 1 numeric | Cube root of `x` | `cbrt(27.0);` → `3.0` |
| `pow(x, y)` | 2 numeric | `x` raised to the power `y` | `pow(2.0, 3.0);` → `8.0` |
| `exp(x)` | 1 numeric | Euler's number raised to `x` (eˣ) | `exp(1.0);` → `2.7182...` |
| `exp10(x)` | 1 numeric | 10 raised to the power `x` | `exp10(2.0);` → `100.0` |
| `square(x)` | 1 numeric | `x` squared (x²) | `square(4.0);` → `16.0` |
| `cube(x)` | 1 numeric | `x` cubed (x³) | `cube(3.0);` → `27.0` |

---

## Trigonometric Functions

All trigonometric functions expect angles in **radians**.

| Function | Arguments | Description | Example |
|----------|-----------|-------------|---------|
| `sin(x)` | 1 numeric | Sine of `x` | `sin(pi);` → `0.0` |
| `cos(x)` | 1 numeric | Cosine of `x` | `cos(0.0);` → `1.0` |
| `tan(x)` | 1 numeric | Tangent of `x` | `tan(pi);` → `0.0` |
| `asin(x)` | 1 numeric, x ∈ [-1, 1] | Arc sine of `x` | `asin(1.0);` → `1.5707...` |
| `acos(x)` | 1 numeric, x ∈ [-1, 1] | Arc cosine of `x` | `acos(1.0);` → `0.0` |
| `atan(x)` | 1 numeric | Arc tangent of `x` | `atan(1.0)` → `0.7853...` |
| `atan2(y, x)` | 2 numeric | Arc tangent of `y/x` using sign of both | `atan2(1.0, 1.0);` → `0.7853...` |
| `sec(x)` | 1 numeric | Secant of `x` (1 / cos x) | `sec(0.0);` → `1.0` |
| `csc(x)` | 1 numeric | Cosecant of `x` (1 / sin x) | `csc(1.5707);` → `1.0` |
| `cot(x)` | 1 numeric | Cotangent of `x` (1 / tan x) | `cot(0.7853);` → `1.0` |
| `versin(x)` | 1 numeric | Versine of `x` (1 - cos x) | `versin(0.0);` → `0.0` |
| `exsec(x)` | 1 numeric | Exsecant of `x` (sec x - 1) | `exsec(0.0);` → `0.0` |

---

## Hyperbolic Functions

| Function | Arguments | Description | Example |
|----------|-----------|-------------|---------|
| `sinh(x)` | 1 numeric | Hyperbolic sine | `sinh(0.0);` → `0.0` |
| `cosh(x)` | 1 numeric | Hyperbolic cosine | `cosh(0.0);` → `1.0` |
| `tanh(x)` | 1 numeric | Hyperbolic tangent | `tanh(0.0);` → `0.0` |
| `asinh(x)` | 1 numeric | Inverse hyperbolic sine | `asinh(0.0);` → `0.0` |
| `acosh(x)` | 1 numeric, x ≥ 1 | Inverse hyperbolic cosine | `acosh(1.0);` → `0.0` |
| `sech(x)` | 1 numeric | Hyperbolic secant (1 / cosh x) | `sech(0.0);` → `1.0` |
| `csch(x)` | 1 numeric | Hyperbolic cosecant (1 / sinh x) | — |
| `coth(x)` | 1 numeric | Hyperbolic cotangent (1 / tanh x) | — |

---

## Logarithmic Functions

| Function | Arguments | Description | Example |
|----------|-----------|-------------|---------|
| `ln(x)` | 1 numeric, x > 0 | Natural logarithm (base e) | `ln(1.0);` → `0.0` |
| `log(x)` | 1 numeric, x > 0 | Base-10 logarithm | `log(100.0);` → `2.0` |
| `log(base, x)` | 2 numeric | Logarithm of `x` in given `base` | `log(2.0, 8.0);` → `3.0` |
| `log2(x)` | 1 numeric, x > 0 | Base-2 logarithm | `log2(8.0);` → `3.0` |
| `log10(x)` | 1 numeric, x > 0 | Base-10 logarithm (explicit) | `log10(1000.0);` → `3.0` |
| `log1p(x)` | 1 numeric, x > -1 | Natural log of (1 + x) | `log1p(0.0);` → `0.0` |

---

## Combinatorial Functions

| Function | Arguments | Description | Example |
|----------|-----------|-------------|---------|
| `fact(n)` | 1 int, n ≥ 0 | Factorial n! | `fact(5);` → `120` |
| `fact2(n)` | 1 int, n ≥ 0 | Double factorial n!! | `fact2(5);` → `15` |
| `comb(n, r)` | 2 int, 0 ≤ r ≤ n | Combinations — n choose r | `comb(5, 2);` → `10` |
| `perm(n, r)` | 2 int, 0 ≤ r ≤ n | Permutations P(n, r) | `perm(5, 2);` → `20` |
| `fib(n)` | 1 int, n ≥ 0 | n-th Fibonacci number | `fib(10);` → `55` |
| `gamma(x)` | 1 numeric | Gamma function Γ(x) | `gamma(5.0);` → `24.0` |
| `gcd(a, b, ...)` | 1+ int | Greatest common divisor | `gcd(12, 8);` → `4` |
| `lcm(a, b, ...)` | 1+ int | Least common multiple | `lcm(4, 6);` → `12` |

---

## Statistical Functions

Statistical functions accept multiple numeric arguments or a single list.

| Function | Arguments | Description | Example |
|----------|-----------|-------------|---------|
| `mean(...)` | 1+ numeric | Arithmetic mean | `mean([1, 2, 3]);` → `2.0` |
| `median(...)` | 1+ numeric | Middle value of sorted inputs | `median([1, 3, 2]);` → `2.0` |
| `mode(...)` | 1+ numeric | Most frequently occurring value | `mode([1, 2, 2, 3]);` → `2.0` |
| `sum(...)` | 1+ numeric | Sum of all values | `sum([1, 2, 3]);` → `6.0` |
| `product(...)` | 1+ numeric | Product of all values | `product([2, 3, 4]);` → `24.0` |
| `min(...)` | 1+ numeric | Minimum value | `min([3, 1, 2]);` → `1.0` |
| `max(...)` | 1+ numeric | Maximum value | `max([3, 1, 2]);` → `3.0` |
| `stddev(...)` | 1+ numeric | Population standard deviation | `stddev([2, 4, 4, 6]);` → `1.414...` |

---

## List Functions

| Function | Arguments | Description | Example |
|----------|-----------|-------------|---------|
| `length(xs)` | 1 list or String | Number of elements | `length([1,2,3]);` → `3` |

---

## Usage Examples

### Basic function call

```
double result = sqrt(16.0);
```

### Using a function inside an expression

```
double y = sin(pi) + cos(0.0);
```

### Nesting function calls

```
double z = sqrt(pow(3.0, 2.0) + pow(4.0, 2.0));
```

### Combinatorics

```
int ways = comb(10, 3);
int arrangements = perm(5, 2);
```

### Statistics

```
double avg = mean(10, 20, 30, 40);
double spread = stddev(10, 20, 30, 40);
```

---

## Error Conditions

Functions validate their inputs and produce a runtime error if constraints are violated:

| Condition | Error Message |
|-----------|---------------|
| `sqrt(x)` where x < 0 | `sqrt: input must be >= 0` |
| `asin(x)` / `acos(x)` where x ∉ [-1,1] | `asin/acos: input must be in [-1,1]` |
| `ln`, `log`, `log2`, `log10` where x ≤ 0 | `ln/log: input must be > 0` |
| `acosh(x)` where x < 1 | `acosh: input must be >= 1` |
| `fact(n)` / `fib(n)` where n < 0 | `fact/fib: input must be >= 0` |
| Reciprocal of zero (e.g. `sec`, `csc`, `cot`) | reciprocal of zero error |
| Wrong number of arguments | `ParamError: The function expects exactly N argument(s)` |
| Unknown function name | `Unknown function: \`name\`` |
