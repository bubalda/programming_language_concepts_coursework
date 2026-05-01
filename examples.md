# Example Programs (Task 9)

## Example 1: Quadratic Equation Solver

**Description:**  
Solves a quadratic equation ax² + bx + c = 0 using the discriminant formula.

**Code:**
```
let a = 1 in let b = -3 in let c = 2 in let d = bb - 4ac in (-b + sqrt(d)) / (2a);
```

**Expected Output:**  
2.0

## Example 2: Statistical Data Analysis

**Description:**  
Computes mean, median, and standard deviation for a dataset.

**Code:**
```
let data = [1,2,3,4,5,6,7,8,9,10] in mean(data);
```

**Expected Output:**  
5.5

## Example 3: Compound Interest Calculator

**Description:**  
Calculates future value using exponential growth.

**Code:**
```
let P = 1000 in let r = 0.05 in let t = 5 in P * exp(r * t);
```

**Expected Output:**  
≈ 1284.03

## Example 4: Triangle Side Calculation (Cosine Rule)

**Description:**  
Finds the third side using trigonometry.

**Code:**
```
let a = 5 in let b = 7 in let angle = 1.0472 in sqrt(aa + bb - 2ab*cos(angle));
```

**Expected Output:**  
≈ 6.24

## Example 5: Population Growth Model (Fibonacci)

**Description:**  
Models growth using Fibonacci sequence.

**Code:**
```
fib(10);
```

**Expected Output:**  
55

## Example 6: Logarithmic Scaling

**Description:**  
Computes logarithmic values.

**Code:**
```
log10(1000);
```

**Expected Output:**  
3.0

## Example 7: Radioactive Decay Model

**Description:**  
Models exponential decay over time.

**Code:**
```
let N0 = 100 in let lambda = 0.1 in let t = 10 in N0 * exp(-lambda * t);
```

**Expected Output:**  
≈ 36.79

## Example 8: Combinatorics (Lottery Probability)

**Description:**  
Calculates combinations and permutations.

**Code:**
```
comb(10,3);
```

**Expected Output:**  
120

## Example 9: Data Aggregation

**Description:**  
Performs statistical aggregation on dataset.

**Code:**
```
let data = [5,10,15,20,25] in sum(data);
```

**Expected Output:**  
75

## Example 10: Hyperbolic Function Modeling

**Description:**  
Evaluates hyperbolic functions.

**Code:**
```
sinh(1);
```

**Expected Output:**  
≈ 1.175