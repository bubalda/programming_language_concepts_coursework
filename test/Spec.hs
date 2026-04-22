import Test.Hspec
import qualified Data.Map as Map

import Lang.Eval.Eval (evalExpr, runEval)
import Lang.Syntax.Syntax (Expr(..), TwoExprOperator(..))
import Lang.Eval.Types (Value(..))

-- Helper runner
runTest :: Expr -> Either String Value
runTest expr = runEval (evalExpr Map.empty expr)

-- Helper check
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _ = False

main :: IO ()
main = hspec $ do

    -- CORE ARITHMETIC

    describe "Core Arithmetic" $ do

        it "1 + 2 = 3" $
            runTest (BinOp Add (IntLit 1) (IntLit 2))
                `shouldBe` Right (VInt 3)

        it "large multiplication" $
            runTest (BinOp Mul (IntLit 1000) (IntLit 2000))
                `shouldBe` Right (VInt 2000000)

        it "invalid addition error" $
            runTest (BinOp Add (IntLit 1) (StringLit "a"))
                `shouldSatisfy` isLeft
        it "subtraction works" $
            runTest (BinOp Sub (IntLit 5) (IntLit 3))
                `shouldBe` Right (VInt 2)

        it "division works" $
            runTest (BinOp Div (IntLit 10) (IntLit 2))
                `shouldBe` Right (VInt 5)

        it "division by zero error" $
            runTest (BinOp Div (IntLit 10) (IntLit 0))
                `shouldSatisfy` isLeft

        it "addition large numbers" $
            runTest (BinOp Add (IntLit 10000) (IntLit 20000))
                `shouldBe` Right (VInt 30000)

        it "subtraction negative result" $
            runTest (BinOp Sub (IntLit 3) (IntLit 5))
                `shouldBe` Right (VInt (-2))

        it "multiplication negative" $
            runTest (BinOp Mul (IntLit (-2)) (IntLit 5))
                `shouldBe` Right (VInt (-10))

        it "division negative result" $
            runTest (BinOp Div (IntLit (-10)) (IntLit 2))
                `shouldBe` Right (VInt (-5))

    -- POWER / ROOTS

    describe "Power and Roots" $ do

        it "sqrt(4)" $
            runTest (Call (Var "sqrt") [IntLit 4])
                `shouldBe` Right (VDouble 2.0)

        it "cbrt(27)" $
            runTest (Call (Var "cbrt") [IntLit 27])
                `shouldBe` Right (VDouble 3.0)

        it "pow(2,3)" $
            runTest (Call (Var "pow") [IntLit 2, IntLit 3])
                `shouldBe` Right (VDouble 8.0)

        it "exp(1)" $
            runTest (Call (Var "exp") [IntLit 1])
                `shouldSatisfy` isRight

        it "square(5)" $
            runTest (Call (Var "square") [IntLit 5])
                `shouldBe` Right (VDouble 25.0)

        it "cube(3)" $
            runTest (Call (Var "cube") [IntLit 3])
                `shouldBe` Right (VDouble 27.0)

        it "exp10(2)" $
            runTest (Call (Var "exp10") [IntLit 2])
                `shouldBe` Right (VDouble 100.0)

        it "sqrt(-1) error" $
            runTest (Call (Var "sqrt") [IntLit (-1)])
                `shouldSatisfy` isLeft

        it "pow large numbers" $
            runTest (Call (Var "pow") [IntLit 10, IntLit 5])
                `shouldBe` Right (VDouble 100000.0)

        it "cbrt negative number" $
            runTest (Call (Var "cbrt") [IntLit (-8)])
                `shouldBe` Right (VDouble (-2.0))

        it "exp large input" $
            runTest (Call (Var "exp") [IntLit 5])
                `shouldSatisfy` isRight

        it "square negative" $
            runTest (Call (Var "square") [IntLit (-3)])
                `shouldBe` Right (VDouble 9.0)

        it "cube negative" $
            runTest (Call (Var "cube") [IntLit (-2)])
                `shouldBe` Right (VDouble (-8.0))

        it "cbrt invalid" $
            runTest (Call (Var "cbrt") [StringLit "a"])
                `shouldSatisfy` isLeft

        it "exp invalid" $
            runTest (Call (Var "exp") [StringLit "a"])
                `shouldSatisfy` isLeft

        it "square invalid" $
            runTest (Call (Var "square") [StringLit "a"])
                `shouldSatisfy` isLeft

        it "cube invalid" $
            runTest (Call (Var "cube") [StringLit "a"])
                `shouldSatisfy` isLeft

        it "exp10 invalid" $
            runTest (Call (Var "exp10") [StringLit "a"])
                `shouldSatisfy` isLeft

        it "sqrt large" $
            runTest (Call (Var "sqrt") [IntLit 1000000])
                `shouldBe` Right (VDouble 1000.0)

        it "exp10 large" $
            runTest (Call (Var "exp10") [IntLit 5])
                `shouldBe` Right (VDouble 100000.0)

    -- TRIGONOMETRIC

    describe "Trigonometric" $ do

        it "sin(0)" $
            runTest (Call (Var "sin") [IntLit 0])
                `shouldBe` Right (VDouble 0.0)

        it "cos(0)" $
            runTest (Call (Var "cos") [IntLit 0])
                `shouldBe` Right (VDouble 1.0)

        it "tan(0)" $
            runTest (Call (Var "tan") [IntLit 0])
                `shouldBe` Right (VDouble 0.0)

        it "asin(0.5)" $
            runTest (Call (Var "asin") [DoubleLit 0.5])
                `shouldSatisfy` isRight

        it "acos(0.5)" $
            runTest (Call (Var "acos") [DoubleLit 0.5])
                `shouldSatisfy` isRight

        it "atan(1)" $
            runTest (Call (Var "atan") [IntLit 1])
                `shouldSatisfy` isRight

        it "atan2(1,1)" $
            runTest (Call (Var "atan2") [IntLit 1, IntLit 1])
                `shouldSatisfy` isRight

        it "sec(0)" $
            runTest (Call (Var "sec") [IntLit 0])
                `shouldSatisfy` isRight

        it "csc(1)" $
            runTest (Call (Var "csc") [IntLit 1])
                `shouldSatisfy` isRight

        it "cot(1)" $
            runTest (Call (Var "cot") [IntLit 1])
                `shouldSatisfy` isRight

        it "versin(0)" $
            runTest (Call (Var "versin") [IntLit 0])
                `shouldBe` Right (VDouble 0.0)

        it "exsec(0)" $
            runTest (Call (Var "exsec") [IntLit 0])
            `shouldBe` Right (VDouble 0.0)


        it "sin(pi approx)" $
            runTest (Call (Var "sin") [DoubleLit 3.14159])
                `shouldSatisfy` isRight

        it "cos(pi)" $
            runTest (Call (Var "cos") [DoubleLit 3.14159])
                `shouldSatisfy` isRight

        it "tan(pi/4)" $
            runTest (Call (Var "tan") [DoubleLit 0.785398])
                `shouldSatisfy` isRight

        it "asin out of range error" $
            runTest (Call (Var "asin") [IntLit 2])
                `shouldSatisfy` isLeft

        it "acos out of range error" $
            runTest (Call (Var "acos") [IntLit 2])
                `shouldSatisfy` isLeft

        it "sin invalid" $
            runTest (Call (Var "sin") [StringLit "a"])
                `shouldSatisfy` isLeft

        it "cos invalid" $
            runTest (Call (Var "cos") [StringLit "a"])
                `shouldSatisfy` isLeft

        it "tan invalid" $
            runTest (Call (Var "tan") [StringLit "a"])
                `shouldSatisfy` isLeft

        it "atan2 invalid" $
            runTest (Call (Var "atan2") [StringLit "a", IntLit 1])
                `shouldSatisfy` isLeft

        it "sec invalid" $
            runTest (Call (Var "sec") [StringLit "a"])
            `shouldSatisfy` isLeft

        it "csc invalid" $
            runTest (Call (Var "csc") [StringLit "a"])
                `shouldSatisfy` isLeft

        it "cot invalid" $
            runTest (Call (Var "cot") [StringLit "a"])
                `shouldSatisfy` isLeft

        it "versin invalid" $
            runTest (Call (Var "versin") [StringLit "a"])
                `shouldSatisfy` isLeft

        it "exsec invalid" $
            runTest (Call (Var "exsec") [StringLit "a"])
                `shouldSatisfy` isLeft

    -- LOGARITHMIC

    describe "Logarithmic" $ do

        it "ln(e)" $
            runTest (Call (Var "ln") [DoubleLit (exp 1)])
                `shouldSatisfy` isRight

        it "log10(100)" $
            runTest (Call (Var "log10") [IntLit 100])
                `shouldBe` Right (VDouble 2.0)

        it "log2(8)" $
            runTest (Call (Var "log2") [IntLit 8])
                `shouldBe` Right (VDouble 3.0)

        it "log1p(1)" $
            runTest (Call (Var "log1p") [IntLit 1])
                `shouldSatisfy` isRight

        it "log(10,100)" $
            runTest (Call (Var "log") [IntLit 10, IntLit 100])
                `shouldBe` Right (VDouble 2.0)

        it "log(-1) error" $
            runTest (Call (Var "log") [IntLit (-1)])
                `shouldSatisfy` isLeft

        it "ln(1) = 0" $
            runTest (Call (Var "ln") [IntLit 1])
                `shouldBe` Right (VDouble 0.0)

        it "log base form" $
            runTest (Call (Var "log") [IntLit 2, IntLit 8])
                `shouldBe` Right (VDouble 3.0)

        it "log base invalid (base 1)" $
            runTest (Call (Var "log") [IntLit 1, IntLit 10])
                `shouldSatisfy` isLeft

        it "log1p invalid" $
            runTest (Call (Var "log1p") [IntLit (-2)])
                `shouldSatisfy` isLeft

        it "log10 invalid" $
            runTest (Call (Var "log10") [StringLit "a"])
                `shouldSatisfy` isLeft

        it "log2 invalid" $
            runTest (Call (Var "log2") [StringLit "a"])
                `shouldSatisfy` isLeft

        it "ln invalid" $
            runTest (Call (Var "ln") [StringLit "a"])
                `shouldSatisfy` isLeft

    -- COMBINATORIAL

    describe "Combinatorial" $ do

        it "fact(5)" $
            runTest (Call (Var "fact") [IntLit 5])
                `shouldBe` Right (VInt 120)

        it "fact2(6)" $
            runTest (Call (Var "fact2") [IntLit 6])
                `shouldBe` Right (VInt 48)

        it "comb(5,2)" $
            runTest (Call (Var "comb") [IntLit 5, IntLit 2])
                `shouldBe` Right (VInt 10)

        it "perm(5,2)" $
            runTest (Call (Var "perm") [IntLit 5, IntLit 2])
                `shouldBe` Right (VInt 20)

        it "fib(10)" $
            runTest (Call (Var "fib") [IntLit 10])
                `shouldBe` Right (VInt 55)

        it "gcd(12,18)" $
            runTest (Call (Var "gcd") [IntLit 12, IntLit 18])
                `shouldBe` Right (VInt 6)

        it "lcm(4,6)" $
            runTest (Call (Var "lcm") [IntLit 4, IntLit 6])
                `shouldBe` Right (VInt 12)

        it "fact(-1) error" $
            runTest (Call (Var "fact") [IntLit (-1)])
                `shouldSatisfy` isLeft

        it "fact2 odd number" $
            runTest (Call (Var "fact2") [IntLit 5])
                `shouldBe` Right (VInt 15)

        it "comb invalid (r > n)" $
            runTest (Call (Var "comb") [IntLit 3, IntLit 5])
                `shouldSatisfy` isLeft

        it "perm invalid (r > n)" $
            runTest (Call (Var "perm") [IntLit 3, IntLit 5])
                `shouldSatisfy` isLeft

        it "fib(0)" $
            runTest (Call (Var "fib") [IntLit 0])
                `shouldBe` Right (VInt 0)

        it "gamma invalid integer" $
            runTest (Call (Var "gamma") [IntLit (-1)])
                `shouldSatisfy` isLeft

        it "gcd invalid" $
            runTest (Call (Var "gcd") [StringLit "a", IntLit 2])
                `shouldSatisfy` isLeft

        it "lcm invalid" $
            runTest (Call (Var "lcm") [StringLit "a", IntLit 2])
                `shouldSatisfy` isLeft

        it "gamma normal" $
            runTest (Call (Var "gamma") [IntLit 5])
                `shouldSatisfy` isRight

    -- STATISTICS

    describe "Statistics" $ do

        it "mean([1,2,3])" $
            runTest (Call (Var "mean") [ListLit [IntLit 1, IntLit 2, IntLit 3]])
                `shouldBe` Right (VDouble 2.0)

        it "median([1,2,3])" $
            runTest (Call (Var "median") [ListLit [IntLit 1, IntLit 2, IntLit 3]])
                `shouldBe` Right (VDouble 2.0)

        it "mode([1,1,2])" $
            runTest (Call (Var "mode") [ListLit [IntLit 1, IntLit 1, IntLit 2]])
                `shouldBe` Right (VDouble 1.0)

        it "sum([1,2,3])" $
            runTest (Call (Var "sum") [ListLit [IntLit 1, IntLit 2, IntLit 3]])
                `shouldBe` Right (VDouble 6.0)

        it "product([1,2,3])" $
            runTest (Call (Var "product") [ListLit [IntLit 1, IntLit 2, IntLit 3]])
                `shouldBe` Right (VDouble 6.0)

        it "min([1,2,3])" $
            runTest (Call (Var "min") [ListLit [IntLit 1, IntLit 2, IntLit 3]])
                `shouldBe` Right (VDouble 1.0)

        it "max([1,2,3])" $
            runTest (Call (Var "max") [ListLit [IntLit 1, IntLit 2, IntLit 3]])
                `shouldBe` Right (VDouble 3.0)

        it "stddev([1,2,3])" $
            runTest (Call (Var "stddev") [ListLit [IntLit 1, IntLit 2, IntLit 3]])
                `shouldSatisfy` isRight

        it "mean([]) error" $
            runTest (Call (Var "mean") [ListLit []])
                `shouldSatisfy` isLeft

        it "median even list" $
            runTest (Call (Var "median") [ListLit [IntLit 1, IntLit 2, IntLit 3, IntLit 4]])
                `shouldBe` Right (VDouble 2.5)

        it "mode multiple values" $
            runTest (Call (Var "mode") [ListLit [IntLit 1, IntLit 2, IntLit 2, IntLit 3]])
                `shouldBe` Right (VDouble 2.0)

        it "sum empty list" $
            runTest (Call (Var "sum") [ListLit []])
                `shouldBe` Right (VDouble 0.0)

        it "product empty list" $
            runTest (Call (Var "product") [ListLit []])
                `shouldBe` Right (VDouble 1.0)

        it "stddev larger set" $
            runTest (Call (Var "stddev") [ListLit (map IntLit [1..10])])
                `shouldSatisfy` isRight

        it "median invalid" $
            runTest (Call (Var "median") [ListLit []])
                `shouldSatisfy` isLeft

        it "mode invalid" $
            runTest (Call (Var "mode") [ListLit []])
                `shouldSatisfy` isLeft

        it "min invalid" $
            runTest (Call (Var "min") [ListLit []])
                `shouldSatisfy` isLeft

        it "max invalid" $
            runTest (Call (Var "max") [ListLit []])
                `shouldSatisfy` isLeft

        it "min large list" $
            runTest (Call (Var "min") [ListLit (map IntLit [1..100])])
                `shouldBe` Right (VDouble 1.0)

        it "max large list" $
            runTest (Call (Var "max") [ListLit (map IntLit [1..100])])
                `shouldBe` Right (VDouble 100.0)


    -- HYPERBOLIC

    describe "Hyperbolic" $ do

        it "sinh(0)" $
            runTest (Call (Var "sinh") [IntLit 0])
                `shouldBe` Right (VDouble 0.0)

        it "cosh(0)" $
            runTest (Call (Var "cosh") [IntLit 0])
                `shouldBe` Right (VDouble 1.0)

        it "tanh(0)" $
            runTest (Call (Var "tanh") [IntLit 0])
                `shouldBe` Right (VDouble 0.0)

        it "asinh(1)" $
            runTest (Call (Var "asinh") [IntLit 1])
                `shouldSatisfy` isRight

        it "acosh(2)" $
            runTest (Call (Var "acosh") [IntLit 2])
                `shouldSatisfy` isRight

        it "invalid hyperbolic input" $
            runTest (Call (Var "sinh") [StringLit "abc"])
                `shouldSatisfy` isLeft

        it "tanh large value" $
            runTest (Call (Var "tanh") [IntLit 5])
                `shouldSatisfy` isRight

        it "csch(0) error" $
            runTest (Call (Var "csch") [IntLit 0])
                `shouldSatisfy` isLeft

        it "sech(0)" $
            runTest (Call (Var "sech") [IntLit 0])
                `shouldBe` Right (VDouble 1.0)

        it "coth(0) error" $
            runTest (Call (Var "coth") [IntLit 0])
                `shouldSatisfy` isLeft

        it "cosh invalid" $
            runTest (Call (Var "cosh") [StringLit "a"])
                `shouldSatisfy` isLeft

        it "tanh invalid" $
            runTest (Call (Var "tanh") [StringLit "a"])
                `shouldSatisfy` isLeft

        it "sech invalid" $
            runTest (Call (Var "sech") [StringLit "a"])
                `shouldSatisfy` isLeft

        it "csch invalid" $
            runTest (Call (Var "csch") [StringLit "a"])
                `shouldSatisfy` isLeft

        it "coth invalid" $
            runTest (Call (Var "coth") [StringLit "a"])
                `shouldSatisfy` isLeft

-- helper
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _ = False