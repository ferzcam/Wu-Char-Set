module Examples.Apolonius (testApolonius) where

import Test.Tasty
import Test.Tasty.HUnit as HU
import Polynomial.Prelude
import Polynomial.TheoremProver

-- Arity 10: indices 0..9
x1, x2, x3, x4, x5, x6, x7, x8, u1, u2 :: Poly
x1 = var 10 7
x2 = var 10 6
x3 = var 10 5
x4 = var 10 4
x5 = var 10 3
x6 = var 10 2
x7 = var 10 1
x8 = var 10 0
u1 = var 10 8
u2 = var 10 9

h1, h2, h3, h4, h5, h6, h7, h8, g :: Poly
h1 = 2 * x1 - u1
h2 = 2 * x2 - u2
h3 = 2 * x3 - u1
h4 = 2 * x4 - u2
h5 = u2 * x5 + u1 * x6 - u1 * u2
h6 = u1 * x5 - u2 * x6
h7 = x1 ^ (2::Int) - x2 ^ (2::Int) - 2 * x1 * x7 + 2 * x2 * x8
h8 = x1 ^ (2::Int) - 2 * x1 * x7 - x3 ^ (2::Int) + 2 * x3 * x7
     - x4 ^ (2::Int) + 2 * x4 * x8
g  = (x5 - x7) ^ (2::Int) + (x6 - x8) ^ (2::Int)
     - (x1 - x7) ^ (2::Int) - x8 ^ (2::Int)

testTheorem :: TestTree
testTheorem = testCase "Test for Apolonius Theorem" $
    isZero (last (theoremProver 8 [h1, h2, h3, h4, h5, h6, h7, h8] g)) @?= True

testApolonius :: TestTree
testApolonius = testGroup "Test for Apolonius Theorem" [testTheorem]
