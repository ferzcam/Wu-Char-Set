{-#LANGUAGE DataKinds, NoImplicitPrelude #-}

module Examples.Pascal (testPascal) where 

import Algebra.Prelude hiding (leadingTerm)
import Test.Tasty
import Test.Tasty.HUnit as HU
import qualified Data.Map.Strict as MS
import Polynomial.Prelude
import Polynomial.Wu
    


x1 = var 9
x2 = var 8
x3 = var 7
x4 = var 6
x5 = var 5
x6 = var 4
x7 = var 3
x8 = var 2
x9 = var 1
x10 = var 0
u1 = var 10
u2 = var 11
u3 = var 12
u4 = var 13
u5 = var 14
u6 = var 15


h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,g:: Polynomial' Grevlex 16

h1 = 2*u2*x2 + 2*u3*x1 - u3^2 - u2^2
h2 = 2*u1*x2 - u1^2
h3 = x3^2 - 2*x2*x3 - 2*u4*x1 + u4^2
h4 = x4^2 - 2*x2*x4 - 2*u5*x1 + u5^2
h5 = x5^2 - 2*x2*x5 - 2*u6*x1 + u6^2
h6 = (u5-u4)*x6 + u4*x4 - u5*x3
h7 = (u6-u5)*x8 - (x5-x4)*x7 + u5*x5 - u6*x4
h8 = u3*x8 - (u2-u1)*x7 - u1*u3
h9 = u6*x10 - x5*x9
h10 = (u4-u3)*x10 - (x3-u2)*x9 + u3*x3 - u2*u4

g = x7*x10 - (x8-x6)*x9 - x6*x7

testTheorem :: TestTree
testTheorem = testCase "Test for Pascal Theorem" $ do
    last (theoremProver [h1,h2,h3,h4,h5,h6,h7,h8,h9,h10] g) @?= 0

testPascal :: TestTree
testPascal = testGroup "Test for Pascal Theorem" [testTheorem]