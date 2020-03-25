{-#LANGUAGE DataKinds, NoImplicitPrelude #-}

module Examples.Pascal (testsPascal) where 

import Algebra.Prelude hiding (leadingTerm)
import Test.Tasty
import Test.Tasty.HUnit as HU
import qualified Data.Map.Strict as MS
import Polynomial.Prelude
import Polynomial.Wu
    

u1 = var 0
u2 = var 1
u3 = var 2
u4 = var 3
u5 = var 4
u6 = var 5
x1 = var 6
x2 = var 7
x3 = var 8
x4 = var 9
x5 = var 10
x6 = var 11
x7 = var 12
x8 = var 13
x9 = var 14
x10 = var 15


h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,s1,s2,s3,g:: Polynomial' Lex 16

h1 = 2*u2*x2 + 2*u3*x1 - u3^2 - u2^2
h2 = 2*u1*x2 - u1^2
h3 = x3^2 - 2*x2*x3 - 2*u4*x1 + u4^2
h4 = x4^2 - 2*x2*x4 - 2*u5*x1 + u5^2
h5 = x5^2 - 2*x2*x5 - 2*u6*x1 + u6^2
h6 = (u5-u4)*x6 + u4*x4 - u4*x3
h7 = (u6-u5)*x8 - (x5-x4)*x7 + u5*x5 - u6*x4
h8 = u3*x8 - (u2-u1)*x7 - u1*u3
h9 = u6*x10 - x5*x9
h10 = (u4-u3)*x10 - (x3-x2)*x9 + u3*x3 - u2*u4
s1 = (u4-u3)*x5 + u6*x3 - u2*u6
s2 = u3*x5 - u3*x4 - (u2-u1)*u6 + (u2-u1)*u5
s3 = u1*u5 - u1*u4
g = x7*x10 - (x8-x6)*x9 - x6*x7

testTheorem :: TestTree
testTheorem = testCase "Test for Pascal Theorem" $ do
    theoremProver [h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,s1,s2,s3] g @?= sort [g]
--testReplacePoly :: TestTree

testsPascal :: TestTree
testsPascal = testGroup "Test for Pascal Theorem" [testTheorem]