module TPolynomial.TPrelude (testsPrelude) where

import Test.Tasty
import Test.Tasty.HUnit as HU
import Polynomial.Prelude

x, y :: Poly
x = var 2 0
y = var 2 1

testPseudoRemainder :: TestTree
testPseudoRemainder = testCase "Test for pseudo remainder" $ do
    -- (x^2 + 118*x - 599)  prem  (x - 5)  =  -4 -- after simplify reduces to a constant
    let r = snd (pseudoRemainder
                   (x ^ (2::Int) + 118 * x - 599)
                   (x - 5) 0)
    isZero r @?= False
    classVarDeg r 0 @?= 0

testsPrelude :: TestTree
testsPrelude = testGroup "Test for Prelude of Polynomials" [testPseudoRemainder]
