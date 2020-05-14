{-#LANGUAGE DataKinds#-}

module TPolynomial.TSpoly (testSpoly) where 


import Algebra.Ring.Polynomial hiding (leadingTerm)
import Test.Tasty
import Test.Tasty.HUnit as HU
import qualified Data.Map.Strict as MS
import Polynomial.Prelude
import Polynomial.Spoly
import Control.Arrow

x = var 0
y = var 1

s1,s2,r1,r2 :: Polynomial' 2
s1 = y^2 -  x^2 -  x^3
s2 = y^2 +  x^2 - 1

r1 = x*y^2 + 2*y^2 - x - 1
r2 = y^4 + y^2 - 1


testPseudoRemainder :: TestTree
testPseudoRemainder = testCase "Test for pseudo remainder with Spoly" $ do
    psRemSpoly s1 s2 0 @?=  x*y^2 + 2*y^2 - x - 1
    psRemSpoly s1 s2 1 @?= - x^3 - 2*x^2 + 1

testPseudoRemainder2 :: TestTree
testPseudoRemainder2 = testCase "Test for pseudo remainder with Spoly" $ do
    
   snd (pseudoRemainder s1 s2 0) @?=  x*y^2 + 2*y^2 - x - 1
   snd (pseudoRemainder s1 s2 1) @?= - x^3 - 2*x^2 + 1


testSpoly :: TestTree
testSpoly = testGroup "Test for Prelude of Polynomials" [testPseudoRemainder, testPseudoRemainder2]
