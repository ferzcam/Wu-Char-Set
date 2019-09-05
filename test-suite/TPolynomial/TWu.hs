{-#LANGUAGE DataKinds, NoImplicitPrelude #-}

module TPolynomial.TWu (testsWu) where 


import Test.Tasty
import Test.Tasty.HUnit as HU
import Polynomial.Wu
import Algebra.Prelude
import Polynomial.Prelude

x = var 0
y = var 1 

s1,s2,r1,r2:: Polynomial'  Lex 2
s1 = y^2 +  x^2 +  x^3
s2 = y^2 +  x^2 - 1

r1 = x*y^2 + 2*y^2 - x - 1
r2 = y^4 + y^2 - 1

testCharSet :: TestTree
testCharSet = testCase "Test for computing char set" $ do
    charSet [s1,s2] [] 0 @?= [r1,r2]

--testReplacePoly :: TestTree

testsWu :: TestTree
testsWu = testGroup "Test for Wu module" [testCharSet]