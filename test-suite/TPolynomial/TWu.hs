{-#LANGUAGE DataKinds, NoImplicitPrelude #-}

module TPolynomial.TWu (testsWu) where 


import Test.Tasty
import Test.Tasty.HUnit as HU
import Polynomial.Wu
import Algebra.Prelude
import Polynomial.Prelude

x1 = var 7
x2 = var 6 
x3 = var 5
x4 = var 4
x5 = var 3
x6 = var 2
x7 = var 1
x8 = var 0
u1 = var 8
u2 = var 9

h1,h2,h3,h4,h5,h6,h7,h8,g,r1 :: Polynomial' Grevlex 10
h1 = 2*x1 - u1
h2 = 2*x2 - u2
h3 = 2*x3 - u1 
h4 = 2*x4 - u2
h5 = u2*x5 + u1*x6 - u1*u2
h6 = u1*x5 - u2*x6
h7 = x1^2 - x2^2 - 2*x1*x7 + 2*x2*x8
h8 = x1^2 - 2*x1*x7 - x3^2 + 2*x3*x7 - x4^2 + 2*x4*x8
g= (x5 - x7)^2 + (x6-x8)^2 - (x1-x7)^2 -x8^2 
r1 = x1*x2^2 + 2*x2^2 - x1 - 1

-- q1 = x^2 + y^2 - z^2 - 8 :: Polynomial' Grevlex 3
-- q2 = x + y + z - 5 :: Polynomial' Grevlex 3

-- r1 = 2*y^2 - 10*y - 10*z + 2*y*z + 17  :: Polynomial' Grevlex 3 
-- r2 = x + y + z - 5 :: Polynomial' Grevlex 3

-- s1,s2,r1,r2:: Polynomial'  Lex 2
-- s1 = y^2 -  x^2 -   x^3
-- s2 = y^2 +  x^2 - 1


-- r2 = y^4 + y^2 - 1

-- testCharSet :: TestTree
-- testCharSet = testCase "Test for computing char set" $ do
--     sort (charSet [q1,q2] [] 0) @?= sort [r1,r2]

testTheorem :: TestTree
testTheorem = testCase "Test for Theorem Prover" $ do
    last (theoremProver [h1,h2,h3,h4,h5,h6,h7,h8] g) @?=  0
--testReplacePoly :: TestTree

testsWu :: TestTree
testsWu = testGroup "Test for Wu module" [testTheorem]