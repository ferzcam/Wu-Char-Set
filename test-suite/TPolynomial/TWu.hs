{-#LANGUAGE DataKinds, NoImplicitPrelude #-}

module TPolynomial.TWu (testsWu) where 


import Test.Tasty
import Test.Tasty.HUnit as HU
import Polynomial.Wu
import Algebra.Prelude
import Polynomial.Prelude


------------------------------------EMPIEZA APOLONIO-------------------------------------
-- x1 = var 7
-- x2 = var 6 
-- x3 = var 5
-- x4 = var 4
-- x5 = var 3
-- x6 = var 2
-- x7 = var 1
-- x8 = var 0
-- u1 = var 8
-- u2 = var 9

-- h1,h2,h3,h4,h5,h6,h7,h8,g,r1 :: Polynomial' Grevlex 10
-- h1 = 2*x1 - u1
-- h2 = 2*x2 - u2
-- h3 = 2*x3 - u1 
-- h4 = 2*x4 - u2
-- h5 = u2*x5 + u1*x6 - u1*u2
-- h6 = u1*x5 - u2*x6
-- h7 = x1^2 - x2^2 - 2*x1*x7 + 2*x2*x8
-- h8 = x1^2 - 2*x1*x7 - x3^2 + 2*x3*x7 - x4^2 + 2*x4*x8
-- g= (x5 - x7)^2 + (x6-x8)^2 - (x1-x7)^2 -x8^2 
------------------------------------TERMINA APOLONIO-------------------------------------


---------------------------------EMPIEZA PASCAL THEOREM----------------------------------
x1 = var 9
x2 = var 8 
x3 = var 7
x4 = var 6
x5 = var 5
x6 = var 4
x7 = var 3
x8 = var 2
x9 =  var 1
x10 = var 0
u1 = var 10
u2 = var 11
u3 = var 12
u4 = var 13
u5 = var 14
u6 = var 15

h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,g:: Polynomial' Grevlex 16
--h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,g
h1 = 2*u2*x2 + 2*u3*x1 - u3^2 - u2^2
h2 = 2*u1*x2 - u1^2 
h3 = x3^2 - 2*x2*x3 - 2*u4*x1 + u4^2
h4 = x4^2 - 2*x2*x4 - 2*u5*x1 + u5^2 
h5 = x5^2 - 2*x2*x5 - 2*u6*x1 + u6^2 
h6 = (u5 - u4)*x6 + u4*x4-u5*x3
h7 = (u6 - u5)*x8 - (x5 - x4)*x7 + u5*x5-u6*x4
h8 = u3*x8 - (u2 - u1)*x7 - u1*u3
h9 = u6*x10-x5*x9
h10 = (u4 -u3)*x10 - (x3 - u2)*x9 + u3*x3 - u2*u4 
g = x7*x10 - (x8-x6)*x9 - x6*x7
---------------------------------Termina-------------------------------------------------


---------------------------------Morleys  THEOREM----------------------------------
-- y1 = var 9
-- y2 = var 8 
-- y3 = var 7
-- y4 = var 6
-- y5 = var 5
-- y6 = var 4
-- y7 = var 3
-- y8 = var 2
-- y9 = var 1
-- y10 = var 0


-- hh1, hh2, hh3, hh4, hh5, hh6, hh7, g :: Polynomial' Grevlex 10
-- --h1,h2,h3,h4,h5,h6,h7,h8,h9,h10,g
-- hh1 = (y3^3 + ((-1)*3*y2^3 + 6*y1*y2 - 3*y1^2 )*y3 )*y5 + ((3*y1 - 3*y2)*y3^2 + y2^3 - 3*y2*y2^2 + 3*y1^2*y2 -y1^3)*y4 - y1*y3^3 + (3*y1*y2^2 - 6*y1^2*y2 + 3*y1^3)*y3
-- hh2 = (y3^3 - 3*y2^2*y3)*y5 + (y2^3 - 2*y2*y3^2)*y4
-- hh3 = y6^2 - 3 
-- hh4 = (((y3^2 + y2^2 - y1*y2)*y5 - y1*y3*y4)*y6 + y1*y3*y5 + (y3^2 +y2^2 - y1*y2)*y4)*y8  + ((y1*y3*y5 + (y3^2 + y2^2 - y1*y2)*y4)*y6 - (y3^2 + y2^2 - y1*y2)*y5 + y1*y2*y4)*y7 - ((y3^2 + y2^2 - y1*y2)*y5^2 + (y3^2 + y2^2 - y1*y2)*y4^2)*y6 - y1*y3*y5^2 - y1*y3*y4^2  
-- hh5 = (y1*y3*y5 - y1*y2*y4)*y8 + (y1*y2*y5 + y1*y3*y4)*7
-- hh6 = (y1*y3*y5 + (y1^2 - y1*y2)*y4 - y1^2*y3)*y10 + ((y1*y2 - y1^2)*y5 + y1*y2*y4 - y1^2*y2 + y1^3)*y9 - y1^2*y2*y5 + (y1^2*y2 - y1^3)*y4 + y1^3 * y3 
-- hh7 = ((2*y4*y5 - y1*y4)*y8 + (y4^2 + y1*y5 - y5^2)*y7 - y4*y5^2 - y4^3)*y10 + ((y4^2 + y1*y5 - y5^2)*y8 + (y1*y4 - 2*y4*y5)*y7 + y5^3 - y1*y5^2 + y4^2*y5 -y1*y4^2)*y9 - (y4^3 + y4*y5^2)*y8 + (y5^3 - y1*y5^2 + y4^2*y5 - y1*y4^2)*y7 + y1*y4*y5^2 + y1*y4^3
-- g = (y6*y8 - y7 -y2*y6 + y3)*y10 + (y8 + y8*y7 - y3*y6 - y2)*y9 - (y3 + y2*y6 )*y8 + (y2 - y3*y6 )*y7 + (y3^2 + y2^2 )*y6
---------------------------------Termina Morley Theorem---------------------------------------------------






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
    -- Pascal
    last (theoremProver [h1,h2,h3,h4,h5,h6,h7,h8,h9,h10] g) @?=  0
    -- MOrley
    --last (theoremProver [hh1, hh2, hh3, hh4, hh5, hh6, hh7] g) @?=  0
    --last (theoremProver [h1,h2,h3,h4,h5,h6,h7,h8,h9,h10] g) @?=  0
--testReplacePoly :: TestTree

testsWu :: TestTree
testsWu = testGroup "Test for Wu module" [testTheorem]