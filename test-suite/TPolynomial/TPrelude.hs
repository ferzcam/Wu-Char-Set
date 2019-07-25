{-#LANGUAGE DataKinds#-}

module TPolynomial.TPrelude (testsPrelude) where 

import Algebra.Prelude hiding ((+), (*), (-), (^))
import Test.Tasty
import Test.Tasty.HUnit as HU
import Polynomial.Prelude
    

xl = var 0
yl = var 1
zl = var 2

xg = var 0
yg = var 1
zg = var 2

p1L = xl + yl^4 + zl^3 :: Polynomial' Lex 3
p2L = xl^2*yl - zl :: Polynomial' Lex 3
p3L = xl*yl + xl*yl*zl :: Polynomial' Lex 3
p4L = xl*yl*zl

p1Gr = xg + yg^4 + zg^3 :: Polynomial' Grevlex 3
p2Gr = xg^2*yg^3 - zg :: Polynomial' Grevlex 3
p3Gr = xg*yg + xg*yg*zg^2 :: Polynomial' Grevlex 3


-- testClassVarDeg :: TestTree
-- testClassVarDeg = testCase "Test for getting class variables" $ do
--     classVarDeg p1L @?= 1
--     classVarDeg p2L @?= 2
--     classVarDeg p3L @?= 1
--     classVarDeg p1Gr @?= 4
--     classVarDeg p2Gr @?= 1
--     classVarDeg p3Gr @?= 2

testDropPolys :: TestTree
testDropPolys = testCase "Test for dropping polys" $ do
    dropPolys [p1L,p2L,p3L] [p4L,p3L] @?= [p1L, p2L]

--testReplacePoly :: TestTree

testsPrelude :: TestTree
testsPrelude = testGroup "Test for Prelude of Polynomials" [testDropPolys]