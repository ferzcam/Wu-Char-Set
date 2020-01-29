{-#LANGUAGE DataKinds, NoImplicitPrelude #-}

module TPolynomial.TPrelude (testsPrelude) where 

import Algebra.Prelude hiding (leadingTerm)
import Test.Tasty
import Test.Tasty.HUnit as HU
import qualified Data.Map.Strict as MS
import Polynomial.Prelude
    
xl = var 0
yl = var 1
zl = var 2

xg = var 0
yg = var 1
zg = var 2


x = var 0
y = var 1 

s1,s2,r1,r2:: Polynomial'  Lex 2
s1 = y^2 -  x^2 -  x^3
s2 = y^2 +  x^2 - 1

r1 = x*y^2 + 2*y^2 - x - 1
r2 = y^4 + y^2 - 1



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

testLeadingTerms :: TestTree
testLeadingTerms = testCase "Test for leading term" $ do
    leadingTerm s1 0 @?= ((snd &&& fst) $ (head . MS.toList._terms) (-x^3))
    leadingTerm s1 1 @?= ((snd &&& fst) $ (head . MS.toList._terms) y^2)
    leadingTerm s2 0 @?= ((snd &&& fst) $ (head . MS.toList._terms) x^2)
    leadingTerm s2 1 @?= ((snd &&& fst) $ (head . MS.toList._terms) y^2)

testPseudoRemainder :: TestTree
testPseudoRemainder = testCase "Test for pseudo remainder" $ do
    snd (pseudoRemainder (x^2 + y^2 - 1) (-x*y^2 + x + 1) 0) @?= y^6 - 3*y^4 + 3*y^2 
    snd (pseudoRemainder (x^2 + y^2 - 1) (-x*y^2 + x + 1) 1) @?= - x^3 - 1
    


testsPrelude :: TestTree
testsPrelude = testGroup "Test for Prelude of Polynomials" [testDropPolys,testPseudoRemainder]