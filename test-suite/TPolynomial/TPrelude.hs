{-#LANGUAGE DataKinds#-}

module TPolynomial.TPrelude (testsPrelude) where 


import Algebra.Ring.Polynomial hiding (leadingTerm)
import Test.Tasty
import Test.Tasty.HUnit as HU
import qualified Data.Map.Strict as MS
import Polynomial.Prelude
import Control.Arrow

x = var 0
y = var 1

s1,s2,r1,r2 :: Polynomial' 2
s1 = y^2 -  x^2 -  x^3
s2 = y^2 +  x^2 - 1

r1 = x*y^2 + 2*y^2 - x - 1
r2 = y^4 + y^2 - 1


-- testClassVarDeg :: TestTree
-- testClassVarDeg = testCase "Test for getting class variables" $ do
--     classVarDeg p1L @?= 1
--     classVarDeg p2L @?= 2
--     classVarDeg p3L @?= 1
--     classVarDeg p1Gr @?= 4
--     classVarDeg p2Gr @?= 1
--     classVarDeg p3Gr @?= 2


testLeadingTerms :: TestTree
testLeadingTerms = testCase "Test for leading term" $ do
    leadingTerm s1 0 @?= ((snd &&& fst) $ (head . MS.toList._terms) (-x^3))
    leadingTerm s1 1 @?= ((snd &&& fst) $ (head . MS.toList._terms) (y^2))
    leadingTerm s2 0 @?= ((snd &&& fst) $ (head . MS.toList._terms) (x^2))
    leadingTerm s2 1 @?= ((snd &&& fst) $ (head . MS.toList._terms) (y^2))

testPseudoRemainder :: TestTree
testPseudoRemainder = testCase "Test for pseudo remainder" $ do
    --snd (pseudoRemainder (x^2 + y^2 - 1) (-x*y^2 + x + 1) 0) @?= y^4 - 3*y^2 + 3 
    --snd (pseudoRemainder (x^2 + y^2 - 1) (-x*y^2 + x + 1) 1) @?= - x^3 - 1
    snd (pseudoRemainder (x^2 +118*x - 599) (x-5) 0) @?= 1
    


testsPrelude :: TestTree
testsPrelude = testGroup "Test for Prelude of Polynomials" [testPseudoRemainder]