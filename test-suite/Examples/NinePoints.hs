{-#LANGUAGE DataKinds#-}

module Examples.NinePoints (testNinePoints) where 


import Algebra.Ring.Polynomial
import Test.Tasty
import Test.Tasty.HUnit as HU
import qualified Data.Map.Strict as MS
import Polynomial.Prelude
import Polynomial.Wu
import Util.Tokenizer    


d = IndepPoint "D"
e = IndepPoint "E"
f = IndepPoint "F"
a = DepPoint "A"
b = DepPoint "B"
c = DepPoint "C"
m = DepPoint "M"
n = DepPoint "N"



lad = Line a d
lcb = Line c b
leb = Line e b
lca = Line c a
lnf = Line n f
lne = Line n e
lnd = Line n d
lnm = Line n m

h1 = Colinear d b c 
h2 = Perpendicular lad lcb
h3 = Colinear e a c
h4 = Perpendicular leb lca
h5 = MidPoint a m b
h6 = SameLen lnf lne
h7 = SameLen lnf lnd

g = SameLen lnf lnm

structures = [P a, P b, P c, P d, P e, P f, P m, P n, L lad, L lcb, L leb, L lca, L lnf, L lne, L lnd, L lnm]

polys :: [Polynomial' 10]
polys@(conclusion:hypotheses) = generatePolynomials structures [h1, h2, h3, h4, h5, h6, h7] g


testTheorem :: TestTree
testTheorem = testCase "Test for NinePoints Theorem" $ do
    last (theoremProver hypotheses conclusion) @?= 0

testNinePoints :: TestTree
testNinePoints = testGroup "Test for NinePoints Theorem" [testTheorem]