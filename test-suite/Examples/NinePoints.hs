module Examples.NinePoints (testNinePoints) where

import Test.Tasty
import Test.Tasty.HUnit as HU
import Polynomial.Prelude
import Polynomial.TheoremProver
import Util.Tokenizer

a, b, c, d, e, f, m, n :: Point
a = Point (X "u1") (X "u1")
b = Point (X "x1") (X "u1")
c = Point (X "x2") (X "x3")
d = Point (X "x4") (X "x5")
e = Point (X "x6") (X "x7")
f = Point (X "x2") (X "u1")
m = Point (X "x8") (X "u1")
n = Point (X "x9") (X "xx10")

lad, lcb, leb, lca, lnf, lne, lnd, lnm :: Line
lad = Line a d
lcb = Line c b
leb = Line e b
lca = Line c a
lnf = Line n f
lne = Line n e
lnd = Line n d
lnm = Line n m

h1, h2, h3, h4, h5, h6, h7, g :: Hypothesis
h1 = Collinear d b c
h2 = Perpendicular lad lcb
h3 = Collinear e a c
h4 = Perpendicular leb lca
h5 = MidPoint a m b
h6 = SameLen lnf lne
h7 = SameLen lnf lnd
g  = SameLen lnf lnm

freeVars :: [Coord]
freeVars = [X "u1"]

polys :: [Poly]
polys = generatePolynomials [h1, h2, h3, h4, h5, h6, h7] g freeVars

conclusion :: Poly
hypotheses :: [Poly]
(conclusion : hypotheses) = polys

testTheorem :: TestTree
testTheorem = testCase "Test for NinePoints Theorem" $
    isZero (last (theoremProver 10 hypotheses conclusion)) @?= True

testNinePoints :: TestTree
testNinePoints = testGroup "Test for NinePoints Theorem" [testTheorem]
