module Examples.Parallelogram (testParallelogram) where

import Test.Tasty
import Test.Tasty.HUnit as HU
import Polynomial.Prelude
import Polynomial.TheoremProver
import Util.Tokenizer

a, b, c, d, o :: Point
a = Point (X "u1") (X "u2")
b = Point (X "u3") (X "u4")
c = Point (X "u5") (X "u6")
d = Point (X "x1") (X "x2")
o = Point (X "x3") (X "x4")

lac, lbd, lab, lcd, lao, ldo :: Line
lac = Line a c
lbd = Line b d
lab = Line a b
lcd = Line c d
lao = Line a o
ldo = Line d o

h1, h2, h3, h4 :: Hypothesis
h1 = Parallel lac lbd
h2 = Parallel lab lcd
h3 = Collinear b o c
h4 = Collinear a o d

conc :: Hypothesis
conc = SameLen lao ldo

freeVars :: [Coord]
freeVars = [X "u1", X "u2", X "u3", X "u4", X "u5", X "u6"]

polys :: [Poly]
polys = generatePolynomials [h1, h2, h3, h4] conc freeVars

conclusion :: Poly
hypotheses :: [Poly]
(conclusion : hypotheses) = polys

testTheorem :: TestTree
testTheorem = testCase "Test for Parallelogram Theorem" $
    isZero (last (theoremProver 4 hypotheses conclusion)) @?= True

testParallelogram :: TestTree
testParallelogram = testGroup "Test for Parallelogram Theorem" [testTheorem]
