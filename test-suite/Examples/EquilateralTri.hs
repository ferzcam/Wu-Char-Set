module Examples.EquilateralTri (testEquilateralTri) where

import Test.Tasty
import Test.Tasty.HUnit as HU
import Polynomial.Prelude
import Polynomial.TheoremProver
import Util.Tokenizer

a, b, c, c1, b1, a1, o :: Point
a  = Point (X "u1") (X "u1")
b  = Point (X "x1") (X "u1")
c  = Point (X "x2") (X "x3")
c1 = Point (X "x5") (X "x4")
b1 = Point (X "x7") (X "x6")
a1 = Point (X "x9") (X "x8")
o  = Point (X "xx11") (X "xx10")

lac1, lc1b, lab, lb1c, lb1a, la1b, la1c :: Line
lac1 = Line a c1
lc1b = Line c1 b
lab  = Line a b
lb1c = Line b1 c
lb1a = Line b1 a
la1b = Line a1 b
la1c = Line a1 c

ang1, ang2, ang3 :: Angle
ang1 = Angle c1 a b
ang2 = Angle c a b1
ang3 = Angle a1 b c

h1, h2, h3, h4, h5, h6, h7, h8, g :: Hypothesis
h1 = SameLen lac1 lc1b
h2 = SameLen lac1 lab
h3 = SameAcAngle ang1 ang2
h4 = SameLen lb1c lb1a
h5 = SameAcAngle ang1 ang3
h6 = SameLen la1b la1c
h7 = Collinear o a1 a
h8 = Collinear o b b1
g  = Collinear o c1 c

freeVars :: [Coord]
freeVars = [X "u1"]

polys :: [Poly]
polys = generatePolynomials [h1, h2, h3, h4, h5, h6, h7, h8] g freeVars

conclusion :: Poly
hypotheses :: [Poly]
(conclusion : hypotheses) = polys

testTheorem :: TestTree
testTheorem = testCase "Test for Equilateral Triangles" $
    isZero (last (theoremProver 11 hypotheses conclusion)) @?= True

testEquilateralTri :: TestTree
testEquilateralTri = testGroup "Test for Equilateral Triangles" [testTheorem]
