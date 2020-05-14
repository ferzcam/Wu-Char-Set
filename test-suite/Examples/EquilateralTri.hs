{-#LANGUAGE DataKinds#-}

module Examples.EquilateralTri (testEquilateralTri) where 


import Algebra.Ring.Polynomial
import Test.Tasty
import Test.Tasty.HUnit as HU
import qualified Data.Map.Strict as MS
import Polynomial.Prelude
import Polynomial.Wu
import Polynomial.TheoremProver
import Util.Tokenizer    
import Data.List

a = Point (U "u1") (U "u1")
b = Point (X "x1") (U "u1")
c = Point (X "x2") (X "x3")
c1 = Point (X "x5") (X "x4")
b1 = Point (X "x7") (X "x6")
a1 = Point (X "x9") (X "x8")
o = Point (X "xx11") (X "xx10")



lac1 = Line a c1
lc1b = Line c1 b
loa1 = Line o a1
lob1 = Line o b1
la1a = Line a1 a
lb1b = Line b1 b
loa = Line o a
lob = Line o b
lab = Line a b
lb1c = Line b1 c
lb1a = Line b1 a
la1b = Line a1 b
la1c = Line a1 c

ang1 = Angle c1 a b
ang2 = Angle c a b1
ang3 = Angle a1 b c

h1 = SameLen lac1 lc1b 
h2 = SameLen lac1 lab
h3 = SameAcAngle ang1 ang2 
h4 = SameLen lb1c lb1a
h5 = SameAcAngle ang1 ang3
h6 = SameLen la1b la1c
h7 = Collinear o a1 a 
h8 = Collinear o b b1
 

g = Collinear o c1 c


polys :: [Polynomial' 11]
polys@(conclusion:hypotheses) = generatePolynomials [h1, h2, h3, h4, h5, h6, h7, h8] g


testTheorem :: TestTree
testTheorem = testCase "Test for Equilateral Triangles" $ do
    last (theoremProver hypotheses conclusion) @?= 0

testEquilateralTri :: TestTree
testEquilateralTri = testGroup "Test for Equilateral Triangles" [testTheorem]