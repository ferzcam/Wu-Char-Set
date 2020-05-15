{-#LANGUAGE DataKinds#-}

module Examples.EquilateralTri (testEquilateralTri) where 


import Algebra.Ring.Polynomial
import Test.Tasty
import Test.Tasty.HUnit as HU
import qualified Data.Map.Strict as MS
import Polynomial.Prelude
import Polynomial.Wu
import Polynomial.TheoremProver
import Polynomial.Spolynomial.WuSP
import Polynomial.Spolynomial.TheoProverSP
import Util.Traslator    
import Data.List

a = Point (U "u1") (U "u1")
b = Point (U "u2") (U "u1")
c = Point (U "u3") (U "u4")
c1 = Point (X "x2") (X "x1")
a1 = Point (X "x4") (X "x3")
b1 = Point (X "x6") (X "x5")
o = Point (X "x8") (X "x7")



lab1 = Line a b1
lcb1 = Line c b1 
lac1 = Line a c1
lbc1 = Line b c1 
lba1 = Line b a1
lca1 = Line c a1

ang1 = Angle b a c1 
ang2 = Angle c b a1 
ang3 = Angle a c b1 

h1 = SameLen lac1 lbc1 
h2 = SameAcAngle ang1 ang2  
h3 = SameLen lba1 lca1  
h4 = SameAcAngle ang1 ang3 
h5 = SameLen lab1 lcb1  
h6 = Collinear o b b1
h7 = Collinear o a a1 


g = Collinear o c c1


polys :: [Polynomial' 11]
polys@(conclusion:hypotheses) = generatePolynomials [h1, h2, h3, h4, h5, h6, h7] g


testTheorem :: TestTree
testTheorem = testCase "Test for Equilateral Triangles" $ do
    last (theoremProver hypotheses conclusion) @?= 0

testTheoremSP :: TestTree
testTheoremSP = testCase "Test for Equilateral Triangles SP" $ do
    last (theoremProverSP hypotheses conclusion) @?= 0


testEquilateralTri :: TestTree
testEquilateralTri = testGroup "Test for Equilateral Triangles" [testTheoremSP, testTheorem]