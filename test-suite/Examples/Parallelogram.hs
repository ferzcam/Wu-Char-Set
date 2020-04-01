{-#LANGUAGE DataKinds#-}

module Examples.Parallelogram (testParallelogram) where 


import Algebra.Ring.Polynomial
import Test.Tasty
import Test.Tasty.HUnit as HU
import qualified Data.Map.Strict as MS
import Polynomial.Prelude
import Polynomial.Wu
import Util.Tokenizer    


a = Point (U "u1") (U "u2")
b = Point (U "u3") (U "u4")
c = Point (U "u5") (U "u6")
d = Point (X "x1") (X "x2")
o = Point (X "x3") (X "x4")

-- a = Point (Z) (Z)
-- b = Point (U "u1") (Z)
-- c = Point (U "u2") (U "u3")
-- d = Point (X "x1") (X "x2")
-- o = Point (X "x3") (X "x4")


lab = Line a b
lcd = Line c d
lad = Line a d 
lbc = Line b c 
lao = Line a o
lco = Line c o

lbo = Line b o
ldo = Line d o

h1 = Parallel lab lcd
h2 = Parallel lad lbc
h3 = Colinear b o d
h4 = Colinear a o c
h5 = SameLen lab lcd
h6 = SameLen lbc lad

--conc = SameLen lao lco
conc = SameLen lbo ldo
points = [P a, P b, P c, P d, P o]

polys :: [Polynomial' 10]
polys@(conclusion:hypotheses) = generatePolynomials points [h1, h2, h3, h4] conc


testTheorem :: TestTree
testTheorem = testCase "Test for Parallelogram Theorem" $ do
    last (theoremProver hypotheses conclusion) @?= 0

testParallelogram :: TestTree
testParallelogram = testGroup "Test for Parallelogram Theorem" [testTheorem]