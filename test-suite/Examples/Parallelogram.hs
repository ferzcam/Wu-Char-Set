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

lac = Line a c
lbd = Line b d
lab = Line a b 
lcd = Line c d 
lao = Line a o
lco = Line c o

lbo = Line b o
ldo = Line d o

h1 = Parallel lac lbd
h2 = Parallel lab lcd
h3 = Colinear b o c
h4 = Colinear a o d

conc = SameLen lao ldo
--conc = SameLen lco lbo

polys :: [Polynomial' 4]
polys@(conclusion:hypotheses) = generatePolynomials [h1, h2, h3, h4] conc

testTheorem :: TestTree
testTheorem = testCase "Test for Parallelogram Theorem" $ do
    last (theoremProver hypotheses conclusion) @?= 0

testParallelogram :: TestTree
testParallelogram = testGroup "Test for Parallelogram Theorem" [testTheorem]