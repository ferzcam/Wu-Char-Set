{-#LANGUAGE DataKinds#-}

module Examples.Parallelogram (testParallelogram) where


import Algebra.Ring.Polynomial
import Test.Tasty
import Test.Tasty.HUnit as HU
import qualified Data.Map.Strict as MS
import Polynomial.Prelude
import Polynomial.Wu
import Polynomial.TheoremProver
import Util.Tokenizer


a = Point (X "u1") (X "u2")
b = Point (X "u3") (X "u4")
c = Point (X "u5") (X "u6")
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
h3 = Collinear b o c
h4 = Collinear a o d

conc = SameLen lao ldo

freeVars = [X "u1", X "u2", X "u3", X "u4", X "u5", X "u6"]

polys :: [Polynomial' 10]
polys@(conclusion:hypotheses) = generatePolynomials [h1, h2, h3, h4] conc freeVars

testTheorem :: TestTree
testTheorem = testCase "Test for Parallelogram Theorem" $ do
    last (theoremProver 4 hypotheses conclusion) @?= 0

testParallelogram :: TestTree
testParallelogram = testGroup "Test for Parallelogram Theorem" [testTheorem]
