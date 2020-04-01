{-#LANGUAGE DataKinds#-}

module Examples.Pascal (testPascal) where 


import Algebra.Ring.Polynomial
import Test.Tasty
import Test.Tasty.HUnit as HU
import qualified Data.Map.Strict as MS
import Polynomial.Prelude
import Polynomial.Wu
import Util.Tokenizer      

a = Point (U "u1") (U "u1")
b = Point (U "u3") (U "u1")
c = Point (U "u5") (U "u6")
d = Point (X "x1") (U "u7")
e = Point (X "x2") (U "u8")
f = Point (X "x3") (U "u9")
o = Point (X "x4") (X "x5")
p = Point (X "x6") (U "u1")
q = Point (X "x7") (X "x8")
s = Point (X "x9") (X "x10")

loa = Line o a
loc = Line o c
lob = Line o b
lod = Line o d
lof = Line o f
loe = Line o e


h1 = SameLen loa loc
h2 = SameLen loa lob
h3 = SameLen loa lod
h4 = SameLen loa lof
h5 = SameLen loa loe
h6 = Colinear p d f
h7 = Colinear q f e
h8 = Colinear q b c
h9 = Colinear s e a
h10 = Colinear s c d
g = Colinear s q p

hypsGeom = [h1, h2, h3, h4, h5, h6, h7, h8, h9, h10]

polys :: [Polynomial' 10]
polys@(conclusion:hypotheses) = generatePolynomials hypsGeom g

testTheorem :: TestTree
testTheorem = testCase "Test for Pascal Theorem" $ do
    last (theoremProver hypotheses conclusion) @?= 0

testPascal :: TestTree
testPascal = testGroup "Test for Pascal Theorem" [testTheorem]

