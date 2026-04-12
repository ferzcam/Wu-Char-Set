{-#LANGUAGE DataKinds#-}

module Examples.Pascal (testPascal) where


import Algebra.Ring.Polynomial
import Test.Tasty
import Test.Tasty.HUnit as HU
import qualified Data.Map.Strict as MS
import Polynomial.Prelude
import Polynomial.Wu
import Polynomial.TheoremProver
import Util.Tokenizer

a = Point (X "u1") (X "u1")
b = Point (X "u3") (X "u1")
c = Point (X "u5") (X "u6")
d = Point (X "x1") (X "u7")
e = Point (X "x2") (X "u8")
f = Point (X "x3") (X "u9")
o = Point (X "x4") (X "x5")
p = Point (X "x6") (X "u1")
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
h6 = Collinear p d f
h7 = Collinear q f e
h8 = Collinear q b c
h9 = Collinear s e a
h10 = Collinear s c d
g = Collinear s q p

hypsGeom = [h1, h2, h3, h4, h5, h6, h7, h8, h9, h10]

freeVars = [X "u1", X "u3", X "u5", X "u6", X "u7", X "u8", X "u9"]

polys :: [Polynomial' 17]
polys@(conclusion:hypotheses) = generatePolynomials hypsGeom g freeVars

testTheorem :: TestTree
testTheorem = testCase "Test for Pascal Theorem" $ do
    last (theoremProver 10 hypotheses conclusion) @?= 0

testPascal :: TestTree
testPascal = testGroup "Test for Pascal Theorem" [testTheorem]
