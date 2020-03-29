{-#LANGUAGE DataKinds, NoImplicitPrelude #-}

module Examples.Parallelogram (testParallelogram) where 

import Algebra.Prelude hiding (leadingTerm)
import Test.Tasty
import Test.Tasty.HUnit as HU
import qualified Data.Map.Strict as MS
import Polynomial.Prelude
import Polynomial.Wu
import Util.Tokenizer    


a = Point "A"
b = Point "B"
c = Point "C"
d = Point "D"
o = Point "O"


lab = Line a b
ldc = Line d c
lda = Line d a 
lbc = Line b c 
lao = Line a o
lco = Line c o

h1 = Parallel lab ldc
h2 = Parallel lda lbc
h3 = Colinear d o b
h4 = Colinear a c o

conc = SameLen lao lco

structures = [P a, P b, P c, P d, P o, L lab, L ldc, L lda, L lbc, L lao, L lco]

polys :: [Polynomial' 7]
polys@(conclusion:hypotheses) = generatePolynomials structures [h1, h2, h3, h4] conc


testTheorem :: TestTree
testTheorem = testCase "Test for Parallelogram Theorem" $ do
    last (theoremProver hypotheses conclusion) @?= 0

testParallelogram :: TestTree
testParallelogram = testGroup "Test for Parallelogram Theorem" [testTheorem]