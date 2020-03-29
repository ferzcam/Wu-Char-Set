{-#LANGUAGE DataKinds, NoImplicitPrelude #-}

module Examples.Apolonius (testApolonius) where 

import Algebra.Prelude hiding (leadingTerm)
import Test.Tasty
import Test.Tasty.HUnit as HU
import qualified Data.Map.Strict as MS
import Polynomial.Prelude
import Polynomial.Wu
    


x1 = var 7
x2 = var 6 
x3 = var 5
x4 = var 4
x5 = var 3
x6 = var 2
x7 = var 1
x8 = var 0
u1 = var 8
u2 = var 9

h1,h2,h3,h4,h5,h6,h7,h8,g :: Polynomial' 10
h1 = 2*x1 - u1
h2 = 2*x2 - u2
h3 = 2*x3 - u1 
h4 = 2*x4 - u2
h5 = u2*x5 + u1*x6 - u1*u2
h6 = u1*x5 - u2*x6
h7 = x1^2 - x2^2 - 2*x1*x7 + 2*x2*x8
h8 = x1^2 - 2*x1*x7 - x3^2 + 2*x3*x7 - x4^2 + 2*x4*x8
g= (x5 - x7)^2 + (x6-x8)^2 - (x1-x7)^2 -x8^2 

testTheorem :: TestTree
testTheorem = testCase "Test for Apolonius Theorem" $ do
    last (theoremProver [h1,h2,h3,h4,h5,h6,h7,h8] g) @?= 0

testApolonius :: TestTree
testApolonius = testGroup "Test for Apolonius Theorem" [testTheorem]