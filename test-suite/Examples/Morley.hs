{-#LANGUAGE DataKinds, NoImplicitPrelude #-}

module Examples.Morley (testMorley) where 

import Algebra.Prelude hiding (leadingTerm)
import Test.Tasty
import Test.Tasty.HUnit as HU
import qualified Data.Map.Strict as MS
import Polynomial.Prelude
import Polynomial.Wu
    


y1 = var 9
y2 = var 8 
y3 = var 7
y4 = var 6
y5 = var 5
y6 = var 4
y7 = var 3
y8 = var 2
y9 = var 1
y10 = var 0


h1, h2, h3, h4, h5, h6, h7, g :: Polynomial' 10
h1 = (y3^3 + ((-3)*y2^2 + 6*y1*y2 - 3*y1^2 )*y3 )*y5 + ((3*y1 - 3*y2)*y3^2 + y2^3 - 3*y1*y2^2 + 3*y1^2*y2 -y1^3)*y4 - y1*y3^3 + (3*y1*y2^2 - 6*y1^2*y2 + 3*y1^3)*y3
h2 = (y3^3 - 3*y2^2*y3)*y5 + (y2^3 - 3*y2*y3^2)*y4
h3 = y6^2 - 3 
h4 = (((y3^2 + y2^2 - y1*y2)*y5 - y1*y3*y4)*y6 + y1*y3*y5 + (y3^2 +y2^2 - y1*y2)*y4)*y8  + ((y1*y3*y5 + (y3^2 + y2^2 - y1*y2)*y4)*y6 - (y3^2 + y2^2 - y1*y2)*y5 + y1*y3*y4)*y7 - ((y3^2 + y2^2 - y1*y2)*y5^2 + (y3^2 + y2^2 - y1*y2)*y4^2)*y6 - y1*y3*y5^2 - y1*y3*y4^2  
h5 = (y1*y3*y5 - y1*y2*y4)*y8 + (y1*y2*y5 + y1*y3*y4)*y7
h6 = (y1*y3*y5 + (y1^2 - y1*y2)*y4 - y1^2*y3)*y10 + ((y1*y2 - y1^2)*y5 + y1*y3*y4 - y1^2*y2 + y1^3)*y9 - y1^2*y3*y5 + (y1^2*y2 - y1^3)*y4 + y1^3 * y3 
h7 = ((2*y4*y5 - y1*y4)*y8 + (y4^2 + y1*y5 - y5^2)*y7 - y4*y5^2 - y4^3)*y10 + ((y4^2 + y1*y5 - y5^2)*y8 + (y1*y4 - 2*y4*y5)*y7 + y5^3 - y1*y5^2 + y4^2*y5 -y1*y4^2)*y9 - (y4^3 + y4*y5^2)*y8 + (y5^3 - y1*y5^2 + y4^2*y5 - y1*y4^2)*y7 + y1*y4*y5^2 + y1*y4^3
g = (y6*y8 - y7 -y2*y6 + y3)*y10 + (y8 + y6*y7 - y3*y6 - y2)*y9 - (y3 + y2*y6 )*y8 + (y2 - y3*y6 )*y7 + (y3^2 + y2^2 )*y6


testTheorem :: TestTree
testTheorem = testCase "Test for Morley Theorem" $ do
    last (theoremProver [h1, h2, h3, h4, h5, h6, h7] g) @?=  0

testMorley :: TestTree
testMorley = testGroup "Test for Morley Theorem" [testTheorem]