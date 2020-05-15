{-#LANGUAGE DataKinds#-}

module Examples.EquiPol (testEquipol) where 

import Algebra.Ring.Polynomial
import Test.Tasty
import Test.Tasty.HUnit as HU
import qualified Data.Map.Strict as MS
import Polynomial.Prelude
import Polynomial.Wu
import Polynomial.TheoremProver
    


y1 = var 10
y2 = var 9 
y3 = var 8
y4 = var 7
y5 = var 6
y6 = var 5
y7 = var 4
y8 = var 3
y9 = var 2
y10 = var 1
y11 = var 0

h1,h2,h3,h4,h5,h6,h7,h8,g :: Polynomial' 11
h1 = 2*y1*y5 - y1^2
h2 = y5^2 + y4^2 - y1^2
h3 = (y1*y3*y5 - y1*y2*y4)*y7 - (y1*y2*y5 + y1*y3*y4)*y6
h4 = 2*y2*y7 + 2*y3*y6 - y3^2 - y2^2
h5 = (y1*y3*y5 + (y1*y2 - y1^2)*y4  )*y9  + ((y1^2 - y1*y2)*y5 + y1*y3*y4)*y8 - y1^2*y3*y5 + (y1^3 - y1^2*y2)*y4 
h6 = (2*y2 - 2*y1)*y9 + 2*y3*y8- y3^2 -y2^2 + y1^2
h7 = y8*y11 - y9*y10
h8 = y6*y11 + (y1 - y7 )*y10 - y1*y6
g= (y4 - y3)*y11 - (y5 - y2)*y10 + y3*y5 - y2*y4

testTheorem :: TestTree
testTheorem = testCase "Test for Equi Pol Theorem" $ do
    last (theoremProver [h1,h2,h3,h4,h5,h6,h7,h8] g) @?= 0

testEquipol :: TestTree
testEquipol = testGroup "Test for Equil Pol Theorem" [testTheorem]