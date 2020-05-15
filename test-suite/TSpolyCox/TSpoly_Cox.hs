{-#LANGUAGE DataKinds#-}

module TSpolyCox.TSpoly_Cox (testSC) where 


import Algebra.Ring.Polynomial hiding (leadingTerm)
import Test.Tasty
import Test.Tasty.HUnit as HU
import qualified Data.Map.Strict as MS
import Polynomial.Prelude
import Polynomial.RandomPol
import Polynomial.Spolynomial.Remainder
import Control.Arrow


genPol :: Int -> Polynomial' 3
genPol g = randPol 3 4 2 g


-- randPol arity size deg seed 
testFunction :: Int ->  Bool
testFunction gen = ( pseu == spol)
    where
        p1 = genPol gen 
        p2 = genPol (gen+1) 
        pseu = snd $ pseudoRemainder p1 p2 0
        spol = psRemSpoly p1 p2 0



testSP_Cox :: TestTree
testSP_Cox = testCase "Test for Psuedo Remainder with SP and Cox Algorithm" $ do
    testFunction 2 @?= True
    testFunction 3 @?= True
    testFunction 4 @?= True

testSC :: TestTree
testSC = testGroup "Test for Psuedo Remainder with SP and Cox Algorithm" [testSC]
