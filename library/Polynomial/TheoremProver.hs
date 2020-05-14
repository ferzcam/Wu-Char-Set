{-#LANGUAGE FlexibleContexts#-}

module Polynomial.TheoremProver where

import Algebra.Ring.Polynomial
import Data.Type.Ordinal
import Polynomial.Prelude
import Polynomial.Wu
import Data.List
import Debug.Trace
import GHC.TypeLits
-- | This algorithm was taken from the book "Ideals, Varieties and Algorithms" 4th ed.


-- Function that test a geometric theorem. 
-- Inputs: hip, Hipotheses Polynomials; g, Theorem Polynomial. 
-- Output: list of the pseudo remainders of g with respect to the ascending chain
theoremProver :: (IsMonomialOrder n Grevlex, KnownNat n) 
    => [Polynomial' n] -> Polynomial' n -> [Polynomial' n]
theoremProver hip g = remWithChain wuChain g 0
    where 
        wuChain =  charSet hip [] 0
      
-- Function that get the pseudoremider of a polinomial with
-- respect to a set of polynomials
remWithChain :: (IsMonomialOrder n Grevlex, KnownNat n) 
    => [Polynomial' n] -> Polynomial' n -> Int -> [ Polynomial' n]
remWithChain [] _ _ = []
remWithChain chain pol var = [rem]++(remWithChain newChain rem (var + 1))
    where  
        rem = snd $ pseudoRemainder pol elemChain var -- remainder
        elemChain = head chain
        newChain = tail chain