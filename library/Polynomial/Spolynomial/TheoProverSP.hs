{-#LANGUAGE FlexibleContexts#-}

module Polynomial.Spolynomial.TheoProverSP where

import Algebra.Ring.Polynomial
import Data.Type.Ordinal
import Polynomial.Prelude
import Polynomial.Spolynomial.WuSP
import Polynomial.Spolynomial.Remainder
import Data.List
import Debug.Trace
import GHC.TypeLits
-- | This algorithm was taken from the book "Ideals, Varieties and Algorithms" 4th ed.


-- Function that test a geometric theorem. 
-- Inputs: hip, Hipotheses Polynomials; g, Theorem Polynomial. 
-- Output: list of the pseudo remainders of g with respect to the ascending chain
theoremProverSP :: (IsMonomialOrder n Grevlex, KnownNat n) 
    => [Polynomial' n] -> Polynomial' n -> [Polynomial' n]
theoremProverSP hip g = remWithChainSP wuChain g 0
    where 
        wuChain =  charSetSP hip [] 0
      
-- Function that get the pseudoremider of a polinomial with
-- respect to a set of polynomials
remWithChainSP :: (IsMonomialOrder n Grevlex, KnownNat n) 
    => [Polynomial' n] -> Polynomial' n -> Int -> [ Polynomial' n]
remWithChainSP [] _ _ = []
remWithChainSP chain pol var =  [rem]++(remWithChainSP newChain rem (var + 1))
    where  
        rem = psRemSpoly pol elemChain var -- remainder
        elemChain = head chain
        newChain = tail chain