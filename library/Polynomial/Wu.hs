{-#LANGUAGE FlexibleContexts#-}

module Polynomial.Wu where

import Algebra.Ring.Polynomial
import Data.Type.Ordinal
import Polynomial.Prelude
import Data.List
import Debug.Trace
import GHC.TypeLits
-- | This algorithm was taken from the book "Ideals, Varieties and Algorithms" 4th ed.

-- | Algorithm to get characteristic set from a set of polynomials.
charSet :: (IsMonomialOrder n Grevlex, KnownNat n) 
    => [Polynomial' n] -> [ Polynomial' n] -> Int -> [ Polynomial' n]
charSet [] a _ = map (simplifyPolinomial) a
charSet p a var
    | lenS == 0 = charSet p a (var+1)
    | lenS == 1 = charSet c (a++s) (var+1)
    | otherwise = case existOneDegPoly s var of
                    Just poly -> charSet (c ++ rem poly) (a++[poly]) (var+1)
                    Nothing -> charSet (c++r++newS) a var

    where
        c = dropPolys p s -- p/s
        s =  filter (`varInPoly` var) p 
        lenS = length s
        rem poly =  pseudoRemainders (dropPolys s [poly]) poly var
        (newS, r) = analizeS s var

-- Function that test a geometric theorem. 
-- Inputs: hip, Hipotheses; g, theorem. 
-- Output: list of the pseudo remainders of g with respect to the ascending chain
theoremProver :: (IsMonomialOrder n Grevlex, KnownNat n) 
    => [Polynomial' n] -> Polynomial' n -> [Polynomial' n]
theoremProver hip g =  trace ((show wuChain) ++ "\n\n\n\n\n\n") remWithChain wuChain g 0
-- remWithChain wuChain g 0 numHip
    where 
        wuChain =  charSet hip [] 0
        -- numHip = length hip - 1

-- Function that get the pseudoremider of a polinomial with
-- respect to a set of polynomials
remWithChain :: (IsMonomialOrder n Grevlex, KnownNat n) 
    => [Polynomial' n] -> Polynomial' n -> Int -> [ Polynomial' n]
remWithChain [e] pol var = [snd $ pseudoRemainder pol e var]
remWithChain chain pol var = [rem]++(remWithChain newChain rem (var + 1) )
--  trace ("New Chain: "++ show newChain)
    where  
        rem = snd $ pseudoRemainder pol elemChain var -- remainder
        
        elemChain = head chain
        newChain = tail chain
        

analizeS :: (IsMonomialOrder n Grevlex, KnownNat n) => [Polynomial' n] -> Int -> ([Polynomial' n], [Polynomial' n])
analizeS ls@(x:y:z) var
        | classVarDeg r var > 1 =   analizeS newls var
        | classVarDeg r var == 1 =  (newls, [])
        | classVarDeg r var == 0 =  (dropPolys newls [max], [r])
        where
            (r, max) = maxNpseudo x y var
            newls = replacePoly ls max r 
        

maxNpseudo :: (IsMonomialOrder n Grevlex, KnownNat n) => Polynomial' n -> Polynomial' n -> Int -> (Polynomial' n, Polynomial' n)
maxNpseudo f g var
    | classVarDeg f var >= classVarDeg g var = (r, f)
    | otherwise = (r, g)
    where
        r = snd $ pseudoRemainder f g var