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
                    Just poly ->  charSet (c ++ rem poly) (a++[poly]) (var+1)
                    --trace ("\nVAR: " ++ show var ++ "\nPOLYONEDEG: " ++ show poly)
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
theoremProver hip g = remWithChain wuChain g 0
    --trace ("\nWUCHAIN: " ++ (show wuChain) ++ "\n\n\n" ++ "G: "++ show g ++ "\n\n\n" ++ "ARITY: " ++ show ((getArity $ head hip)-1) ++ "\n") 
    where 
        wuChain =  charSet hip [] 0
      
-- Function that get the pseudoremider of a polinomial with
-- respect to a set of polynomials
remWithChain :: (IsMonomialOrder n Grevlex, KnownNat n) 
    => [Polynomial' n] -> Polynomial' n -> Int -> [ Polynomial' n]
<<<<<<< HEAD
remWithChain [e] pol var = [snd $ pseudoRemainder pol e var]
remWithChain chain pol var = trace ("\n\nVar:" ++ show var ++ "\n\nElemChain:" ++ show elemChain ++ "\n\nPol :" ++ show pol) [rem]++(remWithChain newChain rem (var + 1) )
--  trace ("New Chain: "++ show newChain)
=======
remWithChain [] _ _ = []
remWithChain chain pol var = [rem]++(remWithChain newChain rem (var + 1))
    --trace ("\nVAR: " ++ show var ++"\n" ++ "POL: " ++ show pol ++ "\n" ++ "ELEMCHAIN: " ++ show elemChain ++"\nREM: " ++ show rem ++ "\n") 
>>>>>>> 007921bd7cb5639da4bc6327b225f73c643fb17a
    where  
        rem = snd $ pseudoRemainder pol elemChain var -- remainder
        elemChain = head chain
        newChain = tail chain
        

analizeS :: (IsMonomialOrder n Grevlex, KnownNat n) => [Polynomial' n] -> Int -> ([Polynomial' n], [Polynomial' n])
analizeS ls@(x:y:z) var
        | classVarDeg r var > 1 =  analizeS newls var
        | classVarDeg r var == 1 = (newls, [])
        | classVarDeg r var == 0 =  (dropPolys newls [max], [r])
        --trace ("\nVAR: " ++ show var ++ "\nX: " ++ show x ++ "\nY: " ++ show y)
        where
            (r, max) = maxNpseudo x y var
            newls = replacePoly ls max r 
        

maxNpseudo :: (IsMonomialOrder n Grevlex, KnownNat n) => Polynomial' n -> Polynomial' n -> Int -> (Polynomial' n, Polynomial' n)
maxNpseudo f g var
    | classVarDeg f var >= classVarDeg g var = (r, f)
    | otherwise = (r, g)
    where
        [newF, newG] = if (classVarDeg f var) > (classVarDeg g var) then [f,g] else [g,f] 
        r = snd $ pseudoRemainder newF newG var