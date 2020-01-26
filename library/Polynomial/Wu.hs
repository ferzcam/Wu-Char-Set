module Polynomial.Wu where


import Data.Type.Ordinal
import Polynomial.Prelude
import Algebra.Prelude hiding ((+), (++), map)
import Debug.Trace
-- | This algorithm was taken from the book "Ideals, Varieties and Algorithms" 4th ed.

-- | Algorithm to get characteristic set from a set of polynomials.
charSet :: (IsMonomialOrder n ord, KnownNat n) 
    => [Polynomial' ord n] -> [ Polynomial' ord n] -> Int -> [ Polynomial' ord n]
charSet [] a _ = map (`simplifyMonomial` (-1) ) a
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


analizeS :: (IsMonomialOrder n ord, KnownNat n) => [Polynomial' ord n] -> Int -> ([Polynomial' ord n], [Polynomial' ord n])
analizeS ls@(x:y:z) var
        | classVarDeg r var > 1 =  trace "EEEEEEE analyze >1" analizeS newls var
        | classVarDeg r var == 1 = trace "EEEEEEE analize ==1" (newls, [])
        | classVarDeg r var == 0 = trace "EEEEEEE analize - 0" (dropPolys newls [max], [r])
        where
            (r, max) = maxNpseudo x y var
            newls = replacePoly ls max r 
        

maxNpseudo :: (IsMonomialOrder n ord, KnownNat n) => Polynomial' ord n -> Polynomial' ord n -> Int -> (Polynomial' ord n, Polynomial' ord n)
maxNpseudo f g var
    | classVarDeg f var >= classVarDeg g var = (r, f)
    | otherwise = (r, g)
    where
        r = snd $ pseudoRemainder f g var