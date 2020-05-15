 {-# LANGUAGE FlexibleContexts #-}

module Polynomial.Spolynomial.WuSP where

import Algebra.Ring.Polynomial hiding (leadingMonomial, leadingTerm)
import Polynomial.Prelude
import GHC.TypeLits
import Debug.Trace
import Polynomial.Spolynomial.Remainder



-- | Algorithm to get characteristic set from a set of polynomials.
charSetSP :: (IsMonomialOrder n Grevlex, KnownNat n) 
    => [Polynomial' n] -> [ Polynomial' n] -> Int -> [ Polynomial' n]
charSetSP [] a _ = map (simplifyPolinomial) a
charSetSP p a var
    | lenS == 0 = charSetSP p a (var+1)
    | lenS == 1 = charSetSP c (a++s) (var+1)
    | otherwise = case existOneDegPoly s var of
                    Just poly ->  charSetSP (c ++ rem poly) (a++[poly]) (var+1)
                    Nothing -> charSetSP (c++r++newS) a var

    where
        c = dropPolys p s -- p/s
        s =  filter (`varInPoly` var) p 
        lenS = length s
        rem poly =  psRemaidersSpoly (dropPolys s [poly]) poly var
        (newS, r) = analizeS' s var
        

analizeS' :: (IsMonomialOrder n Grevlex, KnownNat n) => [Polynomial' n] -> Int -> ([Polynomial' n], [Polynomial' n])
analizeS' ls@(x:y:z) var
        | classVarDeg r var > 1 =  analizeS' newls var
        | classVarDeg r var == 1 = (newls, [])
        | classVarDeg r var == 0 =  (dropPolys newls [max], [r])
        where
            (r, max) = maxNpseudoSP x y var
            newls = replacePoly ls max r 
        

maxNpseudoSP :: (IsMonomialOrder n Grevlex, KnownNat n) => Polynomial' n -> Polynomial' n -> Int -> (Polynomial' n, Polynomial' n)
maxNpseudoSP f g var
    | classVarDeg f var >= classVarDeg g var = (r, f)
    | otherwise = (r, g)
    where
        [newF, newG] = if (classVarDeg f var) > (classVarDeg g var) then [f,g] else [g,f] 
        r = psRemSpoly newF newG var