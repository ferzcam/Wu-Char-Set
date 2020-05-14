 {-# LANGUAGE FlexibleContexts #-}

module Polynomial.Spoly where

import Algebra.Ring.Polynomial hiding (leadingMonomial, leadingTerm)
import Polynomial.Prelude
import GHC.TypeLits
import Debug.Trace


psRemaidersSpoly :: (IsMonomialOrder n Grevlex, KnownNat n) => 
        [Polynomial' n] -> Polynomial' n -> Int -> [Polynomial' n]
psRemaidersSpoly polys poly var = map (\p -> psRemSpoly p poly var) polys
            

psRemSpoly :: (IsOrder n Grevlex, KnownNat n, IsMonomialOrder n Grevlex) 
        => Polynomial' n -> Polynomial' n -> Int ->  Polynomial' n
psRemSpoly f g var 
        | f == g = 0
        | classVarDeg f var < classVarDeg g var || classVarDeg g var == 0 = f
        | otherwise = psRemSpoly (sPolynomial' f g var) g var
     
-- trace ("\nVAR: " ++ show var ++ "\nF: " ++ show f ++ "\nG: " ++ show g ++ "\nREM: " ++ show (simplifyPolinomial (snd pseudo)))

sPolynomial' :: (IsMonomialOrder n Grevlex, KnownNat n) 
            => Polynomial' n -> Polynomial' n -> Int ->  Polynomial' n
sPolynomial' f g i = trace ("f :" ++ show f ++ "\ng :" ++ show g ++ "\n\nfactorsF:" ++ show factorsf ++  "\nSimp factorsF:" ++ show (simplifyMonomial factorsf) ++ "\nfactorsG:" ++ show factorsg ++  "\nSimp factorsG:" ++ show (simplifyMonomial factorsg) ++ "\n\nh :" ++ show h ++ "\n\nCommonLeadf :" ++ show commonLeadf ++ "\nCommonLeadG :" ++ show commonLeadg ++ "\n\n\nsp :" ++ show sp ) simplifyPolinomial ( sp)
                        where
                        h = toPolynomial (1, lcmMonomial (leadingMonomial f i) (leadingMonomial g i))
                        factorsg = chooseTermsWithVar g i
                        factorsf = chooseTermsWithVar f i
                        commonLeadf = commonMonomial factorsf  -- Obtiene el factor comun de la variable de clase del polinomio f
                        commonLeadg = commonMonomial factorsg --chooseTermsWithMon- Obtiene el factor comun de la variable de clase del polinomio g
                        sp = (h // (1, commonLeadf ) ) * (simplifyMonomial factorsg) * f - (h // (1, commonLeadg ) ) * (simplifyMonomial factorsf)* g


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