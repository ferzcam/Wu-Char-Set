 {-# LANGUAGE FlexibleContexts #-}

module Polynomial.Spolynomial.Remainder where

import Algebra.Ring.Polynomial hiding (leadingMonomial, leadingTerm)
import Polynomial.Prelude
import GHC.TypeLits
import Debug.Trace


psRemaidersSpoly :: (IsMonomialOrder n Grevlex, KnownNat n) => 
        [Polynomial' n] -> Polynomial' n -> Int -> [Polynomial' n]
psRemaidersSpoly polys poly var = map (\p -> psRemSpoly p poly var) polys
            

psRemSpoly :: (IsOrder n Grevlex, KnownNat n, IsMonomialOrder n Grevlex) 
        => Polynomial' n -> Polynomial' n -> Int ->  Polynomial' n
psRemSpoly 0 g var = 0
psRemSpoly f g var 
        | f == g = 0
        | classVarDeg f var < classVarDeg g var || classVarDeg g var == 0 = f
        | otherwise = psRemSpoly (sPolynomial' f g var) g var     
-- trace ("\nVAR: " ++ show var ++ "\nF: " ++ show f ++ "\nG: " ++ show g ++ "\nREM: " ++ show (simplifyPolinomial (snd pseudo)))

sPolynomial' :: (IsMonomialOrder n Grevlex, KnownNat n) 
            => Polynomial' n -> Polynomial' n -> Int ->  Polynomial' n
sPolynomial' 0 g i = 0
sPolynomial' f g i = simplifyPolinomial ( sp)
                        where
                        h = toPolynomial (1, lcmMonomial (leadingMonomial f i) (leadingMonomial g i))
                        factorsg = chooseTermsWithVar g i
                        factorsf = chooseTermsWithVar f i
                        commonLeadf = commonMonomial factorsf  -- Obtiene el factor comun de la variable de clase del polinomio f
                        commonLeadg = commonMonomial factorsg --chooseTermsWithMon- Obtiene el factor comun de la variable de clase del polinomio g
                        sp = (h // (1, commonLeadf ) ) * (simplifyMonomial factorsg) * f - (h // (1, commonLeadg ) ) * (simplifyMonomial factorsf)* g
-- trace ("f :" ++ show f ++ "\ng :" ++ show g ++ "\n\nfactorsF:" ++ show factorsf ++  "\nSimp factorsF:" ++ show (simplifyMonomial factorsf) ++ "\nfactorsG:" ++ show factorsg ++  "\nSimp factorsG:" ++ show (simplifyMonomial factorsg) ++ "\n\nh :" ++ show h ++ "\n\nCommonLeadf :" ++ show commonLeadf ++ "\nCommonLeadG :" ++ show commonLeadg ++ "\n\n\nsp :" ++ show sp ) 


