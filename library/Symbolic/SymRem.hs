{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude, FlexibleContexts #-}

module Symbolic.SymRem where

import AlgebraicPrelude
import Algebra.Ring.Polynomial hiding (leadingMonomial, leadingTerm)
import Symbolic.Prelude
import Symbolic.Expr
import Util.Coeff
import GHC.TypeLits
import qualified Data.Sized.Builtin       as S
import qualified Data.Map.Strict        as M
import System.IO (writeFile, appendFile)
import Debug.Trace
import Data.Singletons
import System.Directory
-- References
-- [1] See book Cox, David A, Little, John, Oshea, Donal .Ideals, Varieties, and Algorithms.

-- | Function that implement the s-polynomial for symbolic polynomials. See page 84-85 of [1].
sPolynomialSym :: (IsMonomialOrder n Grevlex, KnownNat n) 
            => PolynomialSym n -> PolynomialSym n -> Int ->  PolynomialSym n
sPolynomialSym 0 g i = 0
sPolynomialSym f g i = simplifyMonSym ( sp)
                        where
                        h = toPolynomial (1, lcmMonomial (leadingMonomial f i) (leadingMonomial g i))
                        factorsg = chooseTermsWithVar g i
                        factorsf = chooseTermsWithVar f i
                        commonLeadf = commonMonomial factorsf  
                        commonLeadg = commonMonomial factorsg 
                        sp = (h // (1, commonLeadf ) ) * (simplifyMonSym factorsg) * f - (h // (1, commonLeadg ) ) * (simplifyMonSym factorsf)* g

-- | Given two polynomials f and g, it obtain the psuedoRemainder of the euclidean division of f with respect to g
psRemSym :: (IsOrder n Grevlex, KnownNat n, IsMonomialOrder n Grevlex) 
        => PolynomialSym n -> PolynomialSym n -> Int ->  PolynomialSym n
psRemSym 0 g var = 0
psRemSym f g var 
        | f == g = 0
        | classVarDeg f var < classVarDeg g var || classVarDeg g var == 0 = f
        | otherwise = psRemSym (sPolynomialSym f g var) g var     

-- | Given a list of polynomials polys and a polynomial poly, it returns the psuedo remaider of each element of polys with
-- respect to poly
remaindersSym :: (IsMonomialOrder n Grevlex, KnownNat n) => 
        [PolynomialSym n] -> PolynomialSym n -> Int -> [PolynomialSym n]
remaindersSym polys poly var = map (\p -> psRemSym p poly var) polys
