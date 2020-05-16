{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude, FlexibleContexts #-}

module Symbolic.Prelude where

import AlgebraicPrelude hiding (appendFile, fromString )
import Algebra.Ring.Polynomial hiding (leadingMonomial, leadingTerm)
import Symbolic.Expr
import Util.Coeff
import GHC.TypeLits
import qualified Data.Sized.Builtin       as S
import qualified Data.Map.Strict        as M
import System.IO (writeFile, appendFile)
import Debug.Trace
import Data.Maybe
import Data.Proxy
import Data.Singletons


type PolynomialSym n = OrderedPolynomial (Expr Integer) Grevlex n
type OrderedMonomial' n = OrderedMonomial Grevlex n


-- | Function that get the common term of a symbolic polynomial
simplifyNumSym :: (KnownNat n )
            => PolynomialSym n -> PolynomialSym n
simplifyNumSym pol =  Polynomial $ M.fromList $ map (\(mon, coef)  -> (mon,  toExpr $ M.map (`div` gcdnum) $ fromExpr coef) ) $ M.toList  $ _terms pol
                where
                  coefficients =  map M.elems $ map (fromExpr) $  map (snd) $  M.toList $ _terms pol
                  -- Here we obtain the coefficients of the polynomial
                  gcdnum =  foldr' gcd (last gcd1) (init gcd1)
                  gcd1 = map (foldr1 gcd) coefficients
                  --  Here we get the gcd of the coeficients

-- | Function that transform a list of integers into a Monomial
toMonomial :: (KnownNat n) => [Int] -> OrderedMonomial Grevlex n
toMonomial a = orderMonomial Proxy (fromList sing a)

-- | Function that get the degree of the variable of class in a symbolic polynomial
classVarDeg :: (IsOrder n Grevlex, KnownNat n, IsMonomialOrder n Grevlex)
        =>  PolynomialSym n -> Int -> Int
classVarDeg pol var = leadingMonomialDegs !! var
        where
                leadingMonomialDegs = S.toList $ getMonomial $ leadingMonomial pol var

-- | Function that generate a Monomial given three integers (variable, degree, arity)
mon :: (IsOrder n Grevlex, KnownNat n, IsMonomialOrder n Grevlex)
        => Int -> Int -> Int -> OrderedMonomial' n
mon var exp numVars = toMonomial exps
        where 
                zeros = replicate numVars 0
                exps = insertAt var exp zeros


-- |  Division of a polynomial with a monomial
(//) :: (IsMonomialOrder n Grevlex, IsOrder n Grevlex, KnownNat n) 
            =>  PolynomialSym n  -> ((Expr Integer), OrderedMonomial' n) ->  PolynomialSym n
pol // (coeff, mon) = sum $ map (toPolynomial . (`tryDiv'` (coeff, mon)) . (snd &&& fst)) terms
            where
                    terms = M.toList $ _terms pol
                               
-- | Function to get the common monomial of a symbolic polynomial
commonMonomial ::(IsMonomialOrder n Grevlex, IsOrder n Grevlex, KnownNat n) 
        =>  PolynomialSym n  ->  OrderedMonomial' n
commonMonomial pol  =  foldr' foo (last monomials) (init monomials) 
                        where
                                foo monomial acc = gcdMonomial acc monomial
                                monomials = M.keys $ _terms pol
                       
-- | Fuction that give the common term of a symbolic polynomial
commonTerm ::(IsMonomialOrder n Grevlex, IsOrder n Grevlex, KnownNat n) 
        =>  PolynomialSym n  ->  ((Expr Integer), OrderedMonomial' n)
commonTerm pol  =  (coeff ,foldr' foo (last monomials) (init monomials) )
                        where
                                foo monomial acc = gcdMonomial acc monomial
                                monomials = M.keys $ _terms pol
                                coeff = foldl1 (gcd) $ M.elems $ _terms pol

-- | Fuction that defines the division between monomials
tryDiv' :: (Integral r, KnownNat n) => (r, OrderedMonomial ord n) -> (r, OrderedMonomial ord n) -> (r, OrderedMonomial ord n)
tryDiv' (a, f) (b, g)
        | g `divs` f = (a `div` b, OrderedMonomial $ S.zipWithSame (-) (getMonomial f) (getMonomial g))
        | otherwise  = error "cannot divide."

-- | Function to get the arity of a polynomial
getArity :: PolynomialSym n -> Int
getArity = (length.S.toList.getMonomial.fst.head.M.toList._terms)

-- | Function to get the polynomials that contain an specific variable in its monomials
chooseTermsWithVar :: (IsMonomialOrder n Grevlex, IsOrder n Grevlex, KnownNat n) => PolynomialSym n -> Int -> PolynomialSym n
chooseTermsWithVar pol var
                | not $ varInPoly pol var = chooseTermsWithVar pol (var + 1)
                | otherwise = foldr' foo 0 idxs
                where
                        deg_pol = classVarDeg pol var
                        idxs = findIndices (\x -> x!!var == deg_pol) (map (S.toList . getMonomial) (M.keys $ _terms pol))
                        foo idx acc = acc + toPolynomial (snd $ auxMonom pol idx , fst $ auxMonom pol idx)
                        auxMonom poly idx = M.elemAt idx $ _terms poly

-- | Function to simplify a Monomial
simplifyMonSym ::(IsMonomialOrder n Grevlex, IsOrder n Grevlex, KnownNat n)   =>  PolynomialSym n -> PolynomialSym n
simplifyMonSym pol = pol // (1, commonMonomial pol)

-- | Function to simplify a Polynomial
simplifyPolynomial ::(IsMonomialOrder n Grevlex, IsOrder n Grevlex, KnownNat n)   =>  PolynomialSym n -> PolynomialSym n
simplifyPolynomial pol  = pol // commonTerm pol
        
-- | Function to obtain the coefficient of a given polynomial
getCoeff :: (IsMonomialOrder n Grevlex, IsOrder n Grevlex, KnownNat n)   =>  PolynomialSym n -> Int -> PolynomialSym n
getCoeff pol var = pol // (1, classVariable)
        where
                deg = classVarDeg pol var
                arity = (length . S.toList . getMonomial . fst . head . M.toList . _terms) pol
                classVariable = mon var deg arity

-- | Function to obtain the leading term of a symbolic polynomial
leadingTerm :: (IsMonomialOrder n Grevlex, IsOrder n Grevlex, KnownNat n) => PolynomialSym n -> Int -> ((Expr Integer), OrderedMonomial' n)
leadingTerm pol var = (snd &&& fst) $ fromJust $ M.lookupLE chosenTerm (_terms pol)
        where
                chosenTerm = toMonomial (foldr1 foo polToList)
                polToList = map (S.toList . getMonomial) (M.keys $ _terms pol)
                foo monomCoeffs acc = if monomCoeffs!!var > acc!!var then monomCoeffs else acc

-- | Function to obtain the leading monomial of a symbolic polynomial
leadingMonomial :: (IsMonomialOrder n Grevlex, IsOrder n Grevlex, KnownNat n) => PolynomialSym n -> Int -> OrderedMonomial Grevlex n
leadingMonomial pol var = snd $ leadingTerm pol var

-- | Function to obtain the leading coefficient of a symbolic polynomial
leadingCoeff :: (IsMonomialOrder n Grevlex, IsOrder n Grevlex, KnownNat n) => PolynomialSym n -> Int -> (Expr Integer)
leadingCoeff pol var = fst $ leadingTerm pol var

-- | Function to know if certain variable is in the polynomial
varInPoly :: (IsOrder n Grevlex, KnownNat n,  IsMonomialOrder n Grevlex)
        =>  PolynomialSym n -> Int -> Bool
varInPoly pol var
    | classVarDeg pol var == 0 = False
    | otherwise = True

-- | Given two sets polys1 and polys2. Returns polys1/polys2                        
dropPolys :: Eq poly => [poly] -> [poly] -> [poly]                       
dropPolys polys1 polys2 = [p | p <- polys1 , p `notElem` polys2]

-- | Given a set polys of polynomials containing p and a polynomial q. Returns the set P with q instead p. 
replacePoly :: Eq poly => [poly] -> poly -> poly -> [poly]
replacePoly polys p q = q : dropPolys polys [p]

-- | Given 
existOneDegPoly :: [PolynomialSym n] -> Int -> Maybe (PolynomialSym n)
existOneDegPoly polys var = find isOneDeg polys
        where
            isOneDeg poly = (((\x -> x==1).last.sort)) (((map ((!! var) . S.toList . getMonomial . fst)) . M.toList . _terms) poly) 


-- | Function that evaluate certain symbolic value in the polynomial
evaluatePoly :: (KnownNat n)
        => PolynomialSym n ->  (String, Integer) -> PolynomialSym n
evaluatePoly poly ("", _) = poly
evaluatePoly poly (str, val) =  Polynomial $ M.fromList  $ evaluateCoef polyList
        where
                polyList = M.toList $ _terms poly
                evaluateCoef = map (\(mon, coeff) ->  (mon, evaluate coeff str val))

-- | Function that given a polynomial and a list [(string), integer] return a polynomial with the symbolic variables evaluated
evaluatePolyList :: (KnownNat n)
        => PolynomialSym n ->  [(String,Integer)] ->  PolynomialSym n
evaluatePolyList poly [] = poly
evaluatePolyList poly (v:vs) = evaluatePolyList (evaluatePoly poly v) vs 


-- Function to insert an element in a given position
insertAt :: Int -> Int-> [Int] -> [Int] 
insertAt z y xs = as ++ (y : tail bs)
                        where (as,bs) = splitAt z xs

