module Polynomial.Prelude where

import Algebra.Prelude hiding (Rational, map, findIndex, drop)
import Data.Maybe (fromJust)
import qualified Data.Map.Strict as MS
import Data.Sized.Builtin as S (toList)
import Data.List hiding (drop)

type Polynomial' ord n = OrderedPolynomial Rational ord n

-- | Returns the position of the class variable. Monomial ordering must be defined correctly.
classVarDeg :: Polynomial' ord n -> Int
classVarDeg poly = let maybeDegree = (find (/=0)) . S.toList . getMonomial . fst . head . (MS.toDescList) . _terms
                    in case maybeDegree poly of
                        Just a -> a
                        Nothing -> 0


-- | Given two sets polys1 and polys2. Returns polys1/polys2                        
dropPolys :: Eq poly => [poly] -> [poly] -> [poly]                       
dropPolys polys1 polys2 = [p | p <- polys1 , not (elem p polys2)]

-- | Given a set polys of polynomials containing p and a polynomial q. Returns the set P with q instead p. 
replacePoly :: Eq poly => [poly] -> poly -> poly -> [poly]
replacePoly polys p q = q:(dropPolys polys [p])


-- | Given a set of poly of polynomials and a integer number var, which correspond to the variable that we are interested. Return a Maybe data type, with the polynomial that have degree one in the variable var and nothing if it not found any polynomial with degree one in the variable var

existOneDegPoly :: [OrderedPolynomial k ord n] -> Int -> Bool
existOneDegPoly polys var 
    |   length polys == 1 = elem (1) $ getDegs (head polys)
    |   otherwise = if (elem (1) $ getDegs (head polys)) then (True) else (existOneDegPoly (tail polys) var)
                    where
                        getDegs  =   (map (!!1)) .  (map (S.toList . getMonomial)) .  MS.keys . _terms 

-- | Leading Term, it pick a  polynomial and return the leading term considering Lexicographic order. 
leadingTerm' :: (IsOrder n ord) => OrderedPolynomial k ord n -> (k, OrderedMonomial ord n)
leadingTerm' poly =   (snd &&& fst)  $ fromJust $ MS.lookupLE monomial terms
            where
                monomial =  last $ MS.keys $ terms
                terms = _terms poly

leadingMonomial' ::  (IsOrder n ord) => OrderedPolynomial k ord n -> OrderedMonomial ord n
leadingMonomial' = snd . leadingTerm' 
            
leadingCoeff' ::  (IsOrder n ord) => OrderedPolynomial k ord n -> k
leadingCoeff' = fst . leadingTerm' 

            