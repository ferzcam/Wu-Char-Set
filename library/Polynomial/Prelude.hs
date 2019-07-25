module Polynomial.Prelude where

import Algebra.Prelude hiding (Rational, map, findIndex, drop)
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