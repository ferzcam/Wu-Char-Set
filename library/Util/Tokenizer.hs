{-# LANGUAGE DataKinds, TupleSections #-}

module Util.Tokenizer where

import Algebra.Prelude hiding (map, (*), (++), (\\))
--hiding (Rational, findIndex, drop, leadingCoeff, leadingTerm, leadingMonomial, (/), gcd, lcm, fromRational, toRational)
import qualified Data.Map.Strict as MS
import Polynomial.Prelude
import Data.Matrix hiding (flatten, trace)
import Data.List
import Debug.Trace

newtype Point = Point String deriving (Eq, Show)
data Circle = Circle (Point, Point) Point deriving (Show) 
data Line = Line Point Point deriving (Show)

data Struct =   P {getPoint :: Point}
                | C Circle 
                | L Line
            deriving (Show)

data Hypothesis =   Colinear Point Point Point 
                    | Parallel Line Line 
                    | Perpendicular Line Line 
                    | Intersect Line Line
                    | InCircle Point Circle

type Conclusion = Hypothesis

generatePolynomials :: (KnownNat n) => [Struct] -> [Hypothesis] -> Conclusion -> [Polynomial' Grevlex n] 
generatePolynomials structs hypotheses conclusion = map (flip geomToAlg variables) statements

    where 
        points = map getPoint (filter (isPoint) structs)
        arity = 2 * length points
        variables = generateVariables arity points conclusion
        statements = hypotheses ++ [conclusion]


generateVariables :: (KnownNat n) => Int -> [Point] -> Conclusion -> [(Point, [Polynomial' Grevlex n])]
generateVariables arity points conclusion = zip orderedPoints (takePairs variables)
    where
        pointsConclusion = flatten conclusion
        orderedPoints = pointsConclusion ++ (points\\pointsConclusion)
        initialArrays = identity arity
        monomials = map toMonomial (toLists initialArrays)
            
        variables = map (toPolynomial . (1,)) monomials

isPoint :: Struct -> Bool
isPoint (P p) = True
isPoint _ = False

takePairs :: [a] -> [[a]]
takePairs [] = []
takePairs [a] = []
takePairs (x:y:s) = [[x,y]] ++ takePairs s


geomToAlg :: Hypothesis -> [(Point, [Polynomial' Grevlex n])] -> Polynomial' Grevlex n
geomToAlg _ = head . snd . head




uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c

instance Flattening Struct where
    flatten (P point) = [point] 
    flatten (C (Circle (h,k) r)) = [h,k,r]
    flatten (L (Line p1 p2)) = [p1,p2]

instance Flattening Hypothesis where
    flatten (Colinear p1 p2 p3) = nub [p1, p2, p3] 
    flatten (Parallel (Line p1 p2) (Line p3 p4)) = nub [p1, p2, p3, p4] 
    flatten (Perpendicular (Line p1 p2) (Line p3 p4)) = nub [p1, p2, p3, p4] 
    flatten (Intersect (Line p1 p2) (Line p3 p4)) = nub [p1, p2, p3, p4] 
    flatten (InCircle p (Circle (h,k) r)) = nub [p, h, k, r]




class Flattening a where
    flatten :: a -> [Point]