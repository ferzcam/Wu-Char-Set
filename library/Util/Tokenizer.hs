{-# LANGUAGE DataKinds, TupleSections #-}

module Util.Tokenizer where
import Prelude hiding ((+), (-))
import Algebra.Prelude hiding (map, (*), (^), (++), (\\))
--hiding (Rational, findIndex, drop, leadingCoeff, leadingTerm, leadingMonomial, (/), gcd, lcm, fromRational, toRational)
import qualified Data.Map.Strict as MS
import Polynomial.Prelude
import Data.Matrix hiding (flatten, trace)
import Data.List
import Debug.Trace
import Data.Maybe

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
                    | SameLen Line Line

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


geomToAlg :: (KnownNat n) => Hypothesis -> [(Point, [Polynomial' Grevlex n])] -> Polynomial' Grevlex n
geomToAlg (Colinear a b c) var =  
                                let pa = fromJust $ lookup a var
                                    pb = fromJust $ lookup b var
                                    pc = fromJust $ lookup c var
                                in  (pb!!1 -  pa!!1)*(pc!!0 - pa!!0) - (pc!!1 - pa!!1)*(pb!!0 - pa!!0)  
geomToAlg (Parallel l1 l2) var =  
                                let pts = map (\point -> fromJust $ lookup point var ) $ flatten (Parallel l1 l2)
                                    p1 = pts!!0
                                    p2 = pts!!1
                                    p3 = pts!!2
                                    p4 = pts!!3
                                in  (p2!!1 - p1!!1)*(p4!!0 - p3!!0) - (p4!!1 - p3!!1)*(p2!!0 - p1!!0) 
geomToAlg (SameLen (Line pt1 pt2) (Line pt3 pt4)) var =  
                                let pts = map (\point -> fromJust $ lookup point var ) $ [pt1, pt2, pt3, pt4]
                                    p1 = trace ("pts:" ++ show pts) pts!!0
                                    p2 = pts!!1
                                    p3 = pts!!2
                                    p4 = pts!!3
                                in  (p2!!1 - p1!!1)^2 + (p2!!0 - p1!!0)^2 - (p4!!0 - p3!!0)^2 - (p4!!1 - p3!!1)^2  




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
    flatten (SameLen (Line p1 p2) (Line p3 p4)) = nub [p1, p2, p3, p4] 




class Flattening a where
    flatten :: a -> [Point]