{-# LANGUAGE DataKinds, TupleSections #-}

module Util.Tokenizer where
import Prelude hiding ((+), (-))
import Algebra.Prelude hiding (map, (*), (^), (++), (\\))
import qualified Data.Map.Strict as MS
import Polynomial.Prelude
import Data.Matrix hiding (flatten, trace)
import Data.List
import Debug.Trace
import Data.Maybe

data Point = Point String  deriving (Eq, Show)
data Circle = Circle Point Point deriving (Show) 
data Line = Line Point Point deriving (Show)
data Angle = Angle Point Point Point deriving (Show)

data Struct =   P {getPoint :: Point}
                | C Circle 
                | L Line
                | A Angle
            deriving (Show)

data Hypothesis =   Colinear Point Point Point 
                    | Parallel Line Line 
                    | Perpendicular Line Line 
                    | InCircle Point Circle
                    | SameLen Line Line
                    | SameAcAngle Angle Angle

type Conclusion = Hypothesis

generatePolynomials :: (KnownNat n) => [Struct] -> [Hypothesis] -> Conclusion -> [Polynomial' n] 
generatePolynomials structs hypotheses conclusion = map (flip geomToAlg variables) statements

    where 
        points = map getPoint (filter (isPoint) structs)
        arity = 2 * length points
        variables = generateVariables arity points conclusion
        statements = conclusion:hypotheses


generateVariables :: (KnownNat n) => Int -> [Point] -> Conclusion -> [(Point, [Polynomial' n])]
generateVariables arity points conclusion = zip orderedPoints (takePairs variables)
    where
        pointsConclusion = flatten conclusion
        orderedPoints = pointsConclusion ++ (points\\pointsConclusion)
        initialArrays = identity (arity-3)
        nonZero = toLists initialArrays -- ^Remove the three last variables to make them zero
        ready = nonZero ++ (replicate 3 (replicate (arity-3) 0)) -- ^The three last variables are fixed to zero to make that (w.l.o.g) the first two points are (0,0) and (x_1,0) 
        monomials = map toMonomial ready
        variables = map (toPolynomial . (1,)) monomials

isPoint :: Struct -> Bool
isPoint (P p) = True
isPoint _ = False

takePairs :: [a] -> [[a]]
takePairs [] = []
takePairs [a] = []
takePairs (x:y:s) = [[x,y]] ++ takePairs s


geomToAlg :: (KnownNat n) => Hypothesis -> [(Point, [Polynomial' n])] -> Polynomial' n
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
                                let dist12 = distance pt1 pt2 var
                                    dist34 = distance pt3 pt4 var
                                in  dist12 - dist34
geomToAlg (Perpendicular (Line pt1 pt2) (Line pt3 pt4)) var =  
                                let pts = map (\point -> fromJust $ lookup point var ) $ [pt1, pt2, pt3, pt4]
                                    p1 = pts!!0
                                    p2 = pts!!1
                                    p3 = pts!!2
                                    p4 = pts!!3
                                in  (p2!!1 - p1!!1)*(p4!!1 - p3!!1) + (p4!!0 - p3!!0)*(p2!!0 - p1!!0)
geomToAlg (InCircle (pt1) (Circle pt2 pt3)) var = 
                                let dist23 = distance pt2 pt3 var
                                    dist21 = distance pt2 pt1 var
                                in dist23 - dist21
geomToAlg (SameAcAngle (Angle pt1 pt2 pt3) (Angle pt4  pt5 pt6)) var =  
                                let pts = map (\point -> fromJust $ lookup point var ) $ [pt1, pt2, pt3, pt4, pt5, pt6]
                                    p1 = trace("pts :" ++ show pts) pts!!0
                                    p2 = pts!!1
                                    p3 = pts!!2
                                    p4 = pts!!3
                                    p5 = pts!!4
                                    p6 = pts!!5
                                in  ((p3!!1 - p2!!1)*(p1!!0 - p2!!0) - (p1!!1 - p2!!1)*(p3!!0 - p2!!0)  )*((p4!!0 - p5!!0)*(p6!!0 - p5!!0)  + (p4!!1 - p5!!1)*(p6!!1 - p5!!1)  )  -  ((p6!!1 - p5!!1)*(p4!!0 - p5!!0) - (p4!!1 - p5!!1)*(p6!!0 - p5!!0))* ((p1!!0 - p2!!0)*(p3!!0 - p2!!0)  + (p1!!1 - p2!!1)*(p3!!1 - p2!!1)  ) 
 

distance :: (KnownNat n) => Point -> Point -> [(Point, [Polynomial' n])] -> Polynomial' n
distance pt1 pt2 var = (p2!!1 - p1!!1)^2 + (p2!!0 - p1!!0)^2
                where 
                    pts = map (\point -> fromJust $ lookup point var ) $ [pt1, pt2]
                    p1 = pts!!0
                    p2 = pts!!1


uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c

instance Flattening Struct where
    flatten (P point) = [point] 
    flatten (C (Circle c r)) = [c,r]
    flatten (L (Line p1 p2)) = [p1,p2]

instance Flattening Hypothesis where
    flatten (Colinear p1 p2 p3) = nub [p1, p2, p3] 
    flatten (Parallel (Line p1 p2) (Line p3 p4)) = nub [p1, p2, p3, p4] 
    flatten (Perpendicular (Line p1 p2) (Line p3 p4)) = nub [p1, p2, p3, p4] 
    flatten (InCircle p (Circle c r)) = nub [p, c, r]
    flatten (SameLen (Line p1 p2) (Line p3 p4)) = nub [p1, p2, p3, p4] 




class Flattening a where
    flatten :: a -> [Point]