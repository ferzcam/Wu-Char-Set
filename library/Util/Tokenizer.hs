{-# LANGUAGE DataKinds, TupleSections, TypeFamilies#-}




module Util.Tokenizer where
import qualified Data.Map.Strict as MS
import Polynomial.Prelude
import Data.Matrix hiding (flatten, trace, zero)
import Data.List
import Debug.Trace
import Data.Maybe
import GHC.TypeLits
import Algebra.Ring.Polynomial

data Coord = X String | U String deriving (Eq, Show)

data Point = Point Coord Coord deriving (Eq, Show)
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
                    | MidPoint Point Point Point
                deriving (Show)

type Conclusion = Hypothesis

generatePolynomials :: (KnownNat n) => [Struct] -> [Hypothesis] -> Conclusion -> [Polynomial' n] 
generatePolynomials structs hypotheses conclusion = trace ("VARIABLES: " ++ show variables ++ "\n\n") map (flip geomToAlg variables) statements

    where 
        points = map getPoint (filter (isPoint) structs)
        arity = 2 * length points
        variables = generateVariables arity points conclusion
        statements = conclusion:hypotheses


generateVariables :: (KnownNat n) => Int -> [Point] -> Conclusion -> [(Coord, Polynomial' n)]
generateVariables arity points conclusion = trace ("VARIABLES: " ++ show variablesFinal) zip variablesFinal monicPolys
    where
        pointsConclusion = flatten conclusion
        pointsHypotheses = points \\ pointsConclusion
        variablesConclusion = concatMap (\(Point c1 c2) -> [c1, c2]) pointsConclusion
        variablesHyp = sort $ (concatMap (\(Point c1 c2) -> [c1, c2]) pointsHypotheses)
        variablesFinal = variablesConclusion ++ variablesHyp 
        initialArrays = identity (arity)
        nonZero = toLists initialArrays -- ^Remove the three last variables to make them zero
        --ready = nonZero ++ (replicate 3 (replicate (arity-3) 0)) -- ^The three last variables are fixed to zero to make that (w.l.o.g) the first two points are (0,0) and (x_1,0) 
        monomials = map toMonomial nonZero
--        variables = (map (Polynomial . MS.fromList . return . (,1) ) monomials) ++ [zero, zero, zero]
        monicPolys = (map (toPolynomial . (1,)) monomials)

isPoint :: Struct -> Bool
isPoint (P p) = True
isPoint _ = False

isX :: Coord -> Bool
isX (X _) = True
isX _ = False

takePairs :: [a] -> [[a]]
takePairs [] = []
takePairs [a] = []
takePairs (x:y:s) = [[x,y]] ++ takePairs s


getCoords :: Point -> [(Coord, Polynomial' n)] -> (Polynomial' n, Polynomial' n)
getCoords (Point x y) dict = (fromJust $ lookup x dict, fromJust $ lookup y dict)

geomToAlg :: (KnownNat n) => Hypothesis -> [(Coord, Polynomial' n)] -> Polynomial' n
geomToAlg (Colinear a b c) dict =  
                                let (x1,y1) = getCoords a dict
                                    (x2,y2) = getCoords b dict
                                    (x3,y3) = getCoords c dict
                                in (y2-y1)*(x3-x2) - (y3-y2)*(x2-x1) 
geomToAlg (Parallel l1 l2) dict =  
                                let [p1, p2, p3, p4] = flatten (Parallel l1 l2)
                                    (x1,y1) = getCoords p1 dict
                                    (x2,y2) = getCoords p2 dict
                                    (x3,y3) = getCoords p3 dict
                                    (x4,y4) = getCoords p4 dict
                                in  (y2 - y1)*(x4 - x3) - (y4 - y3)*(x2 - x1) 
geomToAlg (SameLen (Line pt1 pt2) (Line pt3 pt4)) dict =  
                                let 
                                    (x1,y1) = getCoords pt1 dict
                                    (x2,y2) = getCoords pt2 dict
                                    (x3,y3) = getCoords pt3 dict
                                    (x4,y4) = getCoords pt4 dict
                                in  (y2 - y1)^2 + (x2 - x1)^2 - (y4 - y3)^2 + (x4 - x3)^2
geomToAlg (Perpendicular (Line pt1 pt2) (Line pt3 pt4)) dict =  
                                let (x1,y1) = getCoords pt1 dict
                                    (x2,y2) = getCoords pt2 dict
                                    (x3,y3) = getCoords pt3 dict
                                    (x4,y4) = getCoords pt4 dict
                                in  (y2 - y1)*(y4 - y3) + (x4 - x3)*(x2 - x1)
geomToAlg (InCircle (pt1) (Circle pt2 pt3)) dict = 
                                let (x1,y1) = getCoords pt1 dict
                                    (x2,y2) = getCoords pt2 dict
                                    (x3,y3) = getCoords pt3 dict
                                in (y3 - y2)^2 + (x3 - x2)^2 - (y2 - y1)^2 + (x2 - x1)^2
geomToAlg (SameAcAngle (Angle pt1 pt2 pt3) (Angle pt4  pt5 pt6)) dict =  
                                let (x1,y1) = getCoords pt1 dict
                                    (x2,y2) = getCoords pt2 dict
                                    (x3,y3) = getCoords pt3 dict
                                    (x4,y4) = getCoords pt4 dict
                                    (x5,y5) = getCoords pt5 dict
                                    (x6,y6) = getCoords pt6 dict
                                in  ((y3 - y2)*(x1 - x2) - (y1 - y2)*(x3 - x2))*((x4 - x5)*(x6 - x5) + (y4 - y5)*(y6 - y5)) - ((y6 - y5)*(x4 - x5) - (y4 - y5)*(x6 - x5))*((x1 - x2)*(x3 - x2) + (y1 - y2)*(y3 - y2)) 
geomToAlg (MidPoint first middle last) dict =
                                let (x1,y1) = getCoords first dict
                                    (xm,ym) = getCoords middle dict
                                    (x2,y2) = getCoords last dict
                                in  x1+x2-2*xm + y1+y2 - 2*ym

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



instance Ord Coord where
    compare (X _) (U _) = LT
    compare (U _) (X _) = GT
    compare (X x1) (X x2) = compare x1 x2
    compare (U u1) (U u2) = compare u1 u2

class Flattening a where
    flatten :: a -> [Point]