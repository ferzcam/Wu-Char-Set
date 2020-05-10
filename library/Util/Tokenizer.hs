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

data Hypothesis =   Collinear Point Point Point 
                    | Parallel Line Line 
                    | Perpendicular Line Line 
                    | InCircle Point Circle
                    | SameLen Line Line
                    | SameAcAngle Angle Angle
                    | MidPoint Point Point Point
                deriving (Show)

type Conclusion = Hypothesis

generatePolynomials :: (KnownNat n) => [Hypothesis] -> Conclusion -> [Polynomial' n] 
generatePolynomials hypotheses conclusion = map (flip geomToAlg variables) statements
    where 
        points = nub $ concatMap flatten hypotheses
        variables = generateVariables points conclusion
        statements = conclusion:hypotheses


generateVariables :: (KnownNat n) => [Point] -> Conclusion -> [(Coord, Polynomial' n)]
generateVariables points conclusion =  zip finalVariables monicPolys
    --trace ("VARIABLES: " ++ show (zip finalVariables monicPolys))
    where
        pointsConclusion = nub $ (concatMap (\(Point c1 c2) -> [c1, c2])) (flatten conclusion)
        variablesConclusion = filter (flip elem variablesX) pointsConclusion
        variablesNotConclusion = variablesX \\ variablesConclusion
        variablesX = nub $ filter isX allVariables
        variablesU = nub $ filter isU allVariables
        allVariables =sort $ (concatMap (\(Point c1 c2) -> [c1, c2]) points)
        finalVariables = variablesConclusion ++ variablesNotConclusion ++ (variablesU)
        initialArrays = (toLists.identity) (length (variablesX))
        monomials = map toMonomial initialArrays
        monicPolys = (map (toPolynomial . (1,)) monomials) ++ ((map ((^2).fromInteger) [0..]))

-- trace ("VARIABLES: " ++ show (zip finalVariables monicPolys)) zip finalVariables 

isX :: Coord -> Bool
isX (X _) = True
isX _ = False

isU :: Coord -> Bool
isU (U _) = True
isU _ = False


takePairs :: [a] -> [[a]]
takePairs [] = []
takePairs [a] = []
takePairs (x:y:s) = [[x,y]] ++ takePairs s


getCoords :: Point -> [(Coord, Polynomial' n)] -> (Polynomial' n, Polynomial' n)
getCoords (Point x y) dict = (fromJust $ lookup x dict, fromJust $ lookup y dict)

geomToAlg :: (KnownNat n) => Hypothesis -> [(Coord, Polynomial' n)] -> Polynomial' n
geomToAlg (Collinear a b c) dict =  
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
                                in  (y2 - y1)^2 + (x2 - x1)^2 - (y4 - y3)^2 - (x4 - x3)^2
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
                                in  ((y1 - y2)*(x3 - x2) - (y3 - y2)*(x1 - x2))*((x4 - x5)*(x6 - x5) + (y4 - y5)*(y6 - y5)) - ((y4 - y5)*(x6 - x5) - (y6 - y5)*(x4 - x5))*((x1 - x2)*(x3 - x2) + (y1 - y2)*(y3 - y2)) 
geomToAlg (MidPoint first middle last) dict =
                                let (x1,y1) = getCoords first dict
                                    (xm,ym) = getCoords middle dict
                                    (x2,y2) = getCoords last dict
                                in  x1+x2-2*xm + y1+y2 - 2*ym


uncurry3 :: (a -> b -> c -> d) -> (a,b,c) -> d
uncurry3 f (a,b,c) = f a b c



instance Flattening Hypothesis where
    flatten (Collinear p1 p2 p3) = nub [p1, p2, p3] 
    flatten (Parallel (Line p1 p2) (Line p3 p4)) = nub [p1, p2, p3, p4] 
    flatten (Perpendicular (Line p1 p2) (Line p3 p4)) = nub [p1, p2, p3, p4] 
    flatten (InCircle p (Circle c r)) = nub [p, c, r]
    flatten (SameLen (Line p1 p2) (Line p3 p4)) = nub [p1, p2, p3, p4]
    flatten (MidPoint first middle last) = nub [first, middle, last]
    flatten (SameAcAngle (Angle p1 p2 p3) (Angle p4 p5 p6)) = nub [p1, p2, p3, p4, p5, p6]



instance Ord Coord where
    compare (X _) (U _) = LT
    compare (U _) (X _) = GT
    compare (X x1) (X x2) = compare x1 x2
    compare (U u1) (U u2) = compare u1 u2
  

class Flattening a where
    flatten :: a -> [Point]