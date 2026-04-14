{-# LANGUAGE TupleSections #-}

module Util.Tokenizer where

import Data.List (nub, sortBy, (\\))
import Data.Maybe (fromJust)
import Data.Ord (comparing)
import Polynomial.Poly
import Polynomial.Prelude (Poly)

data Coord = X String | Const Integer deriving (Eq, Show)

data Point = Point Coord Coord deriving (Eq, Show)
data Circle = Circle Point Point deriving (Show)
data Line = Line Point Point deriving (Show)
data Angle = Angle Point Point Point deriving (Show)

data Hypothesis
  = Collinear Point Point Point
  | Parallel Line Line
  | Perpendicular Line Line
  | InCircle Point Circle
  | SameLen Line Line
  | SameAcAngle Angle Angle
  | MidPoint Point Point Point
  deriving (Show)

type Conclusion = Hypothesis

-- | Build the list of polynomial statements for a problem. The conclusion
-- is always the first element of the returned list.
generatePolynomials :: [Hypothesis] -> Conclusion -> [Coord] -> [Poly]
generatePolynomials hypotheses conclusion freeVars =
    map (`geomToAlg` variables) statements
  where
    points = nub $ concatMap flatten hypotheses
    variables = generateVariables points conclusion freeVars
    statements = conclusion : hypotheses

-- | Assign each point-coordinate variable a monic 'Poly' of the appropriate
-- arity.
--
-- Ordering (matches the Java reference @PolyBasic@ / @CharSet@ convention).
-- Java's elimination direction, set in @DrawProcess.java:1872-1890@, inserts
-- new polys into @polylist@ so that it is sorted /ascending/ by @lv@, then
-- @CharSet.charset@ processes the head first — i.e. eliminates the /smallest/
-- @lv@ (earliest-constructed point's coordinate) first. Haskell's 'rootVar'
-- is now the /largest/ variable present (a direct port of Java's
-- @PolyBasic.lv@), and 'charSet' also processes smallest rootVar first, so a
-- poly tying point_k to earlier points gets class index @k@ in both systems:
-- assign ascending class indices in construction order — smallest suffix →
-- index 0 → earliest-constructed dep var.
--
--   1. Free U-parameters /first/ (smallest indices). They are passive
--      coefficients and must not dominate the 'rootVar' (= largest var)
--      of any poly that also contains a dep — otherwise Wu would group
--      constraints by their free parameter rather than by the point
--      they define.
--   2. Dependent variables /not/ in the conclusion, ascending by suffix
--      (earliest-constructed non-conclusion dep var comes first).
--   3. Dependent variables /in/ the conclusion, ascending by suffix.
--      These get the /largest/ indices so they're eliminated last in
--      the chain, leaving 'remWithChain' with the conclusion points at
--      the tip.
--
-- Numeric (not lex) sort of suffixes is required so that @x10@ sorts after
-- @x2@ rather than before it.
generateVariables :: [Point] -> Conclusion -> [Coord] -> [(Coord, Poly)]
generateVariables points conclusion freeVars = zip finalVariables monicPolys
  where
    allCoords = concatMap (\(Point c1 c2) -> [c1, c2]) points
    allVars = nub $ sortBy (comparing varIndex) (filter isVar allCoords)

    pointsConclusion =
      nub $ concatMap (\(Point c1 c2) -> [c1, c2]) (flatten conclusion)
    depVars = filter (`notElem` freeVars) allVars
    freePresent = filter (`elem` allVars) freeVars

    depConclusion = filter (`elem` depVars) pointsConclusion
    depNotConclusion = depVars \\ depConclusion

    finalVariables = freePresent ++ depNotConclusion ++ depConclusion
    nTotal = length finalVariables
    monicPolys = [var nTotal i | i <- [0 .. nTotal - 1]]

-- | Numeric index of a coordinate variable: the integer suffix of its
-- name (@"x42"@ -> @42@). Non-@X@ or un-parseable names sort as @-1@.
varIndex :: Coord -> Int
varIndex (X name) = case reads (drop 1 name) :: [(Int, String)] of
  [(n, "")] -> n
  _         -> -1
varIndex (Const _) = -1

isVar :: Coord -> Bool
isVar (X _) = True
isVar _ = False

takePairs :: [a] -> [[a]]
takePairs [] = []
takePairs [_] = []
takePairs (x:y:s) = [x, y] : takePairs s

getCoords :: Point -> [(Coord, Poly)] -> (Poly, Poly)
getCoords (Point x y) dict = (coordToPoly x, coordToPoly y)
  where
    coordToPoly (Const n) = fromInteger n
    coordToPoly c         = fromJust $ lookup c dict

geomToAlg :: Hypothesis -> [(Coord, Poly)] -> Poly
geomToAlg (Collinear a b c) dict =
    let (x1, y1) = getCoords a dict
        (x2, y2) = getCoords b dict
        (x3, y3) = getCoords c dict
    in (y2 - y1) * (x3 - x2) - (y3 - y2) * (x2 - x1)
geomToAlg (Parallel (Line pt1 pt2) (Line pt3 pt4)) dict =
    let (x1, y1) = getCoords pt1 dict
        (x2, y2) = getCoords pt2 dict
        (x3, y3) = getCoords pt3 dict
        (x4, y4) = getCoords pt4 dict
    in (y2 - y1) * (x4 - x3) - (y4 - y3) * (x2 - x1)
geomToAlg (SameLen (Line pt1 pt2) (Line pt3 pt4)) dict =
    let (x1, y1) = getCoords pt1 dict
        (x2, y2) = getCoords pt2 dict
        (x3, y3) = getCoords pt3 dict
        (x4, y4) = getCoords pt4 dict
    in (y2 - y1) ^ (2::Int) + (x2 - x1) ^ (2::Int)
     - (y4 - y3) ^ (2::Int) - (x4 - x3) ^ (2::Int)
geomToAlg (Perpendicular (Line pt1 pt2) (Line pt3 pt4)) dict =
    let (x1, y1) = getCoords pt1 dict
        (x2, y2) = getCoords pt2 dict
        (x3, y3) = getCoords pt3 dict
        (x4, y4) = getCoords pt4 dict
    in (y2 - y1) * (y4 - y3) + (x4 - x3) * (x2 - x1)
geomToAlg (InCircle pt1 (Circle pt2 pt3)) dict =
    let (x1, y1) = getCoords pt1 dict
        (x2, y2) = getCoords pt2 dict
        (x3, y3) = getCoords pt3 dict
    in (y3 - y2) ^ (2::Int) + (x3 - x2) ^ (2::Int)
     - (y2 - y1) ^ (2::Int) + (x2 - x1) ^ (2::Int)
geomToAlg (SameAcAngle (Angle pt1 pt2 pt3) (Angle pt4 pt5 pt6)) dict =
    let (x1, y1) = getCoords pt1 dict
        (x2, y2) = getCoords pt2 dict
        (x3, y3) = getCoords pt3 dict
        (x4, y4) = getCoords pt4 dict
        (x5, y5) = getCoords pt5 dict
        (x6, y6) = getCoords pt6 dict
    in ((y1 - y2) * (x3 - x2) - (y3 - y2) * (x1 - x2))
         * ((x4 - x5) * (x6 - x5) + (y4 - y5) * (y6 - y5))
     - ((y4 - y5) * (x6 - x5) - (y6 - y5) * (x4 - x5))
         * ((x1 - x2) * (x3 - x2) + (y1 - y2) * (y3 - y2))
geomToAlg (MidPoint first middle end) dict =
    let (x1, y1) = getCoords first dict
        (xm, ym) = getCoords middle dict
        (x2, y2) = getCoords end dict
    in x1 + x2 - 2 * xm + y1 + y2 - 2 * ym

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a, b, c) = f a b c

instance Flattening Hypothesis where
  flatten (Collinear p1 p2 p3) = nub [p1, p2, p3]
  flatten (Parallel (Line p1 p2) (Line p3 p4)) = nub [p1, p2, p3, p4]
  flatten (Perpendicular (Line p1 p2) (Line p3 p4)) = nub [p1, p2, p3, p4]
  flatten (InCircle p (Circle c r)) = nub [p, c, r]
  flatten (SameLen (Line p1 p2) (Line p3 p4)) = nub [p1, p2, p3, p4]
  flatten (MidPoint first middle end) = nub [first, middle, end]
  flatten (SameAcAngle (Angle p1 p2 p3) (Angle p4 p5 p6)) =
    nub [p1, p2, p3, p4, p5, p6]

instance Ord Coord where
  compare (X x1) (X x2) = compare x1 x2
  compare (Const a) (Const b) = compare a b
  compare (X _) (Const _) = LT
  compare (Const _) (X _) = GT

class Flattening a where
  flatten :: a -> [Point]
