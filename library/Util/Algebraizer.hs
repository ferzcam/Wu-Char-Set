{-# LANGUAGE DataKinds #-}

module Util.Algebraizer where

import Util.Tokenizer
import qualified Data.Map.Strict as Map
import Data.List (isPrefixOf, words, stripPrefix)
import Data.Maybe (fromJust)
import Data.Char (isSpace)

-- | A step in a geometric construction.
-- Each step either introduces a new point or adds a constraint.
data GeoStep
  = GTriangle String String String
    -- ^ Three free points forming a triangle
  | GCircumcenter String String String String
    -- ^ @circumcenter O A B C@: O is the circumcenter of triangle ABC
  | GOrthocenter String String String String
    -- ^ @orthocenter H A B C@: H is the orthocenter of triangle ABC
  | GIncenter String String String String
    -- ^ @incenter I A B C@: I is the incenter of triangle ABC
  | GParallelogram String String String String
    -- ^ @parallelogram D A B C@: D completes the parallelogram ABCD (AD parallel BC, AB parallel DC)
  | GMidpoint String String String
    -- ^ @midpoint M A B@: M is the midpoint of segment AB
  | GFoot String String String String
    -- ^ @foot F P A B@: F is the foot of perpendicular from P to line AB
  | GMirror String String String
    -- ^ @mirror M P Q@: M is the reflection of P through Q
  | GInterLL String String String String String
    -- ^ @inter_ll I A B C D@: I is the intersection of lines AB and CD
  | GInterCL String String String String String
    -- ^ @inter_cl I O R A B@: I is the intersection of circle(O,R) and line AB
  | GInterCC String String String String String
    -- ^ @inter_cc I O1 R1 O2 R2@: I is the intersection of circles (O1,R1) and (O2,R2)
  | GOnCircle String String String
    -- ^ @on_circle P O R@: P is a free point on circle(O,R)
  | GOnLine String String String
    -- ^ @on_line P A B@: P is a free point on line AB
  | GFreePoint String
    -- ^ @free P@: unconstrained free point
  | GDepPoint String
    -- ^ @dep_point P@: fully-dependent point (2 X coords, no automatic constraints)
  | GSemiFree String
    -- ^ @semi_free P@: 1 free U coord + 1 dependent X coord, no automatic constraints
  -- Additional constraints (for "such that" conditions)
  | GCollinear String String String
    -- ^ @collinear A B C@: A, B, C are collinear
  | GPerp String String String String
    -- ^ @perp A B C D@: AB is perpendicular to CD
  | GPara String String String String
    -- ^ @para A B C D@: AB is parallel to CD
  | GCong String String String String
    -- ^ @cong A B C D@: |AB| = |CD|
  | GEqAngle String String String String String String
    -- ^ @eqangle A B C D E F@: angle(ABC) = angle(DEF)
  -- AlphaGeometry-style loose constructors (semi-free point + 1 constraint)
  | GOnTLine String String String String
    -- ^ @on_tline P Q A B@: P is on the line through Q perpendicular to AB (1 DOF)
  | GOnPLine String String String String
    -- ^ @on_pline P Q A B@: P is on the line through Q parallel to AB (1 DOF)
  | GOnBLine String String String
    -- ^ @on_bline P A B@: P is on the perpendicular bisector of AB (1 DOF)
  | GAngleBisector String String String String
    -- ^ @angle_bisector R C A B@: R is on the bisector of angle CAB (1 DOF)
  | GSym String String String String
    -- ^ @sym X P A B@: X is the reflection of P across line AB (0 DOF)
  deriving (Show, Eq)

-- | What to prove
data GeoConclusion
  = GProveCong String String String String
  | GProveCollinear [String]
  | GProveCyclic [String]
  | GProvePara String String String String
  | GProvePerp String String String String
  | GProveEqAngle String String String String String String
  | GProveMidpoint String String String
    -- ^ @prove_midpoint M A B@: M is the midpoint of segment AB
  deriving (Show, Eq)

-- | A complete geometry problem
data GeoProblem = GeoProblem
  { geoSteps      :: [GeoStep]
  , geoConclusion :: GeoConclusion
  } deriving (Show, Eq)

-- | Result of algebraization: points, hypotheses, conclusion, and variable count
data AlgResult = AlgResult
  { arPoints     :: Map.Map String Point
  , arHypotheses :: [Hypothesis]
  , arConclusion :: Conclusion
  , arNumXVars   :: Int      -- ^ Total number of X variables (free + dependent)
  , arFreeVars   :: [Coord]  -- ^ Which X variables are free (unconstrained)
  } deriving (Show)

-- | State threaded through algebraization
data AlgState = AlgState
  { asPoints    :: Map.Map String Point
  , asHyps      :: [Hypothesis]
  , asNextX     :: Int      -- ^ Next X variable index
  , asFreeVars  :: [Coord]  -- ^ Accumulated free coordinates
  , asFrameUsed :: Int      -- ^ How many frame slots used (0, 1, or 2)
  } deriving (Show)

-- | Create initial state. The coordinate frame is fixed lazily as the
-- first 'GTriangle' / 'GFreePoint' points are processed: the first
-- anchor goes to the origin, the second to @(0, u)@ (y-axis) with a
-- single free parameter, and from the third onward points are fully
-- free. This matches the Java reference, which uses @A=(0,0), B=(0,x4)@
-- and relies on the implicit WLOG invariance of Wu's method under
-- rigid motions — it saves three free variables up front and keeps
-- every hypothesis involving @A@ or @B@ much smaller.
initState :: AlgState
initState = AlgState Map.empty [] 1 [] 0

-- | Create a fresh X coordinate
freshX :: AlgState -> (Coord, AlgState)
freshX st = (X ("x" ++ show (asNextX st)), st { asNextX = asNextX st + 1 })

-- | Mark a coordinate as free (unconstrained)
markFree :: Coord -> AlgState -> AlgState
markFree c st = st { asFreeVars = c : asFreeVars st }

-- | Create a fresh fully-determined point (2 X coords, both dependent)
freshPointXX :: String -> AlgState -> (Point, AlgState)
freshPointXX name st =
  let (cx, st1) = freshX st
      (cy, st2) = freshX st1
      pt = Point cx cy
  in (pt, st2 { asPoints = Map.insert name pt (asPoints st2) })

-- | Create a semi-free point (2 X coords, first is free, second dependent)
freshPointFreeX :: String -> AlgState -> (Point, AlgState)
freshPointFreeX name st =
  let (cx, st1) = freshX st
      st2 = markFree cx st1
      (cy, st3) = freshX st2
      pt = Point cx cy
  in (pt, st3 { asPoints = Map.insert name pt (asPoints st3) })

-- | Create a fully free point (2 X coords, both free)
freshPointFreeFree :: String -> AlgState -> (Point, AlgState)
freshPointFreeFree name st =
  let (cx, st1) = freshX st
      st2 = markFree cx st1
      (cy, st3) = freshX st2
      st4 = markFree cy st3
      pt = Point cx cy
  in (pt, st4 { asPoints = Map.insert name pt (asPoints st4) })

-- | Register a point with fixed coordinates (constants or existing coords)
fixedPoint :: String -> Coord -> Coord -> AlgState -> AlgState
fixedPoint name cx cy st = st { asPoints = Map.insert name (Point cx cy) (asPoints st) }

-- | Look up a point by name
lookupPt :: String -> AlgState -> Point
lookupPt name st = case Map.lookup name (asPoints st) of
  Just pt -> pt
  Nothing -> error $ "Point not found: " ++ name

-- | Add hypothesis constraints
addHyps :: [Hypothesis] -> AlgState -> AlgState
addHyps hs st = st { asHyps = asHyps st ++ hs }

-- | Process a single construction step
processStep :: GeoStep -> AlgState -> AlgState

-- Triangle: A at origin, B on x-axis (1 free coord), C free (2 free coords).
-- Marks the frame as fully consumed so any subsequent 'GFreePoint' gets
-- full 2-DOF coordinates rather than clashing with A/B.
processStep (GTriangle a b c) st =
  let st1 = fixedPoint a (Const 0) (Const 0) st
      (bx, st2) = freshX st1
      st3 = markFree bx st2
      st4 = fixedPoint b bx (Const 0) st3
      (cx, st5) = freshX st4
      st6 = markFree cx st5
      (cy, st7) = freshX st6
      st8 = markFree cy st7
      st9 = fixedPoint c cx cy st8
  in st9 { asFrameUsed = 2 }

-- Circumcenter: 2 constraints (equidistant from vertices)
processStep (GCircumcenter o a b c) st =
  let (ptO, st1) = freshPointXX o st
      ptA = lookupPt a st1
      ptB = lookupPt b st1
      ptC = lookupPt c st1
      h1 = SameLen (Line ptO ptA) (Line ptO ptB)
      h2 = SameLen (Line ptO ptA) (Line ptO ptC)
  in addHyps [h1, h2] st1

-- Orthocenter: 2 constraints (AH perp BC, BH perp AC)
processStep (GOrthocenter h a b c) st =
  let (ptH, st1) = freshPointXX h st
      ptA = lookupPt a st1
      ptB = lookupPt b st1
      ptC = lookupPt c st1
      h1 = Perpendicular (Line ptA ptH) (Line ptB ptC)
      h2 = Perpendicular (Line ptB ptH) (Line ptA ptC)
  in addHyps [h1, h2] st1

-- Incenter: 2 constraints (angle bisectors from A and C)
processStep (GIncenter i a b c) st =
  let (ptI, st1) = freshPointXX i st
      ptA = lookupPt a st1
      ptB = lookupPt b st1
      ptC = lookupPt c st1
      h1 = SameAcAngle (Angle ptB ptA ptI) (Angle ptI ptA ptC)
      h2 = SameAcAngle (Angle ptA ptC ptI) (Angle ptI ptC ptB)
  in addHyps [h1, h2] st1

-- Parallelogram: D completes ABCD so AD || BC and AB || DC
processStep (GParallelogram d a b c) st =
  let (ptD, st1) = freshPointXX d st
      ptA = lookupPt a st1
      ptB = lookupPt b st1
      ptC = lookupPt c st1
      h1 = Parallel (Line ptA ptD) (Line ptB ptC)
      h2 = Parallel (Line ptA ptB) (Line ptD ptC)
  in addHyps [h1, h2] st1

-- Midpoint: 2 constraints (collinear + equidistant)
processStep (GMidpoint m a b) st =
  let (ptM, st1) = freshPointXX m st
      ptA = lookupPt a st1
      ptB = lookupPt b st1
      h1 = Collinear ptA ptM ptB
      h2 = SameLen (Line ptA ptM) (Line ptM ptB)
  in addHyps [h1, h2] st1

-- Foot of perpendicular: 2 constraints (on line + perpendicular)
processStep (GFoot f p a b) st =
  let (ptF, st1) = freshPointXX f st
      ptP = lookupPt p st1
      ptA = lookupPt a st1
      ptB = lookupPt b st1
      h1 = Collinear ptA ptF ptB
      h2 = Perpendicular (Line ptP ptF) (Line ptA ptB)
  in addHyps [h1, h2] st1

-- Mirror (reflection through a point): Q is midpoint of P and M
processStep (GMirror m p q) st =
  let (ptM, st1) = freshPointXX m st
      ptP = lookupPt p st1
      ptQ = lookupPt q st1
      -- Q is midpoint of P and M means:
      h1 = Collinear ptP ptQ ptM
      h2 = SameLen (Line ptP ptQ) (Line ptQ ptM)
  in addHyps [h1, h2] st1

-- Line-line intersection: 2 constraints (on both lines)
processStep (GInterLL i a b c d) st =
  let (ptI, st1) = freshPointXX i st
      ptA = lookupPt a st1
      ptB = lookupPt b st1
      ptC = lookupPt c st1
      ptD = lookupPt d st1
      h1 = Collinear ptA ptI ptB
      h2 = Collinear ptC ptI ptD
  in addHyps [h1, h2] st1

-- Circle-line intersection: 2 constraints (on circle + on line)
processStep (GInterCL i o r a b) st =
  let (ptI, st1) = freshPointXX i st
      ptO = lookupPt o st1
      ptR = lookupPt r st1
      ptA = lookupPt a st1
      ptB = lookupPt b st1
      h1 = SameLen (Line ptO ptI) (Line ptO ptR)
      h2 = Collinear ptA ptI ptB
  in addHyps [h1, h2] st1

-- Circle-circle intersection: 2 constraints (on both circles)
processStep (GInterCC i o1 r1 o2 r2) st =
  let (ptI, st1) = freshPointXX i st
      ptO1 = lookupPt o1 st1
      ptR1 = lookupPt r1 st1
      ptO2 = lookupPt o2 st1
      ptR2 = lookupPt r2 st1
      h1 = SameLen (Line ptO1 ptI) (Line ptO1 ptR1)
      h2 = SameLen (Line ptO2 ptI) (Line ptO2 ptR2)
  in addHyps [h1, h2] st1

-- Point on circle (1 DOF): 1 free coord + 1 dependent + 1 constraint
processStep (GOnCircle p o r) st =
  let (ptP, st1) = freshPointFreeX p st
      ptO = lookupPt o st1
      ptR = lookupPt r st1
      h1 = SameLen (Line ptO ptP) (Line ptO ptR)
  in addHyps [h1] st1

-- Point on line (1 DOF): 1 free coord + 1 dependent + 1 constraint
processStep (GOnLine p a b) st =
  let (ptP, st1) = freshPointFreeX p st
      ptA = lookupPt a st1
      ptB = lookupPt b st1
      h1 = Collinear ptA ptP ptB
  in addHyps [h1] st1

-- Free point (0 constraints, 2 DOF).
--
-- The first two free points in a problem are consumed as frame anchors
-- rather than getting full 2-DOF coordinate pairs: the first goes to
-- the origin @(0,0)@ (0 free vars), the second to @(0, u)@ on the
-- y-axis (1 free var). From the third free point onward, points are
-- fully free. This mirrors Java's canonical embedding and is what
-- lets perpendicularities like @PERP (A G1) (B A)@ collapse from
-- 8-term polynomials to 2 terms.
processStep (GFreePoint p) st =
  case asFrameUsed st of
    0 ->
      let st1 = fixedPoint p (Const 0) (Const 0) st
      in st1 { asFrameUsed = 1 }
    1 ->
      let (uy, st1) = freshX st
          st2       = markFree uy st1
          st3       = fixedPoint p (Const 0) uy st2
      in st3 { asFrameUsed = 2 }
    _ ->
      let (_, st1) = freshPointFreeFree p st
      in st1

-- Dependent point (0 constraints here, 2 X coords — constraints added separately)
processStep (GDepPoint p) st =
  let (_, st1) = freshPointXX p st
  in st1

-- Semi-free point (0 constraints here, 1 free + 1 dependent — constraint added separately)
processStep (GSemiFree p) st =
  let (_, st1) = freshPointFreeX p st
  in st1

-- Pure constraints (no new points)
processStep (GCollinear a b c) st =
  let ptA = lookupPt a st
      ptB = lookupPt b st
      ptC = lookupPt c st
  in addHyps [Collinear ptA ptB ptC] st

processStep (GPerp a b c d) st =
  let ptA = lookupPt a st
      ptB = lookupPt b st
      ptC = lookupPt c st
      ptD = lookupPt d st
  in addHyps [Perpendicular (Line ptA ptB) (Line ptC ptD)] st

processStep (GPara a b c d) st =
  let ptA = lookupPt a st
      ptB = lookupPt b st
      ptC = lookupPt c st
      ptD = lookupPt d st
  in addHyps [Parallel (Line ptA ptB) (Line ptC ptD)] st

processStep (GCong a b c d) st =
  let ptA = lookupPt a st
      ptB = lookupPt b st
      ptC = lookupPt c st
      ptD = lookupPt d st
  in addHyps [SameLen (Line ptA ptB) (Line ptC ptD)] st

processStep (GEqAngle a b c d e f) st =
  let ptA = lookupPt a st
      ptB = lookupPt b st
      ptC = lookupPt c st
      ptD = lookupPt d st
      ptE = lookupPt e st
      ptF = lookupPt f st
  in addHyps [SameAcAngle (Angle ptA ptB ptC) (Angle ptD ptE ptF)] st

-- AG-style loose constructors: new semi-free point + 1 constraint.
processStep (GOnTLine p q a b) st =
  let (ptP, st1) = freshPointFreeX p st
      ptQ = lookupPt q st1
      ptA = lookupPt a st1
      ptB = lookupPt b st1
  in addHyps [Perpendicular (Line ptQ ptP) (Line ptA ptB)] st1

processStep (GOnPLine p q a b) st =
  let (ptP, st1) = freshPointFreeX p st
      ptQ = lookupPt q st1
      ptA = lookupPt a st1
      ptB = lookupPt b st1
  in addHyps [Parallel (Line ptQ ptP) (Line ptA ptB)] st1

processStep (GOnBLine p a b) st =
  let (ptP, st1) = freshPointFreeX p st
      ptA = lookupPt a st1
      ptB = lookupPt b st1
  in addHyps [SameLen (Line ptP ptA) (Line ptP ptB)] st1

processStep (GAngleBisector r c a b) st =
  let (ptR, st1) = freshPointFreeX r st
      ptC = lookupPt c st1
      ptA = lookupPt a st1
      ptB = lookupPt b st1
  in addHyps [SameAcAngle (Angle ptC ptA ptR) (Angle ptR ptA ptB)] st1

-- Line reflection: X is the reflection of P across line AB.
-- Encoded as "A and B are both equidistant from X and P" (they lie on the
-- perpendicular bisector of XP, which is line AB).
processStep (GSym x p a b) st =
  let (ptX, st1) = freshPointXX x st
      ptP = lookupPt p st1
      ptA = lookupPt a st1
      ptB = lookupPt b st1
      h1 = SameLen (Line ptA ptX) (Line ptA ptP)
      h2 = SameLen (Line ptB ptX) (Line ptB ptP)
  in addHyps [h1, h2] st1


-- | Convert a GeoConclusion to a Hypothesis (same type, used as conclusion)
conclusionToHyp :: GeoConclusion -> AlgState -> Conclusion
conclusionToHyp (GProveCong a b c d) st =
  SameLen (Line (lookupPt a st) (lookupPt b st))
          (Line (lookupPt c st) (lookupPt d st))
conclusionToHyp (GProveCollinear [a, b, c]) st =
  Collinear (lookupPt a st) (lookupPt b st) (lookupPt c st)
conclusionToHyp (GProvePara a b c d) st =
  Parallel (Line (lookupPt a st) (lookupPt b st))
           (Line (lookupPt c st) (lookupPt d st))
conclusionToHyp (GProvePerp a b c d) st =
  Perpendicular (Line (lookupPt a st) (lookupPt b st))
                (Line (lookupPt c st) (lookupPt d st))
conclusionToHyp (GProveEqAngle a b c d e f) st =
  SameAcAngle (Angle (lookupPt a st) (lookupPt b st) (lookupPt c st))
              (Angle (lookupPt d st) (lookupPt e st) (lookupPt f st))
conclusionToHyp (GProveMidpoint m a b) st =
  MidPoint (lookupPt m st) (lookupPt a st) (lookupPt b st)
conclusionToHyp (GProveCyclic [a, b, c, d]) st =
  -- Cyclic: use the fact that |OA|=|OB|=|OC|=|OD| for some center O.
  -- But since we don't have a Cyclic hypothesis type, we use the
  -- determinant condition. For now, express as SameAcAngle.
  -- cyclic(A,B,C,D) iff angle(BAC) = angle(BDC)
  SameAcAngle (Angle (lookupPt b st) (lookupPt a st) (lookupPt c st))
              (Angle (lookupPt b st) (lookupPt d st) (lookupPt c st))
conclusionToHyp _ _ = error "Unsupported conclusion type"


-- | Algebraize a complete geometry problem
algebraize :: GeoProblem -> AlgResult
algebraize prob =
  let finalState = foldl (flip processStep) initState (geoSteps prob)
      concl = conclusionToHyp (geoConclusion prob) finalState
  in AlgResult
    { arPoints     = asPoints finalState
    , arHypotheses = asHyps finalState
    , arConclusion = concl
    , arNumXVars   = asNextX finalState - 1
    , arFreeVars   = reverse (asFreeVars finalState)
    }


-- ================================================================
-- Simple DSL Parser
-- ================================================================

-- | Parse a problem from DSL text format.
--
-- Format (one instruction per line, # comments):
--
-- > triangle A B C
-- > circumcenter O A B C
-- > orthocenter H A B C
-- > midpoint M A B
-- > foot F P A B
-- > mirror M P Q
-- > inter_ll I A B C D
-- > inter_cl I O R A B
-- > inter_cc I O1 R1 O2 R2
-- > on_circle P O R
-- > on_line P A B
-- > free P
-- > dep_point P
-- > semi_free P
-- > collinear A B C
-- > perp A B C D
-- > para A B C D
-- > cong A B C D
-- > eqangle A B C D E F
-- > prove_cong A B C D
-- > prove_collinear A B C
-- > prove_cyclic A B C D
-- > prove_para A B C D
-- > prove_perp A B C D
parseProblem :: String -> GeoProblem
parseProblem input =
  let ls = filter (not . isBlankOrComment) (lines input)
      (steps, concl) = parseLines ls
  in GeoProblem steps concl

isBlankOrComment :: String -> Bool
isBlankOrComment s =
  let trimmed = dropWhile isSpace s
  in null trimmed || head trimmed == '#'

parseLines :: [String] -> ([GeoStep], GeoConclusion)
parseLines [] = error "No conclusion found"
parseLines [l] = case parseConclusion l of
  Just c  -> ([], c)
  Nothing -> error $ "Last line must be a conclusion: " ++ l
parseLines (l:ls) = case parseConclusion l of
  Just c  -> ([], c)
  Nothing -> let step = parseStep l
                 (rest, concl) = parseLines ls
             in (step : rest, concl)

parseStep :: String -> GeoStep
parseStep line = case words line of
  ("triangle":a:b:c:_)       -> GTriangle a b c
  ("circumcenter":o:a:b:c:_) -> GCircumcenter o a b c
  ("orthocenter":h:a:b:c:_)  -> GOrthocenter h a b c
  ("incenter":i:a:b:c:_)     -> GIncenter i a b c
  ("parallelogram":d:a:b:c:_)-> GParallelogram d a b c
  ("midpoint":m:a:b:_)       -> GMidpoint m a b
  ("foot":f:p:a:b:_)         -> GFoot f p a b
  ("mirror":m:p:q:_)         -> GMirror m p q
  ("inter_ll":i:a:b:c:d:_)   -> GInterLL i a b c d
  ("inter_cl":i:o:r:a:b:_)   -> GInterCL i o r a b
  ("inter_cc":i:o1:r1:o2:r2:_) -> GInterCC i o1 r1 o2 r2
  ("on_circle":p:o:r:_)      -> GOnCircle p o r
  ("on_line":p:a:b:_)        -> GOnLine p a b
  ("free":p:_)               -> GFreePoint p
  ("dep_point":p:_)           -> GDepPoint p
  ("semi_free":p:_)           -> GSemiFree p
  ("collinear":a:b:c:_)      -> GCollinear a b c
  ("perp":a:b:c:d:_)         -> GPerp a b c d
  ("para":a:b:c:d:_)         -> GPara a b c d
  ("cong":a:b:c:d:_)         -> GCong a b c d
  ("eqangle":a:b:c:d:e:f:_)  -> GEqAngle a b c d e f
  ("on_tline":p:q:a:b:_)     -> GOnTLine p q a b
  ("on_pline":p:q:a:b:_)     -> GOnPLine p q a b
  ("on_bline":p:a:b:_)       -> GOnBLine p a b
  ("angle_bisector":r:c:a:b:_) -> GAngleBisector r c a b
  ("sym":x:p:a:b:_)          -> GSym x p a b
  _ -> error $ "Cannot parse step: " ++ line

parseConclusion :: String -> Maybe GeoConclusion
parseConclusion line = case words line of
  ("prove_cong":a:b:c:d:_)       -> Just $ GProveCong a b c d
  ("prove_collinear":pts)         -> Just $ GProveCollinear pts
  ("prove_cyclic":pts)            -> Just $ GProveCyclic pts
  ("prove_para":a:b:c:d:_)       -> Just $ GProvePara a b c d
  ("prove_perp":a:b:c:d:_)       -> Just $ GProvePerp a b c d
  ("prove_eqangle":a:b:c:d:e:f:_)-> Just $ GProveEqAngle a b c d e f
  ("prove_midpoint":m:a:b:_)     -> Just $ GProveMidpoint m a b
  _ -> Nothing


-- | Pretty-print an AlgResult for inspection
showAlgResult :: AlgResult -> String
showAlgResult ar =
  let nFree = length (arFreeVars ar)
      nDep  = arNumXVars ar - nFree
  in unlines
  [ "Variables: " ++ show (arNumXVars ar)
      ++ " total (" ++ show nDep ++ " dependent, " ++ show nFree ++ " free)"
  , "Free vars: " ++ show (arFreeVars ar)
  , "Points:"
  , unlines [ "  " ++ name ++ " = " ++ show pt
            | (name, pt) <- Map.toAscList (arPoints ar) ]
  , "Hypotheses (" ++ show (length (arHypotheses ar)) ++ "):"
  , unlines [ "  " ++ show h | h <- arHypotheses ar ]
  , "Conclusion: " ++ show (arConclusion ar)
  ]
