-- | Multivariate polynomials over 'Integer', stored in a recursive
-- distributive form modelled on Java JGEX's @TMono@.
--
-- A non-zero polynomial is a descending chain of monomials
--
-- @
--   c_1 * x_v1^d_1 + c_2 * x_v2^d_2 + ... + const
-- @
--
-- where each @c_i@ is itself a polynomial using only variables strictly
-- greater than @v_i@. This makes class-variable operations (degree in
-- @v@, leading coefficient in @v@, strip leading term in @v@) /O(1)/
-- whenever @v@ is the root variable — which is always the case during
-- Wu's elimination because variables are processed in ascending order.
module Polynomial.Poly
  ( Poly(..)
  , mkPoly
  , isZero
  , zeroPoly
  , constPoly
  , var
  , fromTerms
  , numTerms
  , polyArity
  , showPoly
  -- Class variable operations
  , classVarDeg
  , varInPoly
  , leadingCoeffPoly
  , existOneDegPoly
  -- Simplification (ported from Java PolyBasic)
  , coefGcd
  , factor1
  , simplify
  , factorRemove
  , exactDiv
  -- Pseudo-remainder
  , pseudoRemainder
  , pseudoRemainders
  ) where

import qualified Data.IntMap.Strict as IntMap
import Data.IntMap.Strict (IntMap)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed (Vector)
import Data.List (foldl', find, intercalate, sortBy)
import Data.Ord (comparing, Down(..))
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import System.IO.Unsafe (unsafePerformIO)

-- ---------------------------------------------------------------------------
-- Core recursive type
-- ---------------------------------------------------------------------------

-- | Internal recursive polynomial node.
--
-- Invariants (canonical form):
--
--   * 'TZero' is the unique zero representation.
--   * @'TConst' n@ implies @n /= 0@.
--   * @'TNode' x d c r@ implies @d >= 1@, @c /= 'TZero'@, @c@ uses only
--     variables strictly greater than @x@, and @r@ is either 'TZero',
--     'TConst', @'TNode' x d' _ _@ with @d' < d@ (same variable, lower
--     degree), or @'TNode' x' _ _ _@ with @x' > x@ (deeper variable).
data TPoly
  = TZero
  | TConst !Integer
  | TNode !Int !Int !TPoly !TPoly
  --       v    d    coef    rest
  deriving Eq

-- | A multivariate polynomial with 'Integer' coefficients. The '_polyId'
-- field is a globally-unique identity assigned at construction time —
-- 'Eq' compares on id, not structure, matching the reference-equality
-- semantics Java relies on for @TMono@ in @CharSet.charset@ and
-- @PolyBasic@. Always build 'Poly' via 'mkPoly' so the id is freshly
-- allocated.
data Poly = Poly
  { _polyArity :: {-# UNPACK #-} !Int
  , _polyId    :: {-# UNPACK #-} !Int
  , _polyBody  :: !TPoly
  }

polyArity :: Poly -> Int
polyArity = _polyArity

-- | Identity-based equality. Two polys are equal iff they came from the
-- same 'mkPoly' call. This matches Java's reference equality on @TMono@.
instance Eq Poly where
  a == b = _polyId a == _polyId b

instance Show Poly where
  show = showPoly

-- ---------------------------------------------------------------------------
-- Identity allocation
-- ---------------------------------------------------------------------------

-- | Global counter for 'Poly' identities. Accessed via 'unsafePerformIO'
-- so that pure construction sites can allocate a fresh id. NOINLINE is
-- essential: GHC would otherwise share the single call across all users
-- and give every 'Poly' the same id.
{-# NOINLINE polyIdCounter #-}
polyIdCounter :: IORef Int
polyIdCounter = unsafePerformIO (newIORef 0)

-- | Smart constructor that allocates a fresh identity. Use this in place
-- of the raw 'Poly' constructor everywhere so equality is identity-based.
{-# NOINLINE mkPoly #-}
mkPoly :: Int -> TPoly -> Poly
mkPoly ar body = unsafePerformIO $ do
  i <- atomicModifyIORef' polyIdCounter (\n -> (n + 1, n))
  pure (Poly ar i body)

-- ---------------------------------------------------------------------------
-- Smart constructors
-- ---------------------------------------------------------------------------

-- | Smart 'TNode' constructor that preserves canonical form when the
-- coefficient degenerates to 'TZero'.
tNode :: Int -> Int -> TPoly -> TPoly -> TPoly
tNode _ _ TZero rest = rest
tNode v d c rest
  | d == 0    = tadd c rest
  | otherwise = TNode v d c rest

-- ---------------------------------------------------------------------------
-- Construction
-- ---------------------------------------------------------------------------

zeroPoly :: Poly
zeroPoly = mkPoly 0 TZero

isZero :: Poly -> Bool
isZero p = case _polyBody p of
  TZero -> True
  _     -> False

constPoly :: Int -> Integer -> Poly
constPoly ar 0 = mkPoly ar TZero
constPoly ar n = mkPoly ar (TConst n)

-- | @var arity i@ creates the polynomial @x_i@.
var :: Int -> Int -> Poly
var ar i = mkPoly ar (TNode i 1 (TConst 1) TZero)

-- | Build a polynomial from a list of @(exponent-vector, coefficient)@
-- pairs. Matching keys are summed; zero coefficients are dropped.
fromTerms :: Int -> [(Vector Int, Integer)] -> Poly
fromTerms ar ts =
  mkPoly ar $ foldl' (\acc (vec, c) -> tadd acc (monomialFromVec vec c)) TZero ts

-- | Build a single monomial from an exponent vector and a coefficient.
monomialFromVec :: Vector Int -> Integer -> TPoly
monomialFromVec _ 0 = TZero
monomialFromVec vec c =
  let n = V.length vec
      vars = [(i, vec V.! i) | i <- [0 .. n - 1], vec V.! i > 0]
      -- Ascending order of var index so foldr wraps outer-first.
  in foldr (\(i, e) acc -> TNode i e acc TZero) (TConst c) vars

numTerms :: Poly -> Int
numTerms (Poly _ _ p) = go p
  where
    go TZero            = 0
    go (TConst _)       = 1
    go (TNode _ _ c r)  = go c + go r

-- ---------------------------------------------------------------------------
-- Addition, subtraction, negation
-- ---------------------------------------------------------------------------

tadd :: TPoly -> TPoly -> TPoly
tadd TZero q = q
tadd p TZero = p
tadd (TConst a) (TConst b) =
  let s = a + b in if s == 0 then TZero else TConst s
tadd c@(TConst _) (TNode x d cp r) = TNode x d cp (tadd c r)
tadd (TNode x d cp r) c@(TConst _) = TNode x d cp (tadd r c)
tadd p@(TNode xp dp cp pr) q@(TNode xq dq cq qr)
  | xp < xq   = TNode xp dp cp (tadd pr q)
  | xp > xq   = TNode xq dq cq (tadd p qr)
  | dp > dq   = TNode xp dp cp (tadd pr q)
  | dp < dq   = TNode xq dq cq (tadd p qr)
  | otherwise =
      case tadd cp cq of
        TZero -> tadd pr qr
        c'    -> TNode xp dp c' (tadd pr qr)

tneg :: TPoly -> TPoly
tneg TZero           = TZero
tneg (TConst n)      = TConst (negate n)
tneg (TNode x d c r) = TNode x d (tneg c) (tneg r)

tsub :: TPoly -> TPoly -> TPoly
tsub p q = tadd p (tneg q)

-- ---------------------------------------------------------------------------
-- Multiplication
-- ---------------------------------------------------------------------------

-- | Scale every coefficient of @p@ by the integer @n@.
scaleInt :: Integer -> TPoly -> TPoly
scaleInt 0    _                  = TZero
scaleInt 1    p                  = p
scaleInt (-1) p                  = tneg p
scaleInt _    TZero              = TZero
scaleInt n    (TConst m)         = TConst (n * m)
scaleInt n    (TNode x d c r)    = TNode x d (scaleInt n c) (scaleInt n r)

tmul :: TPoly -> TPoly -> TPoly
tmul TZero _ = TZero
tmul _ TZero = TZero
tmul (TConst a) q = scaleInt a q
tmul p (TConst b) = scaleInt b p
tmul (TNode xp dp cp pr) q =
  tadd (shiftVarDeg xp dp (tmul cp q)) (tmul pr q)

-- | Multiply @p@ by @x_v^d@. Walks the tree once.
shiftVarDeg :: Int -> Int -> TPoly -> TPoly
shiftVarDeg _ 0 p              = p
shiftVarDeg _ _ TZero          = TZero
shiftVarDeg v d c@(TConst _)   = TNode v d c TZero
shiftVarDeg v d p@(TNode x deg c r)
  | v == x    = TNode v (deg + d) c (shiftVarDeg v d r)
  | v < x     = TNode v d p TZero
  | otherwise = TNode x deg (shiftVarDeg v d c) (shiftVarDeg v d r)

-- ---------------------------------------------------------------------------
-- Num instance
-- ---------------------------------------------------------------------------

instance Num Poly where
  Poly ar1 _ p + Poly ar2 _ q = mkPoly (max ar1 ar2) (tadd p q)
  Poly ar1 _ p - Poly ar2 _ q = mkPoly (max ar1 ar2) (tsub p q)
  Poly ar1 _ p * Poly ar2 _ q = mkPoly (max ar1 ar2) (tmul p q)
  abs = id
  signum p = if isZero p then 0 else mkPoly (_polyArity p) (TConst 1)
  fromInteger 0 = mkPoly 0 TZero
  fromInteger n = mkPoly 0 (TConst n)
  negate (Poly ar _ p) = mkPoly ar (tneg p)

-- ---------------------------------------------------------------------------
-- Class variable operations
-- ---------------------------------------------------------------------------

-- | Maximum degree of variable @v@ across all monomials. /O(1)/ when @v@
-- is the root variable (the common case during elimination); /O(n)/
-- otherwise.
classVarDeg :: Poly -> Int -> Int
classVarDeg (Poly _ _ p) v = tClassVarDeg p v

tClassVarDeg :: TPoly -> Int -> Int
tClassVarDeg TZero      _ = 0
tClassVarDeg (TConst _) _ = 0
tClassVarDeg (TNode x d c r) v
  | x == v    = d
  | x >  v    = 0
  | otherwise = max (tClassVarDeg c v) (tClassVarDeg r v)

varInPoly :: Poly -> Int -> Bool
varInPoly (Poly _ _ p) v = tVarIn p v

tVarIn :: TPoly -> Int -> Bool
tVarIn TZero      _ = False
tVarIn (TConst _) _ = False
tVarIn (TNode x _ c r) v
  | x == v    = True
  | x >  v    = False
  | otherwise = tVarIn c v || tVarIn r v

-- | Coefficient of @x_v^{deg}@ where @deg = 'classVarDeg' p v@. Returns
-- @p@ itself when @v@ is absent from @p@.
leadingCoeffPoly :: Poly -> Int -> Poly
leadingCoeffPoly (Poly ar _ p) v = mkPoly ar (tLeadingCoeff p v)

tLeadingCoeff :: TPoly -> Int -> TPoly
tLeadingCoeff TZero        _ = TZero
tLeadingCoeff t@(TConst _) _ = t
tLeadingCoeff p@(TNode x d c r) v
  | x == v    = c
  | x >  v    = p
  | otherwise =
      -- v is nested below x: leading-in-v must be assembled from pieces
      -- in c (wrapped in x^d) and r. This path is not hit during normal
      -- elimination but must be correct for callers that query arbitrary
      -- variables.
      let dC = tClassVarDeg c v
          dR = tClassVarDeg r v
      in case compare dC dR of
           GT -> TNode x d (tLeadingCoeff c v) TZero
           LT -> tLeadingCoeff r v
           EQ -> tadd (TNode x d (tLeadingCoeff c v) TZero) (tLeadingCoeff r v)

existOneDegPoly :: [Poly] -> Int -> Maybe Poly
existOneDegPoly polys v = find isOneDeg polys
  where
    isOneDeg p = not (isZero p) && classVarDeg p v == 1

-- | Drop the leading term in @v@ (assumes the root is exactly @v@ with
-- the given degree). Used in 'pseudoRemainder' to precompute @g_lo@.
stripLeadingV :: TPoly -> Int -> Int -> TPoly
stripLeadingV (TNode x d _ r) v m
  | x == v && d == m = r
stripLeadingV p _ _ = p

-- ---------------------------------------------------------------------------
-- coefGcd and factor1 (ported from Java PolyBasic)
-- ---------------------------------------------------------------------------

-- | Fold over every integer coefficient in the tree. Used for GCD.
foldCoeffs :: (a -> Integer -> a) -> a -> TPoly -> a
foldCoeffs _ z TZero           = z
foldCoeffs f z (TConst n)      = f z n
foldCoeffs f z (TNode _ _ c r) = foldCoeffs f (foldCoeffs f z c) r

-- | GCD of all coefficients, with early exit when it reaches 1.
gcdAll :: TPoly -> Integer
gcdAll = go 0
  where
    go acc TZero      = acc
    go acc (TConst n) = gcd acc n
    go 1 _            = 1
    go acc (TNode _ _ c r) =
      let acc' = go acc c in
      if acc' == 1 then 1 else go acc' r

-- | Leading coefficient of the leading monomial (innermost along the
-- coef spine). Used only for sign normalisation in 'coefGcd'.
leadingIntCoeff :: TPoly -> Integer
leadingIntCoeff TZero            = 0
leadingIntCoeff (TConst n)       = n
leadingIntCoeff (TNode _ _ c _)  = leadingIntCoeff c

divideCoeffs :: Integer -> TPoly -> TPoly
divideCoeffs _ TZero            = TZero
divideCoeffs g (TConst n)       = TConst (n `quot` g)
divideCoeffs g (TNode x d c r)  = TNode x d (divideCoeffs g c) (divideCoeffs g r)

-- | Divide all coefficients by their GCD and normalise the sign of the
-- leading monomial to be positive. (Java: @PolyBasic.coefgcd@.)
coefGcd :: Poly -> Poly
coefGcd (Poly ar _ p) = mkPoly ar (tCoefGcd p)

tCoefGcd :: TPoly -> TPoly
tCoefGcd TZero = TZero
tCoefGcd p =
  let g  = gcdAll p
      lc = leadingIntCoeff p
      g' = if lc < 0 then negate g else g
  in if g' == 0 || g' == 1 then p else divideCoeffs g' p

-- | Extract common variable powers shared by every monomial. (Java:
-- @PolyBasic.factor1@.)
factor1 :: Poly -> Poly
factor1 (Poly ar _ p) = mkPoly ar (tFactor1 p)

tFactor1 :: TPoly -> TPoly
tFactor1 TZero       = TZero
tFactor1 (TConst n)  = TConst n
tFactor1 p =
  case collectExpMaps p of
    []     -> p
    [_]    -> p                             -- single monomial → nothing to share
    (m:ms) ->
      let mins = combineMins m ms
      in if IntMap.null mins then p else dropFactor mins p

-- | Walk the tree, returning one exponent map per monomial. Ancestors'
-- exponents are threaded through the context.
collectExpMaps :: TPoly -> [IntMap Int]
collectExpMaps = go IntMap.empty
  where
    go _   TZero            = []
    go ctx (TConst _)       = [ctx]
    go ctx (TNode x d c r)  =
      go (IntMap.insert x d ctx) c ++ go ctx r

-- | Fold 'IntMap.intersectionWith min' across a non-empty list, with
-- early exit once the accumulator has no keys left.
combineMins :: IntMap Int -> [IntMap Int] -> IntMap Int
combineMins acc _  | IntMap.null acc = IntMap.empty
combineMins acc [] = acc
combineMins acc (m:ms) = combineMins (IntMap.intersectionWith min acc m) ms

-- | Divide every monomial of @p@ by the product @∏ x_v^{e_v}@ given by
-- @factor@. Assumes @factor@ was obtained from 'collectMinExps' so the
-- division is exact at every monomial.
dropFactor :: IntMap Int -> TPoly -> TPoly
dropFactor _ TZero       = TZero
dropFactor _ c@(TConst _) = c
dropFactor factor (TNode x d c r) =
  let k  = IntMap.findWithDefault 0 x factor
      d' = d - k
      c' = dropFactor factor c
      r' = dropFactor factor r
  in if d' <= 0
     then tadd c' r'
     else TNode x d' c' r'

simplify :: Poly -> Poly
simplify = coefGcd . factor1

-- | Stub: the flat-Map implementation tried an exact polynomial division
-- by the leading-coef factor. Porting exact division to the recursive
-- form is non-trivial and belongs in a later incremental pass.
factorRemove :: Poly -> Poly -> Poly
factorRemove result _ = result

-- | Stub (see 'factorRemove').
exactDiv :: Poly -> Poly -> Maybe Poly
exactDiv _ _ = Nothing

-- ---------------------------------------------------------------------------
-- Pseudo-remainder — Java PolyBasic.prem / prem1
-- ---------------------------------------------------------------------------

-- | Pseudo-remainder of @f@ divided by @g@ with respect to variable @v@.
-- The quotient is never consumed by callers, so the first component of
-- the pair is always @zeroPoly@.
--
-- Applies 'coefGcd' after every reduction step to keep coefficients
-- bounded (Java: @PolyBasic.prem1@).
pseudoRemainder :: Poly -> Poly -> Int -> (Poly, Poly)
pseudoRemainder (Poly arF _ f) (Poly arG _ g) v =
  let ar  = max arF arG
      m   = tClassVarDeg g v
      d   = tLeadingCoeff g v
      gLo = stripLeadingV g v m
      r0  = findQR v m d gLo f
      r1  = tCoefGcd (tFactor1 r0)
  in (zeroPoly, mkPoly ar r1)

-- | Inner loop of pseudo-remainder. @v@ is the class variable, @m@ the
-- degree of the divisor in @v@, @d@ its leading coefficient (in @v@),
-- and @g_lo@ the divisor with its leading @v@-term stripped.
--
-- At each step we split @r = c_r * x_v^{d_r} + r_lo@ (which is /O(1)/
-- when @v@ is the root of @r@) and update
--
-- @
--   r' = d * r_lo - c_r * x_v^{d_r - m} * g_lo
-- @
--
-- This is the standard @d*r - c_r*x^{d_r-m}*g@ subtraction rewritten to
-- cancel the leading term up front.
findQR :: Int -> Int -> TPoly -> TPoly -> TPoly -> TPoly
findQR v m d gLo r0
  | m == 0    = r0
  | otherwise = go r0
  where
    -- Fast path when @v@ is the root of @r@ (the common Wu case); slow
    -- path (splitting via monomial walk) when @v@ is nested — this
    -- happens in 'reducePass' where a short poly's leading variable can
    -- be deeper than the target poly's root variable.
    go r = case r of
      TNode x _ _ _
        | x == v -> fastStep r
        | x >  v -> r                        -- v absent, deg_v = 0 < m
      _          -> nestedStep r

    fastStep r@(TNode _ dr cr rRest)
      | dr < m = r
      | otherwise =
          let dTimesRlo = scaleTPoly d rRest
              shifted   = shiftVarDeg v (dr - m) (scaleTPoly cr gLo)
              newR      = tCoefGcd (tsub dTimesRlo shifted)
          in go newR
    fastStep r = r

    nestedStep r =
      let dr = tClassVarDeg r v
      in if dr < m
         then r
         else
           let (lcR, rest) = splitOnV r v dr
               dTimesRest  = scaleTPoly d rest
               shifted     = shiftVarDeg v (dr - m) (scaleTPoly lcR gLo)
               newR        = tCoefGcd (tsub dTimesRest shifted)
           in go newR

-- | Split @p@ into @(lcV, rest)@ such that @p = lcV * x_v^dr + rest@,
-- where @lcV@ does not involve @x_v@ and @rest@ has degree in @v@
-- strictly less than @dr@. Used only on the nested (@v@ not at the
-- root) path of 'findQR'.
splitOnV :: TPoly -> Int -> Int -> (TPoly, TPoly)
splitOnV p v dr =
  let monos = collectMonos p
      (top, lo) = partitionMonos v dr monos
      lcV  = buildFromMonos [(IntMap.delete v m, c) | (m, c) <- top]
      rest = buildFromMonos lo
  in (lcV, rest)

-- | Collect every monomial as @(exponent-map, coefficient)@.
collectMonos :: TPoly -> [(IntMap Int, Integer)]
collectMonos = go IntMap.empty
  where
    go _   TZero           = []
    go ctx (TConst n)      = [(ctx, n)]
    go ctx (TNode x d c r) = go (IntMap.insert x d ctx) c ++ go ctx r

-- | Partition monomials into (v-deg == dr, v-deg < dr).
partitionMonos
  :: Int
  -> Int
  -> [(IntMap Int, Integer)]
  -> ([(IntMap Int, Integer)], [(IntMap Int, Integer)])
partitionMonos v dr = foldr step ([], [])
  where
    step m@(em, _) (top, lo) =
      if IntMap.findWithDefault 0 v em == dr
      then (m : top, lo)
      else (top, m : lo)

-- | Rebuild a 'TPoly' from a list of @(exponent-map, coefficient)@.
buildFromMonos :: [(IntMap Int, Integer)] -> TPoly
buildFromMonos = foldl' (\acc (em, c) -> tadd acc (monomialFromMap em c)) TZero

monomialFromMap :: IntMap Int -> Integer -> TPoly
monomialFromMap _ 0 = TZero
monomialFromMap em c =
  let pairs = [(i, e) | (i, e) <- IntMap.toAscList em, e > 0]
  in foldr (\(i, e) acc -> TNode i e acc TZero) (TConst c) pairs

-- | Multiply @p@ by @q@, with shortcuts for @q = ±1@.
scaleTPoly :: TPoly -> TPoly -> TPoly
scaleTPoly (TConst 1)    p = p
scaleTPoly (TConst (-1)) p = tneg p
scaleTPoly c             p = tmul c p

pseudoRemainders :: [Poly] -> Poly -> Int -> [Poly]
pseudoRemainders polys g v = map (\p -> snd (pseudoRemainder p g v)) polys

-- ---------------------------------------------------------------------------
-- Display
-- ---------------------------------------------------------------------------

-- | Collect monomials as @(exponent-map, coefficient)@ pairs for
-- rendering.
collectMonomials :: TPoly -> [(IntMap Int, Integer)]
collectMonomials = go IntMap.empty
  where
    go _   TZero         = []
    go ctx (TConst n)    = [(ctx, n)]
    go ctx (TNode x d c r) =
      go (IntMap.insert x d ctx) c ++ go ctx r

showPoly :: Poly -> String
showPoly (Poly _ _ p) =
  case collectMonomials p of
    []   -> "0"
    mons -> intercalate " + " (map showMono (sortBy (comparing (Down . fst)) mons))
  where
    showMono (exps, c) =
      let vars = concatMap showVarPow (IntMap.toAscList exps)
      in case (c, vars) of
           (1,  "") -> "1"
           (-1, "") -> "-1"
           (1,  _)  -> vars
           (-1, _)  -> "-" ++ vars
           (_,  "") -> show c
           _        -> show c ++ "*" ++ vars

    showVarPow (_, 0) = ""
    showVarPow (i, 1) = "x" ++ show i
    showVarPow (i, e) = "x" ++ show i ++ "^" ++ show e
