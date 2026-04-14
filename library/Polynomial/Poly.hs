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
  , topVarPoly
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

-- | Largest variable index present in @p@. Returns 'maxBound' for a
-- zero or bare-constant polynomial (so they sort to the end of a pool
-- ordered ascending by 'topVarPoly'). Matches Java's @PolyBasic.lv(m) =
-- m.x@, which is the topmost (= largest-index) variable of the tree.
-- The Haskell canonical form stores the /smallest/ variable at the
-- root, so we walk the entire tree taking the max of every node's
-- variable index.
topVarPoly :: Poly -> Int
topVarPoly (Poly _ _ p) = case tTopVar p of
                            Nothing -> maxBound
                            Just v  -> v

tTopVar :: TPoly -> Maybe Int
tTopVar TZero            = Nothing
tTopVar (TConst _)       = Nothing
tTopVar (TNode x _ c r)  =
  let m = case (tTopVar c, tTopVar r) of
            (Nothing, Nothing) -> x
            (Just a,  Nothing) -> max x a
            (Nothing, Just b)  -> max x b
            (Just a,  Just b)  -> max x (max a b)
  in Just m

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
    (m:ms) ->
      -- Java PolyBasic.get_factor1 (PolyBasic.java:634-654) builds the
      -- min-exponent map over every monomial of @m@ and then factors
      -- out those powers, /except/ for @m.x@ — the polynomial's class
      -- variable (= Java's @lv(m)@, the largest variable present).
      -- Skipping the class var is essential: if we factored it out,
      -- the resulting chain entry would no longer constrain the
      -- variable it's supposed to define.
      --
      -- We protect the /largest/ variable (matching Java's @m.x@). An
      -- earlier version deleted the TNode's literal @x@ field, which
      -- in Haskell's canonical form is the /smallest/ variable — the
      -- exact opposite — leaving clingy leading-coef factors like the
      -- @B.y@ in @B.y * (C.x - M.x)@ unstripped, which in turn made
      -- 'reducePass' over-reduce dependent hypotheses downstream.
      let mins0  = combineMins m ms
          topVar = case tTopVar p of
                     Just v  -> v
                     Nothing -> minBound  -- unreachable: p is non-const
          mins   = IntMap.delete topVar mins0
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

-- | If @p2@ divides @p1@ exactly, return the quotient; otherwise @p1@.
-- Mirrors Java @PolyBasic.factor_remove@ (PolyBasic.java:721): a cheap
-- "strip a shared multi-term factor" operation used by @prem@ to cancel
-- the dividend's and divisor's leading-in-@v@ coefficients against the
-- raw pseudoremainder so they don't inflate downstream monomials.
factorRemove :: Poly -> Poly -> Poly
factorRemove p1@(Poly ar1 _ b1) (Poly ar2 _ b2) =
  case tFactorRemove b1 b2 of
    q | q `sameT` b1 -> p1
      | otherwise    -> mkPoly (max ar1 ar2) q
  where
    -- Identity-compare to avoid re-wrapping when factor_remove is a no-op.
    sameT TZero TZero = True
    sameT a b         = a `seqEq` b
    seqEq TZero TZero                         = True
    seqEq (TConst a) (TConst b)               = a == b
    seqEq (TNode x1 d1 c1 r1) (TNode x2 d2 c2 r2) =
      x1 == x2 && d1 == d2 && seqEq c1 c2 && seqEq r1 r2
    seqEq _ _                                 = False

-- | Exact polynomial division. Returns 'Just q' when @p2@ divides @p1@
-- exactly, 'Nothing' otherwise. Mirrors Java @PolyBasic.div@
-- (PolyBasic.java:777).
exactDiv :: Poly -> Poly -> Maybe Poly
exactDiv (Poly ar1 _ b1) (Poly ar2 _ b2) =
  fmap (mkPoly (max ar1 ar2)) (tDiv b1 b2)

-- | Body of 'factorRemove'. Guards mirror Java:
--
--   * zero/length-1 divisor → bail
--   * dividend > 1000 terms → bail (cost control)
--   * same root var → bail (long division handled by @pseudoRemainder@)
--   * either operand a bare constant → bail
tFactorRemove :: TPoly -> TPoly -> TPoly
tFactorRemove p1 p2
  | tIsZero p1 || tIsZero p2       = p1
  | tIsConst p1 || tIsConst p2     = p1
  | tNumTerms p1 > 1000            = p1
  | tRootVar p1 == tRootVar p2     = p1
  | tNumTerms p2 <= 1              = p1
  | otherwise =
      let p1' = tCoefGcd (tFactor1 p1)
          p2' = tCoefGcd (tFactor1 p2)
      in case tDiv p1' p2' of
           Just q  -> q
           Nothing -> p1

tIsZero :: TPoly -> Bool
tIsZero TZero = True
tIsZero _     = False

tIsConst :: TPoly -> Bool
tIsConst (TConst _) = True
tIsConst _          = False

tRootVar :: TPoly -> Int
tRootVar (TNode x _ _ _) = x
tRootVar _               = maxBound

tNumTerms :: TPoly -> Int
tNumTerms TZero           = 0
tNumTerms (TConst _)      = 1
tNumTerms (TNode _ _ c r) = tNumTerms c + tNumTerms r

-- | Exact polynomial division at the 'TPoly' level.
--
-- Structural port of @PolyBasic.div@ with the convention flipped: Java's
-- @m.x > d.x@ (m has a variable bigger than any in d, so recurse on m's
-- per-monomial coefficients) corresponds to Haskell's @mx < dx@ since
-- the Haskell root is the /smallest/ variable and its @coef@ spans vars
-- strictly greater.
tDiv :: TPoly -> TPoly -> Maybe TPoly
tDiv TZero _           = Just TZero
tDiv _     TZero       = Nothing
tDiv (TConst a) (TConst b) =
  case a `quotRem` b of
    (q, 0) -> Just (if q == 0 then TZero else TConst q)
    _      -> Nothing
tDiv (TConst _) (TNode {}) = Nothing
tDiv m (TConst c) = tDivByInt c m
tDiv m@(TNode mx _ _ _) d@(TNode dx dd dCoef dRest)
  | mx >  dx = Nothing
  | mx <  dx = tDivViaCoefs mx m d
  | otherwise = tDivSameVar mx dd dCoef dRest m

-- | Divide every integer coefficient of @p@ by @c@, failing if any
-- remainder is non-zero.
tDivByInt :: Integer -> TPoly -> Maybe TPoly
tDivByInt _ TZero      = Just TZero
tDivByInt c (TConst n) =
  case n `quotRem` c of
    (q, 0) -> Just (if q == 0 then TZero else TConst q)
    _      -> Nothing
tDivByInt c (TNode x d coef r) = do
  coef' <- tDivByInt c coef
  r'    <- tDivByInt c r
  pure (tNode x d coef' r')

-- | Handles the @mx < dx@ case: m is a polynomial in variable @mx@ whose
-- coefficients live in the vars-@>mx@ namespace, which is exactly where
-- @d@ lives. Walk m's same-root chain and divide each coefficient by @d@.
tDivViaCoefs :: Int -> TPoly -> TPoly -> Maybe TPoly
tDivViaCoefs mx p0 d = go p0
  where
    go (TNode x deg c r) | x == mx = do
      qC   <- tDiv c d
      rest <- go r
      pure (tadd (shiftVarDeg mx deg qC) rest)
    go t = tDiv t d

-- | Handles the @mx == dx == v@ case: classical univariate long division
-- in @v@ with coefficients in the vars-@>v@ namespace.
tDivSameVar :: Int -> Int -> TPoly -> TPoly -> TPoly -> Maybe TPoly
tDivSameVar v dDeg dCoef dRest = go
  where
    go TZero      = Just TZero
    go (TConst _) = Nothing
    go (TNode mx mDeg mc mr)
      | mx /= v    = Nothing
      | mDeg < dDeg = Nothing
      | otherwise = do
          q <- tDiv mc dCoef
          let delta = mDeg - dDeg
              q1    = if delta == 0 then q else shiftVarDeg v delta q
              newM  = tsub mr (tmul q1 dRest)
          rest <- go newM
          pure (tadd q1 rest)

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
      -- gLo = g with its leading-in-v term removed. We compute this as
      -- @g - d*x_v^m@ rather than via 'stripLeadingV' because @v@ may be
      -- nested below the root of @g@ (root = smallest variable in the
      -- Haskell mirror convention), and 'stripLeadingV' only handles the
      -- root case. See historical bug: with g = x1*x9 - x0*x10 and v=x1,
      -- the old code returned gLo = g unchanged, causing 'findQR' to
      -- loop forever multiplying the leading-in-v coefficient by -d on
      -- every step.
      gLo = tsub g (shiftVarDeg v m d)
      r0  = findQR v m d gLo f
      -- factor_remove pass (Java PolyBasic.java:381-382): after prem1
      -- multiplies the dividend by the divisor's leading coef (and vice
      -- versa via the inner loop), cancel those factors back out by
      -- exact polynomial division. Skipped when the reduction was a
      -- no-op (m == 0 or f had no v), because then r0 == f and the
      -- "divide by fLead" would corrupt.
      fM   = tClassVarDeg f v
      r2   = if m > 0 && fM > 0
               then let fLead = tLeadingCoeff f v
                        r1a   = tFactorRemove r0 fLead
                    in tFactorRemove r1a d
               else r0
      r3   = tCoefGcd (tFactor1 r2)
  in (zeroPoly, mkPoly ar r3)

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
