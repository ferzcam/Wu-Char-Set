{-# LANGUAGE BangPatterns #-}
-- | Wu's characteristic set construction.
--
-- Structural port of @maths/CharSet.java:charset@. The pool of pending
-- polynomials is kept sorted ascending by 'rootVar', which here is the
-- /largest/ variable index present in a polynomial — mirroring Java's
-- @PolyBasic.lv(m) = m.x@. Concretely:
--
--   * We pop the first /group/ — the maximal prefix of the pool sharing
--     the same 'rootVar' @vra@.
--   * A group of one is emitted directly to the chain.
--   * Otherwise we pick the minimum-positive-degree-in-@vra@ element as
--     divisor (@PolyBasic.getMinV@) and pseudo-remainder every other
--     group member against it. Each reduced remainder has @rootVar <
--     vra@ by construction (prem eliminated the topmost variable), so
--     it folds back into the pool at an earlier position.
--   * The divisor becomes the group's chain entry, and we recurse on
--     the updated pool.
--
-- Picking the /largest/ variable as the group key is the whole reason
-- this version avoids the mid-chain term explosion the old
-- smallest-var-first driver was hitting on @imo_2000_p1@. Eliminating
-- the latest-constructed point's coordinate first exploits the
-- typically-linear dependence of each new constraint on the point it
-- defines, keeping intermediate remainders small.
module Polynomial.Wu
  ( charSet
  , charSetIO
  , rootVar
  , showPolyShort
  ) where

import Data.List (foldl', partition, sortOn)
import qualified Data.Set as S
import Polynomial.Poly
import Polynomial.Prelude (simplifyPolinomial)

-- ---------------------------------------------------------------------------
-- Root-variable helpers
-- ---------------------------------------------------------------------------

-- | Largest variable index present in @p@, or @maxBound@ if @p@ is
-- zero or a nonzero constant. Matches Java's @lv(m) = m.x@: the
-- topmost (highest-indexed) variable of the polynomial, which is the
-- one eliminated by the group this poly belongs to in Wu's main loop.
--
-- A polynomial with no variables is parked at @maxBound@ so it sorts
-- to the end of the pool and is emitted as a trivial singleton chain
-- entry.
rootVar :: Poly -> Int
rootVar = topVarPoly

-- ---------------------------------------------------------------------------
-- Pool operations
-- ---------------------------------------------------------------------------

-- | Sort a pool ascending by 'rootVar'. Stable.
sortPool :: [Poly] -> [Poly]
sortPool = sortOn rootVar

-- | Insert @p@ into a pool kept sorted ascending by 'rootVar'. New
-- polys go at the end of their root-var bucket (FIFO within a bucket).
insertByRoot :: Poly -> [Poly] -> [Poly]
insertByRoot p pool =
  let v = rootVar p
      (before, after) = span (\q -> rootVar q <= v) pool
  in before ++ [p] ++ after

-- | Pick the polynomial with the smallest positive degree in variable
-- @v@. Mirrors @PolyBasic.getMinV@. Among ties, picks the first seen.
-- Precondition: at least one element has a positive degree in @v@
-- (always true for a group built by 'spanGroup').
pickMinDegInV :: Int -> [Poly] -> Poly
pickMinDegInV v = foldl1 choose
  where
    choose a b
      | db > 0 && (da == 0 || db < da) = b
      | otherwise                      = a
      where
        da = classVarDeg a v
        db = classVarDeg b v

-- | Split the pool into (leading group sharing @rootVar@, rest). Assumes
-- the pool is sorted by 'rootVar' ascending.
spanGroup :: [Poly] -> ([Poly], [Poly])
spanGroup []       = ([], [])
spanGroup (p:ps)   =
  let v = rootVar p
      (grp, rest) = span (\q -> rootVar q == v) ps
  in (p : grp, rest)

-- ---------------------------------------------------------------------------
-- Main loop
-- ---------------------------------------------------------------------------

-- | Characteristic-set construction.
--
-- @charSet _numElim hyps _acc _v0@ triangulates @hyps@. The legacy
-- @numElim@ / accumulator / start-var arguments are kept for
-- call-site compatibility but are no longer needed: with @Tokenizer@
-- assigning free vars the lowest class indices and deps above them,
-- an all-free poly (if one is ever produced) has @rootVar < numFree@
-- and simply gets emitted as a trivial singleton chain entry that is
-- a no-op for 'remWithChain'.
--
-- Structurally the loop is 'runLoop', which pops one 'rootVar' group
-- at a time and emits exactly one chain entry per group.
charSet :: Int -> [Poly] -> [Poly] -> Int -> [Poly]
charSet _numElim p0 _ _ = map simplify (go p0)
  where
    go :: [Poly] -> [Poly]
    go input =
      let pool = sortPool . reducePass . sortPool
               . map simplify . filter (not . isZero) $ input
          chain = runLoop pool
      in if cfinished chain then chain else go chain

    runLoop :: [Poly] -> [Poly]
    runLoop []   = []
    runLoop pool =
      let (grp, rest) = spanGroup pool
          v           = rootVar (head grp)
      in case grp of
           [single] -> single : runLoop rest
           _        ->
             let (divor, reinserts) = reduceGroup v grp
                 newPool = foldl' (flip insertByRoot) rest reinserts
             in divor : runLoop newPool

-- | A chain is \"c-finished\" iff all its entries have distinct
-- 'rootVar' — i.e. it is a proper triangular set. Mirrors Java
-- @CharSet.cfinished@ semantically; Java's version only checks
-- /adjacent/ entries because its chain is kept sorted via @ppush@,
-- so adjacency implies global distinctness. Our 'runLoop' emits in
-- processing order, which can put duplicates at non-adjacent
-- positions (when a reduceGroup reinsert lands at a 'rootVar' that
-- was already processed earlier), so we must scan all pairs — a set
-- of 'rootVar's with size equal to the list length.
--
-- When this is false, 'charSet' re-runs the pipeline on that chain
-- until it triangulates cleanly.
cfinished :: [Poly] -> Bool
cfinished xs =
  let rs = map rootVar xs
  in length rs == length (nubOrd rs)
  where
    nubOrd = go S.empty
      where
        go _    []     = []
        go seen (y:ys)
          | S.member y seen = go seen ys
          | otherwise       = y : go (S.insert y seen) ys

-- | Within a group sharing @rootVar = v@ (largest variable of the
-- poly), pick a min-positive-degree-in-@v@ divisor and
-- pseudo-remainder every other group member against it. Prem
-- eliminates @v@ from each remainder, so its new 'rootVar' (largest
-- var) is strictly less than @v@ and it folds earlier in the pool.
-- The defensive inner loop handles the "stayed" case where prem
-- returns a result whose largest var is still @v@ (lower degree in
-- @v@) — this is expected whenever the divisor did not have minimum
-- positive degree, matching Java's CharSet.java:140-190 re-loop on
-- @divor + stayed@.
reduceGroup :: Int -> [Poly] -> (Poly, [Poly])
reduceGroup v = go []
  where
    go :: [Poly] -> [Poly] -> (Poly, [Poly])
    go acc grp =
      let divor  = pickMinDegInV v grp
          others = filter (/= divor) grp
          rems   = [ r
                   | p <- others
                   , let (_, r0) = pseudoRemainder p divor v
                   , let r = simplifyPolinomial r0
                   , not (isZero r)
                   ]
          (fwd, stayed) = partition (\r -> rootVar r < v) rems
      in if null stayed
           then (divor, acc ++ fwd)
           else go (acc ++ fwd) (divor : stayed)

-- ---------------------------------------------------------------------------
-- Reduction pass — Java PolyBasic / CharSet.reduce{1}
-- ---------------------------------------------------------------------------

-- | Directional reduction pass. Mirrors Java's @CharSet.reduce1(pp)@ applied
-- to a list already in ascending-'rootVar' order:
--
-- For each index @i@ in the list (processed from last to first), if the poly
-- at @i@ is \"short\" (nonzero, ≤ 2 terms, has a variable), pseudo-remainder
-- every /successor/ (position > @i@) by it. Predecessors are left alone.
--
-- Java's @reduce1@ is implemented as @reverse; reduce; reverse@ where the
-- inner @reduce@ (CharSet.java:137-151) walks @p1@ forward and, for each
-- short @p1@, reduces all /earlier/ entries. Reversing before/after flips
-- \"earlier\" to \"later\", giving the successor-reduction semantics used
-- here.
--
-- The previous symmetric implementation reduced predecessors /and/
-- successors, which over-reduced and perturbed the chain produced by
-- 'runLoop' relative to the Java reference trace.
reducePass :: [Poly] -> [Poly]
reducePass ps0 =
  let n = length ps0
  in foldr step ps0 [0 .. n - 1]
  where
    step :: Int -> [Poly] -> [Poly]
    step i acc =
      let (before, rest) = splitAt i acc
      in case rest of
           [] -> acc
           (short : after)
             | isZero short || numTerms short > 2 || rootVar short == maxBound
                 -> acc
             | otherwise ->
                 let v      = rootVar short
                     after' = map (reduceOne short v) after
                 in before ++ short : after'

    reduceOne :: Poly -> Int -> Poly -> Poly
    reduceOne short v p
      | not (varInPoly p v) = p
      | otherwise =
          let r = simplifyPolinomial (snd (pseudoRemainder p short v))
          in if isZero r then p else r

-- ---------------------------------------------------------------------------
-- Display
-- ---------------------------------------------------------------------------

-- | Compact polynomial printer: full form if small, otherwise just a term
-- count. Used by the verbose IO variants so a 10k-term intermediate does
-- not blow up the terminal.
showPolyShort :: Poly -> String
showPolyShort p
  | nt <= 40  = show p
  | otherwise = "<" ++ show nt ++ " terms>"
  where nt = numTerms p

-- ---------------------------------------------------------------------------
-- IO-logging variant (used by Main --verbose to stream incremental output
-- so intermediate state can be compared against a Java reference trace and
-- still yields a useful trace on timeout).
-- ---------------------------------------------------------------------------

-- | IO variant of 'charSet' that logs every group processed.
charSetIO :: (String -> IO ()) -> Int -> [Poly] -> [Poly] -> Int -> IO [Poly]
charSetIO logIO _numElim p0 _ _ = fmap (map simplify) (goIO 0 p0)
  where
    goIO :: Int -> [Poly] -> IO [Poly]
    goIO depth input = do
      logIO $ "[charSet] entry on " ++ show (length input) ++ " polys"
             ++ (if depth > 0 then "  (cfinished recursion depth "
                                    ++ show depth ++ ")"
                              else "")
      let !simplified = map simplify (filter (not . isZero) input)
          !sorted1    = sortPool simplified
          !reduced    = reducePass sorted1
          !pool0      = sortPool reduced
      logIO $ "[charSet] after entry simplify+sort+reducePass+resort: "
             ++ show (length pool0) ++ " polys"
      mapM_ (\(i, pp) -> logIO $ "  p" ++ show i
                                ++ " rootVar=" ++ show (rootVar pp)
                                ++ " terms="   ++ show (numTerms pp)
                                ++ " "          ++ showPolyShort pp)
            (zip [(0::Int)..] pool0)
      chain <- runLoopIO pool0
      logIO $ "[charSet] chain length = " ++ show (length chain)
      mapM_ (\(i, pp) -> logIO $ "  h" ++ show i
                                ++ " rootVar=" ++ show (rootVar pp)
                                ++ " terms="   ++ show (numTerms pp)
                                ++ " "          ++ showPolyShort pp)
            (zip [(0::Int)..] chain)
      if cfinished chain
        then pure chain
        else do
          logIO $ "[charSet] not cfinished — duplicate rootVar detected, recursing"
          goIO (depth + 1) chain
    runLoopIO :: [Poly] -> IO [Poly]
    runLoopIO []   = pure []
    runLoopIO pool = do
      let (grp, rest) = spanGroup pool
          v           = rootVar (head grp)
      case grp of
        [single] -> do
          logIO $ "[charSet] v=" ++ show v ++ " single poly -> chain: "
                 ++ showPolyShort single
          (single :) <$> runLoopIO rest
        _ -> do
          logIO $ "[charSet] v=" ++ show v
                 ++ " group size=" ++ show (length grp)
          let (divor, reinserts) = reduceGroup v grp
          logIO $ "  divor terms=" ++ show (numTerms divor)
                 ++ " deg_v=" ++ show (classVarDeg divor v)
                 ++ " " ++ showPolyShort divor
          mapM_ (\(i, r) -> logIO $ "    reinsert[" ++ show i ++ "] rootVar="
                                   ++ show (rootVar r)
                                   ++ " terms=" ++ show (numTerms r)
                                   ++ " " ++ showPolyShort r)
                (zip [(0::Int)..] reinserts)
          let newPool = foldl' (flip insertByRoot) rest reinserts
          (divor :) <$> runLoopIO newPool
