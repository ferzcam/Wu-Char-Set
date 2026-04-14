{-# LANGUAGE BangPatterns #-}
-- | Wu's characteristic set construction.
--
-- Structural port of @maths/CharSet.java:charset@. The pool of pending
-- polynomials is kept sorted by 'rootVar' (smallest variable index
-- present) — Haskell's inverted-root equivalent of Java's
-- @lv(m) = m.x@, which in Java's convention is the /largest/ variable
-- of the tree. Concretely:
--
--   * We pop the first /group/ — the maximal prefix of the pool sharing
--     the same 'rootVar' @vra@.
--   * A group of one is emitted directly to the chain.
--   * Otherwise we pick the minimum-degree-in-@vra@ element as divisor
--     (@PolyBasic.getMinV@) and pseudo-remainder every other group
--     member against it. Each reduced remainder has @rootVar > vra@ by
--     construction (prem eliminated @vra@), so it folds back into the
--     pool at a later position.
--   * The divisor becomes the group's chain entry, and we recurse on
--     the updated pool.
--
-- A trailing 'reducePass' + 'cfinished' check guards against chains
-- that still share a root var across entries (Java re-invokes
-- @charset(tp)@ in the same situation).
module Polynomial.Wu
  ( charSet
  , charSetIO
  , rootVar
  , showPolyShort
  ) where

import Data.List (foldl', partition, sortOn)
import Polynomial.Poly
import Polynomial.Prelude (simplifyPolinomial)

-- ---------------------------------------------------------------------------
-- Root-variable helpers
-- ---------------------------------------------------------------------------

-- | Smallest variable index present in @p@, or @maxBound@ if @p@ is
-- zero or a nonzero constant. Matches Java's @lv(m) = m.x@ under
-- Haskell's inverted root convention: in Java @m.x@ is the /largest/
-- variable of the tree, in Haskell it is the /smallest/. Wu's elimination
-- direction is flipped accordingly (smallest-first here, largest-first
-- in Java), so the chain produced is structurally equivalent.
--
-- A polynomial with no variables is parked at @maxBound@ so it sorts to
-- the end of the pool and is naturally skipped by the main loop's
-- @rootVar p >= numElim@ guard.
rootVar :: Poly -> Int
rootVar p
  | isZero p  = maxBound
  | otherwise =
      let ar = polyArity p
          go !i
            | i >= ar        = maxBound
            | varInPoly p i  = i
            | otherwise      = go (i + 1)
      in go 0

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
-- @charSet numElim hyps _acc _v0@ triangulates @hyps@ with respect to
-- variables @[0 .. numElim - 1]@. Variables @>= numElim@ are free
-- U-parameters and never become chain entries. The legacy accumulator
-- and start-var arguments are ignored.
--
-- Structurally the loop is 'runLoop', which pops one 'rootVar' group at
-- a time and emits exactly one chain entry per group — so its output
-- is 'cfinished' by construction and needs neither a post-'reducePass'
-- (which could rewrite a chain entry to a different 'rootVar' and break
-- the invariant) nor Java's @cfinished@ recursion.
charSet :: Int -> [Poly] -> [Poly] -> Int -> [Poly]
charSet numElim p0 _ _ =
  let initialPool =
        sortPool . reducePass . sortPool . map simplify . filter (not . isZero) $ p0
  in map simplify (runLoop initialPool)
  where
    runLoop :: [Poly] -> [Poly]
    runLoop []   = []
    runLoop pool =
      let (grp, rest) = spanGroup pool
          v           = rootVar (head grp)
      in if v >= numElim
           then []  -- everything left lives in free U-params
           else case grp of
                  [single] -> single : runLoop rest
                  _        ->
                    let (divor, reinserts) = reduceGroup v grp
                        newPool = foldl' (flip insertByRoot) rest reinserts
                    in divor : runLoop newPool

-- | Within a group sharing @rootVar = v@, pick a min-degree-in-@v@
-- divisor and pseudo-remainder every other group member against it.
-- Remainders with @rootVar > v@ are returned as reinserts; a defensive
-- inner loop handles the pathological "stayed" case where prem returns
-- a result still rooted at @v@ (shouldn't happen with a correct
-- 'pseudoRemainder', but Java's CharSet.java:72-100 defends against it).
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
          (fwd, stayed) = partition (\r -> rootVar r > v) rems
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
charSetIO logIO numElim p0 _ _ = do
  logIO $ "[charSet] entry on " ++ show (length p0) ++ " polys"
  let !simplified = map simplify (filter (not . isZero) p0)
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
  pure (map simplify chain)
  where
    runLoopIO :: [Poly] -> IO [Poly]
    runLoopIO []   = pure []
    runLoopIO pool = do
      let (grp, rest) = spanGroup pool
          v           = rootVar (head grp)
      if v >= numElim
        then do
          logIO $ "[charSet] done: remaining pool head rootVar=" ++ show v
                 ++ " >= numElim=" ++ show numElim
          pure []
        else case grp of
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
