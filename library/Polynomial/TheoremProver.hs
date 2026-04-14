{-# LANGUAGE BangPatterns #-}
-- | Theorem prover driver built on Wu's characteristic set method.
--
-- Mirrors the Java reference pipeline in
-- @src/main/java/wprover/DrawProcess.java:charsetAndAddPoly@, which runs
-- 'Polynomial.Wu.charSet' /incrementally/ — once per hypothesis, feeding
-- the previously-built chain back as input for the next step. This is
-- critical for performance: because each call's pool is the existing
-- chain plus one new hypothesis, the initial 'reducePass' inside
-- 'charSet' always has every previously-discovered ≤2-term chain entry
-- available as a reducer, so each new hypothesis collapses against the
-- accumulated relations /before/ it ever reaches the main loop. A
-- single batch call cannot replicate this, because
-- 'Polynomial.Wu.runLoop' never re-runs reducePass as new short entries
-- get emitted — pending pool polys are only reduced by relations that
-- were short at entry time. See @problem.txt@ in the Java repo for the
-- reference trace.
module Polynomial.TheoremProver
  ( theoremProver
  , theoremProverVerbose
  , theoremProverIO
  , buildChain
  , buildChainIO
  , remWithChain
  , remWithChainIO
  ) where

import Data.List (foldl', sortBy)
import Data.Ord (comparing, Down(..))
import Polynomial.Poly
import Polynomial.Wu (charSet, charSetIO, rootVar, showPolyShort)

-- | Test a geometric theorem.
--
-- Inputs:
--
--   * @numElim@: number of dependent (X) variables to eliminate;
--   * @hip@: hypothesis polynomials, in geometric construction order
--     (earliest-constructed point's constraints first);
--   * @g@: conclusion polynomial.
--
-- Output: successive pseudo-remainders of @g@ w.r.t. the ascending Wu
-- chain. The theorem holds iff the last remainder is zero.
theoremProver :: Int -> [Poly] -> Poly -> [Poly]
theoremProver numElim hip g = remWithChain wuChain g
  where
    wuChain = buildChain numElim hip

-- | Like 'theoremProver', but also returns the characteristic set.
-- The triple fields kept for call-site compatibility: the last two
-- @[Int]@/@Poly@ slots were a pre-elimination diagnostic that no longer
-- applies (empty list / unchanged conclusion).
theoremProverVerbose
  :: Int -> [Poly] -> Poly
  -> ([Poly], [Poly], [Poly], Poly, [Int])
     -- ^ @(wuChain, remainders, hyps, conclusion, [])@
theoremProverVerbose numElim hip g =
  (wuChain, remWithChain wuChain g, hip, g, [])
  where
    wuChain = buildChain numElim hip

-- | Incremental 'charSet' driver. Folds over the hypothesis list in
-- order, re-invoking 'charSet' after each addition so that the chain
-- built so far is available as a pool of reducers for the next
-- hypothesis. This mirrors Java's @DrawProcess.charsetAndAddPoly@ path,
-- which is the whole reason Java avoids the mid-chain term explosion
-- the old batch driver was hitting on @imo_2000_p1@.
--
-- The new hypothesis is appended to the end of the existing chain
-- before the call; 'charSet' re-sorts and reduces the combined pool
-- itself.
buildChain :: Int -> [Poly] -> [Poly]
buildChain numElim = foldl' step []
  where
    step !chain h = charSet numElim (chain ++ [h]) [] 0

-- | Pseudo-remainder of a polynomial with respect to an ascending
-- chain. Each chain entry @c@ carries its own elimination variable
-- @rootVar c@ (the largest variable it contains, mirroring Java
-- @lv(c)@); we reduce @pol@ against @c@ in that variable and pass the
-- result on. The iteration index is /not/ used as the variable —
-- with interleaved free\/dep indexing from the Algebraizer, chain
-- positions are not contiguous, so indexing by position silently
-- produced no-ops in prior versions.
remWithChain :: [Poly] -> Poly -> [Poly]
remWithChain chain pol = go (sortBy (comparing (Down . rootVar)) chain) pol
  where
    go []     _ = []
    go (c:cs) p = r : go cs r
      where
        v = rootVar c
        r = snd (pseudoRemainder p c v)

-- ---------------------------------------------------------------------------
-- IO-logging variants (for Main --verbose; stream progress incrementally
-- so a timeout still yields a useful trace).
-- ---------------------------------------------------------------------------

-- | IO variant of 'theoremProver' that streams 'charSet' and the
-- pseudoremainder chain to the provided logger.
theoremProverIO
  :: (String -> IO ())
  -> Int -> [Poly] -> Poly
  -> IO ([Poly], [Poly], [Poly], Poly, [Int])
theoremProverIO logIO numElim hip g = do
  logIO $ "[theoremProver] " ++ show (length hip) ++ " hyps, "
         ++ show numElim ++ " dep vars"
  logIO $ "  conclusion: " ++ showPolyShort g
  wuChain <- buildChainIO logIO numElim hip
  logIO $ "[remChain] reducing conclusion through "
         ++ show (length wuChain) ++ "-poly chain"
  rems <- remWithChainIO logIO wuChain g
  logIO $ "[remChain] done, " ++ show (length rems) ++ " remainders"
  pure (wuChain, rems, hip, g, [])

-- | IO variant of 'buildChain'. Streams each incremental 'charSetIO'
-- invocation so a timeout in the middle of the fold still produces a
-- useful trace.
buildChainIO :: (String -> IO ()) -> Int -> [Poly] -> IO [Poly]
buildChainIO logIO numElim = go (0 :: Int) []
  where
    go _ chain []     = pure chain
    go k chain (h:hs) = do
      logIO $ "[buildChain] step " ++ show k
             ++ ": chain size " ++ show (length chain)
             ++ " + hyp rootVar=" ++ show (rootVar h)
             ++ " terms=" ++ show (numTerms h)
             ++ " " ++ showPolyShort h
      chain' <- charSetIO logIO numElim (chain ++ [h]) [] 0
      go (k + 1) chain' hs

-- | IO variant of 'remWithChain' that logs each pseudoremainder step.
-- Chain is iterated /descending/ by 'rootVar' to mirror Java's
-- @PanelWu.div@ (which prepends into @vt@ so the iteration starts at
-- the largest-lv entry). Iterating ascending silently re-introduces
-- small-index vars via the lead coefficients of later chain entries.
remWithChainIO :: (String -> IO ()) -> [Poly] -> Poly -> IO [Poly]
remWithChainIO logIO chain pol =
  go (sortBy (comparing (Down . rootVar)) chain) pol
  where
    go []     _ = pure []
    go (c:cs) p = do
      let v = rootVar c
          !(_, r) = pseudoRemainder p c v
      logIO $ "  R(v=" ++ show v ++ ")"
             ++ ": deg=" ++ show (classVarDeg r v)
             ++ " terms=" ++ show (numTerms r)
             ++ " " ++ showPolyShort r
      rest <- go cs r
      pure (r : rest)
