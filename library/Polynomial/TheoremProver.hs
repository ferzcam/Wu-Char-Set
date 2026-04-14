{-# LANGUAGE BangPatterns #-}
-- | Theorem prover driver built on Wu's characteristic set method.
--
-- Mirrors the Java reference pipeline in @src/main/java/maths/CharSet.java@:
-- no separate linear pre-elimination phase — 'charSet' handles linear
-- divisors internally via its 'existOneDegPoly' branch, consuming each
-- divisor exactly once (Java's @getMinV@ within a leading-variable group).
module Polynomial.TheoremProver
  ( theoremProver
  , theoremProverVerbose
  , theoremProverIO
  , remWithChain
  , remWithChainIO
  ) where

import Polynomial.Poly
import Polynomial.Wu (charSet, charSetIO, showPolyShort)

-- | Test a geometric theorem.
--
-- Inputs:
--
--   * @numElim@: number of dependent (X) variables to eliminate;
--   * @hip@: hypothesis polynomials;
--   * @g@: conclusion polynomial.
--
-- Output: successive pseudo-remainders of @g@ w.r.t. the ascending Wu
-- chain. The theorem holds iff the last remainder is zero.
theoremProver :: Int -> [Poly] -> Poly -> [Poly]
theoremProver numElim hip g = remWithChain wuChain g 0
  where
    wuChain = charSet numElim hip [] 0

-- | Like 'theoremProver', but also returns the characteristic set.
-- The triple fields kept for call-site compatibility: the last two
-- @[Int]@/@Poly@ slots were a pre-elimination diagnostic that no longer
-- applies (empty list / unchanged conclusion).
theoremProverVerbose
  :: Int -> [Poly] -> Poly
  -> ([Poly], [Poly], [Poly], Poly, [Int])
     -- ^ @(wuChain, remainders, hyps, conclusion, [])@
theoremProverVerbose numElim hip g =
  (wuChain, remWithChain wuChain g 0, hip, g, [])
  where
    wuChain = charSet numElim hip [] 0

-- | Pseudo-remainder of a polynomial with respect to an ascending chain.
remWithChain :: [Poly] -> Poly -> Int -> [Poly]
remWithChain [] _ _ = []
remWithChain (c:cs) pol v = r : remWithChain cs r (v + 1)
  where
    r = snd (pseudoRemainder pol c v)

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
  wuChain <- charSetIO logIO numElim hip [] 0
  logIO $ "[remChain] reducing conclusion through "
         ++ show (length wuChain) ++ "-poly chain"
  rems <- remWithChainIO logIO wuChain g 0
  logIO $ "[remChain] done, " ++ show (length rems) ++ " remainders"
  pure (wuChain, rems, hip, g, [])

-- | IO variant of 'remWithChain' that logs each pseudoremainder step.
remWithChainIO :: (String -> IO ()) -> [Poly] -> Poly -> Int -> IO [Poly]
remWithChainIO _     []     _   _ = pure []
remWithChainIO logIO (c:cs) pol v = do
  let !(_, r) = pseudoRemainder pol c v
  logIO $ "  R(v=" ++ show v ++ ") via h" ++ show v
         ++ ": deg=" ++ show (classVarDeg r v)
         ++ " terms=" ++ show (numTerms r)
         ++ " " ++ showPolyShort r
  rest <- remWithChainIO logIO cs r (v + 1)
  pure (r : rest)
