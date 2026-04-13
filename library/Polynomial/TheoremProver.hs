-- | Theorem prover driver built on Wu's characteristic set method.
module Polynomial.TheoremProver
  ( theoremProver
  , theoremProverVerbose
  , remWithChain
  , preElimLinear
  ) where

import Data.Maybe (listToMaybe)
import Polynomial.Poly
import Polynomial.Prelude (simplifyPolinomial)
import Polynomial.Wu (charSet)

-- | Pre-eliminate linear constraints before the main Wu triangularization.
--
-- Scans all X-variables (@0..numElim-1@) for hypotheses that are degree 1
-- in that variable, then uses 'pseudoRemainder' to eliminate that variable
-- from all other hypotheses and from the conclusion. Iterates until no
-- more linear eliminations are possible.
--
-- This improves on 'charSet''s built-in linear handling because:
--
--   (1) it processes easy variables first regardless of position ordering,
--   (2) it also simplifies the conclusion, reducing work in 'remWithChain'.
--
-- Returns @(simplifiedHyps, simplifiedConclusion, eliminatedVarIndices)@.
preElimLinear :: Int -> [Poly] -> Poly -> ([Poly], Poly, [Int])
preElimLinear numElim hyps0 concl0 = go numElim hyps0 concl0 []
  where
    go 0 hs c elims = (hs, c, reverse elims)  -- safety bound
    go fuel hs c elims = case findLinearHyp numElim hs of
      Nothing -> (hs, c, reverse elims)
      Just (linP, v) ->
        let reduce p
              | p == linP          = p  -- keep the linear hyp itself intact
              | not (varInPoly p v) = p -- skip if p doesn't involve v
              | otherwise = simplifyPolinomial (snd (pseudoRemainder p linP v))
            hs' = filter (not . isZero) (map reduce hs)
            c'  = if varInPoly c v
                    then simplifyPolinomial (snd (pseudoRemainder c linP v))
                    else c
        in go (fuel - 1) hs' c' (v : elims)

-- | Find a hypothesis that is degree 1 in some X-variable.
-- Prefers variables that appear in fewer hypotheses (cheaper elimination).
findLinearHyp :: Int -> [Poly] -> Maybe (Poly, Int)
findLinearHyp numElim hyps = listToMaybe candidates
  where
    candidates =
      [ (h, v)
      | v <- [0 .. numElim - 1]
      , let polysWithVar = filter (`varInPoly` v) hyps
      , length polysWithVar >= 2
      , h <- take 1 [p | p <- polysWithVar, classVarDeg p v == 1]
      ]

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
theoremProver numElim hip g = remWithChain wuChain g' 0
  where
    (hip', g', _) = preElimLinear numElim hip g
    wuChain = charSet numElim hip' [] 0

-- | Like 'theoremProver', but also returns the characteristic set and
-- pre-elimination diagnostics.
theoremProverVerbose
  :: Int -> [Poly] -> Poly
  -> ([Poly], [Poly], [Poly], Poly, [Int])
     -- ^ @(wuChain, remainders, preElimHyps, preElimConcl, eliminatedVars)@
theoremProverVerbose numElim hip g =
  (wuChain, remWithChain wuChain g' 0, hip', g', elims)
  where
    (hip', g', elims) = preElimLinear numElim hip g
    wuChain = charSet numElim hip' [] 0

-- | Pseudo-remainder of a polynomial with respect to an ascending chain.
remWithChain :: [Poly] -> Poly -> Int -> [Poly]
remWithChain [] _ _ = []
remWithChain (c:cs) pol v = r : remWithChain cs r (v + 1)
  where
    r = snd (pseudoRemainder pol c v)
