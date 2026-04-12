{-#LANGUAGE FlexibleContexts#-}

module Polynomial.TheoremProver where

import Algebra.Ring.Polynomial
import Data.Type.Ordinal
import Polynomial.Prelude
import Polynomial.Wu
import Data.List
import Data.Maybe (listToMaybe)
import Debug.Trace
import GHC.TypeLits
-- | This algorithm was taken from the book "Ideals, Varieties and Algorithms" 4th ed.


-- | Pre-eliminate linear constraints before the main Wu triangularization.
-- Scans all X-variables (0..numElim-1) for hypotheses that are degree 1 in
-- that variable, then uses pseudoRemainder to eliminate that variable from
-- all other hypotheses and from the conclusion.  Iterates until no more
-- linear eliminations are possible.
--
-- This improves on charSet's built-in linear handling because:
-- (1) it processes easy variables first regardless of position ordering, and
-- (2) it also simplifies the conclusion, reducing work in remWithChain.
-- | Returns (simplifiedHyps, simplifiedConclusion, eliminatedVarIndices).
preElimLinear :: (IsMonomialOrder n Grevlex, KnownNat n)
    => Int -> [Polynomial' n] -> Polynomial' n
    -> ([Polynomial' n], Polynomial' n, [Int])
preElimLinear numElim hyps concl = go numElim hyps concl []
  where
    go 0 hs c elims = (hs, c, reverse elims)  -- safety bound
    go fuel hs c elims = case findLinearHyp numElim hs of
      Nothing            -> (hs, c, reverse elims)
      Just (linP, var) ->
        let reduce p
              | p == linP            = p  -- keep the linear hyp itself intact
              | not (varInPoly p var) = p -- skip if p doesn't involve var
              | otherwise = simplifyPolinomial (snd (pseudoRemainder p linP var))
            hs' = filter (/= 0) (map reduce hs)  -- drop redundant constraints
            c'  = if varInPoly c var
                  then simplifyPolinomial (snd (pseudoRemainder c linP var))
                  else c
        in go (fuel - 1) hs' c' (var : elims)

-- | Find a hypothesis that is degree 1 in some X-variable.
-- Prefers variables that appear in fewer hypotheses (cheaper elimination).
findLinearHyp :: (IsMonomialOrder n Grevlex, KnownNat n)
    => Int -> [Polynomial' n] -> Maybe (Polynomial' n, Int)
findLinearHyp numElim hyps =
    listToMaybe candidates
  where
    candidates =
      [ (h, var)
      | var <- [0..numElim-1]
      , let polysWithVar = filter (`varInPoly` var) hyps
      -- Only consider variables where at least 2 hypotheses involve them
      -- (so the linear one can simplify the others).
      , length polysWithVar >= 2
      , h <- take 1 [p | p <- polysWithVar, classVarDeg p var == 1]
      ]


-- Function that test a geometric theorem.
-- Inputs: hip, Hipotheses Polynomials; g, Theorem Polynomial;
--         numElim, number of dependent (X) variables to eliminate.
-- Output: list of the pseudo remainders of g with respect to the ascending chain
theoremProver :: (IsMonomialOrder n Grevlex, KnownNat n)
    => Int -> [Polynomial' n] -> Polynomial' n -> [Polynomial' n]
theoremProver numElim hip g = remWithChain wuChain g' 0
    where
        (hip', g', _) = preElimLinear numElim hip g
        wuChain =  charSet numElim hip' [] 0

-- | Like 'theoremProver', but also returns the characteristic set (Wu chain)
-- and pre-elimination diagnostics.
theoremProverVerbose :: (IsMonomialOrder n Grevlex, KnownNat n)
    => Int -> [Polynomial' n] -> Polynomial' n
    -> ([Polynomial' n], [Polynomial' n], [Polynomial' n], Polynomial' n, [Int])
    -- ^ (wuChain, remainders, preElimHyps, preElimConcl, eliminatedVars)
theoremProverVerbose numElim hip g =
    (wuChain, remWithChain wuChain g' 0, hip', g', elims)
    where
        (hip', g', elims) = preElimLinear numElim hip g
        wuChain = charSet numElim hip' [] 0
      
-- Function that get the pseudoremider of a polinomial with
-- respect to a set of polynomials
remWithChain :: (IsMonomialOrder n Grevlex, KnownNat n) 
    => [Polynomial' n] -> Polynomial' n -> Int -> [ Polynomial' n]
remWithChain [] _ _ = []
remWithChain chain pol var = [rem]++(remWithChain newChain rem (var + 1))
    where  
        rem = snd $ pseudoRemainder pol elemChain var -- remainder
        elemChain = head chain
        newChain = tail chain