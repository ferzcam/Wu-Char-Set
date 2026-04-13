-- | Wu's characteristic set construction.
--
-- Algorithm from /Ideals, Varieties and Algorithms/ (Cox-Little-O'Shea, 4th ed.),
-- with optimizations ported from the Java JGEX reference implementation:
--
--   * 'reducePass' mirrors @CharSet.reduce@ — at entry and exit, short
--     polynomials (≤2 terms) are used to pseudo-remainder-reduce the rest.
--   * 'analizeS' selects the /lowest-degree/ divisor (à la @PolyBasic.getMinV@)
--     so remainders stay small.
--   * 'pseudoRemainder' itself (in "Polynomial.Poly") interleaves coefficient
--     GCD after every reduction step to prevent coefficient explosion.
module Polynomial.Wu
  ( charSet
  , analizeS
  , maxNpseudo
  ) where

import Data.List (foldl', sortBy)
import Data.Ord (comparing)
import Polynomial.Poly
import Polynomial.Prelude (dropPolys, replacePoly, simplifyPolinomial)

-- | Characteristic-set construction.
--
-- @charSet numElim hyps acc var@ triangulates @hyps@ with respect to the
-- first @numElim@ (dependent) variables. Higher-indexed variables are free
-- U-parameters and are treated as passive coefficients.
--
-- A 'reducePass' is applied once at entry and once at exit (mirroring Java
-- @CharSet.reduce@) — doing it on every recursive step would redo the same
-- short-poly sweeps @O(numElim)@ times for no extra benefit.
charSet :: Int -> [Poly] -> [Poly] -> Int -> [Poly]
charSet numElim p0 a0 v0 =
  map simplify (reducePass (go numElim (reducePass p0) a0 v0))
  where
    go _ [] a _ = a
    go n _ a var | var >= n = a
    go n p a var =
      let s = filter (`varInPoly` var) p
          c = dropPolys p s
          lenS = length s
      in case lenS of
           0 -> go n p a (var + 1)
           1 -> go n c (a ++ s) (var + 1)
           _ -> case existOneDegPoly s var of
                  Just poly ->
                    go n
                       (c ++ pseudoRemainders (dropPolys s [poly]) poly var)
                       (a ++ [poly])
                       (var + 1)
                  Nothing ->
                    let (newS, r) = analizeS s var
                    in go n (c ++ r ++ newS) a var

-- | Pick the best divisor among two candidates for variable @v@ (lowest
-- degree wins — Java's @getMinV@ heuristic). Returns @(remainder, divisor)@.
maxNpseudo :: Poly -> Poly -> Int -> (Poly, Poly)
maxNpseudo f g v
  | dF >= dG = (snd (pseudoRemainder f g v), f)
  | otherwise = (snd (pseudoRemainder g f v), g)
  where
    dF = classVarDeg f v
    dG = classVarDeg g v

-- | Iteratively reduce a set @s@ of polynomials all sharing class variable
-- @v@. Uses the /lowest-degree/ element as divisor at each step, mirroring
-- Java's @PolyBasic.getMinV@ selection.
analizeS :: [Poly] -> Int -> ([Poly], [Poly])
analizeS ls v =
  let -- Sort ascending by degree in v; pick the smallest as divisor.
      sorted = sortBy (comparing (\p -> classVarDeg p v)) ls
      divisor = head sorted
      dividend = head (tail sorted)
      (r, maxP) = maxNpseudo dividend divisor v
      dR = classVarDeg r v
      newls = replacePoly ls maxP r
  in case dR of
       0 -> (dropPolys newls [maxP], [r])
       1 -> (newls, [])
       _ -> analizeS newls v

-- ---------------------------------------------------------------------------
-- Reduction pass (port of CharSet.reduce)
-- ---------------------------------------------------------------------------

-- | Use short polynomials (≤2 terms) to pseudo-remainder-reduce every other
-- polynomial in the set. Repeats until no further reductions apply.
--
-- Short polys are typically linear hypotheses of the form @x_i - c@ or
-- similar — applying them as rewrites early keeps intermediate results tiny.
reducePass :: [Poly] -> [Poly]
reducePass ps =
  let shorts = filter (\p -> not (isZero p) && numTerms p <= 2) ps
  in if null shorts then ps else foldl' reduceWith ps shorts
  where
    reduceWith acc short =
      case leadingVar short of
        Nothing -> acc
        Just v ->
          [ if p == short || not (varInPoly p v)
              then p
              else let r = simplifyPolinomial (snd (pseudoRemainder p short v))
                   in if isZero r then p else r
          | p <- acc ]

-- | The smallest variable index present in the polynomial. Used as the
-- elimination variable for the short-poly reduction pass.
leadingVar :: Poly -> Maybe Int
leadingVar p
  | isZero p = Nothing
  | otherwise =
      let ar = polyArity p
          go i | i >= ar              = Nothing
               | varInPoly p i        = Just i
               | otherwise            = go (i + 1)
      in go 0
