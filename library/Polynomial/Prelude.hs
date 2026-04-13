{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

-- | Backward-compatible wrapper around "Polynomial.Poly".
--
-- The old API was parameterized over a type-level natural for the arity
-- (via @computational-algebra@). The new core uses a runtime 'Int' arity
-- but we keep a phantom 'Polynomial'' synonym so existing test code with
-- annotations like @:: [Polynomial' 10]@ keeps compiling unchanged.
module Polynomial.Prelude
  ( -- * Core type (re-exported from 'Polynomial.Poly')
    Poly
  , Polynomial'
  , isZero
  , zeroPoly
  , constPoly
  , var
  , fromTerms
  , numTerms
  , polyArity
    -- * Class-variable operations
  , classVarDeg
  , varInPoly
  , leadingCoeffPoly
  , existOneDegPoly
    -- * Simplification
  , coefGcd
  , factor1
  , simplify
  , simplifyPolinomial
  , factorRemove
  , exactDiv
    -- * Pseudo-remainder
  , pseudoRemainder
  , pseudoRemainders
    -- * Set operations
  , dropPolys
  , replacePoly
    -- * Misc
  , getArity
  , polyNumTerms
  , showPoly
  ) where

import GHC.TypeLits (Nat)
import Polynomial.Poly

-- | Phantom-parameterized alias kept for source compatibility with the old
-- @OrderedPolynomial Rational Grevlex n@ type. The 'Nat' argument is ignored
-- at runtime — the real arity lives inside the 'Poly' value.
type Polynomial' (n :: Nat) = Poly

-- | Old name for 'simplify'. Kept to avoid churn in callers.
simplifyPolinomial :: Poly -> Poly
simplifyPolinomial = simplify

-- | Arity accessor under its old name.
getArity :: Poly -> Int
getArity = polyArity

-- | Number of terms under its old name.
polyNumTerms :: Poly -> Int
polyNumTerms = numTerms

-- | Given two sets polys1 and polys2, returns polys1 \\ polys2.
dropPolys :: Eq p => [p] -> [p] -> [p]
dropPolys polys1 polys2 = [p | p <- polys1, p `notElem` polys2]

-- | Replace @p@ with @q@ in a list.
replacePoly :: Eq p => [p] -> p -> p -> [p]
replacePoly polys p q = q : dropPolys polys [p]
