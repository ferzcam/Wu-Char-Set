 {-# LANGUAGE NoImplicitPrelude #-}

module Polynomial.Prelude where

import Prelude (Rational)
import Algebra.Prelude hiding (Rational, (++), map, findIndex, drop, leadingCoeff, leadingTerm, leadingMonomial)
import qualified Data.Map.Strict as MS
import qualified Data.Sized.Builtin as S (toList)
import Data.List hiding (drop)
import Data.Maybe
import Debug.Trace

type Polynomial' ord n = OrderedPolynomial Rational ord n

-- -- | Returns the position of the class variable. Monomial ordering must be defined correctly.
-- classVarDeg ::  Polynomial' ord n -> Int
-- classVarDeg poly = let maybeDegree = (find (/=0)) . S.toList . getMonomial . fst . head . (MS.toDescList) . _terms
--                     in case maybeDegree poly of
--                         Just a -> a
--                         Nothing -> 0

classVarDeg :: (IsOrder n ord, KnownNat n, IsMonomialOrder n ord)
        =>  Polynomial' ord n -> Int -> Int
classVarDeg pol var =  leadingMonomialDegs !! var
        where
                leadingMonomialDegs = S.toList $ getMonomial $ leadingMonomial pol var

-- (IsOrder n order, KnownNat n,  IsMonomialOrder n order)
varInPoly :: (IsOrder n ord, KnownNat n,  IsMonomialOrder n ord)
        =>  Polynomial' ord n -> Int -> Bool
varInPoly pol var
    | classVarDeg pol var == 0 = False
    | otherwise = True

-- | Given two sets polys1 and polys2. Returns polys1/polys2                        
dropPolys :: Eq poly => [poly] -> [poly] -> [poly]                       
dropPolys polys1 polys2 = [p | p <- polys1 , not (elem p polys2)]

-- | Given a set polys of polynomials containing p and a polynomial q. Returns the set P with q instead p. 
replacePoly :: Eq poly => [poly] -> poly -> poly -> [poly]
replacePoly polys p q = q:(dropPolys polys [p])


existOneDegPoly :: [Polynomial' ord n] -> Int -> Maybe (Polynomial' ord n)
existOneDegPoly polys var = find (isOneDeg) polys
        where
            isOneDeg poly = any (==1) ( ((map ((!! var) . S.toList . getMonomial . fst)) . MS.toList . _terms) poly) 

pseudoRemainders :: (IsMonomialOrder n ord, KnownNat n) => 
        [Polynomial' ord n] -> Polynomial' ord n -> Int -> [Polynomial' ord n]
pseudoRemainders polys poly var = map (\p -> snd $ pseudoRemainder p poly var) polys
            

pseudoRemainder :: (IsOrder n ord, KnownNat n, IsMonomialOrder n ord) 
        => Polynomial' ord n -> Polynomial' ord n -> Int -> (Polynomial' ord n, Polynomial' ord n)
pseudoRemainder f g var = trace ("EEEEE PSEUDO REM " ++ show f ++ "       " ++ show g) findQR 0 f g var m d
        where 
                m = classVarDeg g var
                d = leadingCoeff g var




findQR :: (IsMonomialOrder n ord, KnownNat n) 
        => Polynomial' ord n -> Polynomial' ord n -> Polynomial' ord n -> Int -> Int -> Rational -> (Polynomial' ord n, Polynomial' ord n)
findQR q r g var m d
        | r == 0 || classVarDeg r var < m = (q,r)
        | otherwise = let
                        arity = (length . S.toList . getMonomial . fst . head . MS.toList . _terms) r
                        newMonomial = mon var ((classVarDeg r var)-m) arity
                        x = Polynomial $ MS.fromList [(newMonomial ,1)]
                        newR = d!*r - (leadingCoeff r var)!*g*x
                        newQ = d!*q + (leadingCoeff r var)!*x
                        in findQR newQ newR g var m d

-- trace ("EEEEEEE LEADING TERM" ++ (show pol) ++ "     " ++ show polToList)
-- Assumes that the polynomial containns variable. The ordering will be Lexicographical
leadingTerm :: (IsMonomialOrder n ord, IsOrder n ord, KnownNat n) => Polynomial' ord n -> Int -> (Rational, OrderedMonomial ord n)
leadingTerm pol var =  (snd &&& fst) $ fromJust $ MS.lookupLE chosenTerm (_terms pol)
        where
                chosenTerm = toMonomial (foldr1 foo polToList)
                polToList = map (S.toList . getMonomial) (MS.keys $ _terms pol)
                foo = \monomCoeffs acc -> if monomCoeffs!!var > acc!!var then monomCoeffs else acc

leadingMonomial :: (IsMonomialOrder n ord, IsOrder n ord, KnownNat n) => Polynomial' ord n -> Int -> OrderedMonomial ord n
leadingMonomial pol var = snd $ leadingTerm pol var

leadingCoeff :: (IsMonomialOrder n ord, IsOrder n ord, KnownNat n) => Polynomial' ord n -> Int -> Rational
leadingCoeff pol var = trace "EEEEEEE leading Coeff" fst $ leadingTerm pol var




toMonomial :: (KnownNat n) => [Int] -> OrderedMonomial ord n
toMonomial a = orderMonomial Proxy (fromList sing a)

--Genera un polinomio del tipo p(x1,x2,...xn) = xi^k
mon :: (IsOrder n order, KnownNat n, IsMonomialOrder n order)
        => Int -> Int -> Int -> OrderedMonomial order n
mon var exp numVars = toMonomial exps
        where 
                zeros = replicate numVars 0
                exps = insertAt var exp zeros

insertAt :: Int -> Int-> [Int] -> [Int] 
insertAt z y xs = as ++ (y:(tail bs))
                        where (as,bs) = splitAt z xs
-----
