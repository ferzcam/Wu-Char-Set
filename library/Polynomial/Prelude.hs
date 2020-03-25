 {-# LANGUAGE NoImplicitPrelude #-}

module Polynomial.Prelude where

import Prelude (Rational, (/), gcd, lcm, fromRational, toRational) 
import Algebra.Prelude hiding (Rational, (++), map, findIndex, drop, leadingCoeff, leadingTerm, leadingMonomial, (/), gcd, lcm, fromRational, toRational)
import qualified Data.Map.Strict as MS
import qualified Data.Sized.Builtin as S (toList)
import Data.List hiding (drop)
import Data.Maybe
import Debug.Trace

type Polynomial' ord n = OrderedPolynomial Rational ord n
--type Polynomial' ord n = OrderedPolynomial k ord n

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
dropPolys polys1 polys2 = [p | p <- polys1 , p `notElem` polys2]

-- | Given a set polys of polynomials containing p and a polynomial q. Returns the set P with q instead p. 
replacePoly :: Eq poly => [poly] -> poly -> poly -> [poly]
replacePoly polys p q = q : dropPolys polys [p]


existOneDegPoly :: [Polynomial' ord n] -> Int -> Maybe (Polynomial' ord n)
existOneDegPoly polys var = find isOneDeg polys
        where
            isOneDeg poly = elem 1 (((map ((!! var) . S.toList . getMonomial . fst)) . MS.toList . _terms) poly) 

pseudoRemainders :: (IsMonomialOrder n ord, KnownNat n) => 
        [Polynomial' ord n] -> Polynomial' ord n -> Int -> [Polynomial' ord n]
pseudoRemainders polys poly var = map (\p -> snd $ pseudoRemainder p poly var) polys
            

pseudoRemainder :: (IsOrder n ord, KnownNat n, IsMonomialOrder n ord) 
        => Polynomial' ord n -> Polynomial' ord n -> Int -> (Polynomial' ord n, Polynomial' ord n)
pseudoRemainder f g var = (fst pseudo, simplifyPolinomial (snd pseudo))
        where 
                m = classVarDeg g var
                d = getCoeff factors var
                factors = chooseTermsWithVar g var
                pseudo = findQR 0 f g var m d        

findQR :: (IsMonomialOrder n ord, KnownNat n) 
        => Polynomial' ord n -> Polynomial' ord n -> Polynomial' ord n -> Int -> Int -> Polynomial' ord n -> (Polynomial' ord n, Polynomial' ord n)
findQR q r g var m d
        | r == 0 || classVarDeg r var < m = (q,r)
        | otherwise = let
                        arity = (length . S.toList . getMonomial . fst . head . MS.toList . _terms) r
                        newMonomial = mon var ((classVarDeg r var) - m) arity
                        x = Polynomial $ MS.fromList [(newMonomial ,1)]
                        factors = chooseTermsWithVar r var
                        lc_r = getCoeff factors var
                        newR =  d*r - lc_r*g*x
                        newQ = d*q + lc_r*x
                        in  findQR newQ newR g var m d
                        --trace ("\n new R:" ++ show newR ++ "\n Deg old r: " ++ show (classVarDeg r var) ++ "\t \t Deg new r: " ++ show (classVarDeg newR var)  ++  "\n lcr: "  ++ show lc_r ++ "\t d: " ++ show d)

-- trace ("EEEEEEE LEADING TERM" ++ (show pol) ++ "     " ++ show polToList)
-- Assumes that the polynomial containns variable. The ordering will be Lexicographical
leadingTerm :: (IsMonomialOrder n ord, IsOrder n ord, KnownNat n) => Polynomial' ord n -> Int -> (Rational, OrderedMonomial ord n)
leadingTerm pol var =  (snd &&& fst) $ fromJust $ MS.lookupLE chosenTerm (_terms pol)
        where
                chosenTerm = toMonomial (foldr1 foo polToList)
                polToList = map (S.toList . getMonomial) (MS.keys $ _terms pol)
                foo monomCoeffs acc = if monomCoeffs!!var > acc!!var then monomCoeffs else acc

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
insertAt z y xs = as ++ (y : tail bs)
                        where (as,bs) = splitAt z xs
-----

----------------------------------------FUNCIONES EXTRAS PARA OBTENER DE MANERA ADECUADA EL LC-------------------------
chooseTermsWithVar :: (IsMonomialOrder n ord, IsOrder n ord, KnownNat n) => Polynomial' ord n -> Int -> Polynomial' ord n
chooseTermsWithVar pol var
                | not $ varInPoly pol var = chooseTermsWithVar pol (var + 1)
                | otherwise = foldr' foo 0 idxs
                where
                        deg_pol = classVarDeg pol var
                        idxs = findIndices (\x -> x!!var == deg_pol) (map (S.toList . getMonomial) (MS.keys $ _terms pol))
                        foo idx acc = acc + toPolynomial (snd $ auxMonom pol idx , fst $ auxMonom pol idx)
                        auxMonom poly idx = MS.elemAt idx $ _terms poly

simplifyMonomial ::(IsMonomialOrder n ord, IsOrder n ord, KnownNat n)   =>  Polynomial' ord n -> Polynomial' ord n
simplifyMonomial pol = pol // (1, commonMonomial pol)


simplifyPolinomial ::(IsMonomialOrder n ord, IsOrder n ord, KnownNat n)   =>  Polynomial' ord n -> Polynomial' ord n
simplifyPolinomial pol  = pol // commonTerm pol
        
getCoeff :: (IsMonomialOrder n ord, IsOrder n ord, KnownNat n)   =>  Polynomial' ord n -> Int -> Polynomial' ord n
getCoeff pol var = pol // (1, classVariable)
        where
                deg = classVarDeg pol var
                arity = (length . S.toList . getMonomial . fst . head . MS.toList . _terms) pol
                classVariable = mon var deg arity
                



gcdRational :: Rational -> Rational -> Rational
gcdRational num1 num2 = toRational (gcdNum / lcmDen)
        where 
                gcdNum = fromInteger $ gcd a c 
                lcmDen = fromInteger $ lcm b d
                a = numerator $ fromRational num1
                b = denominator $ fromRational num1
                c = numerator $ fromRational num2
                d = denominator $ fromRational num2

replaceZero :: (Num a) => [a] -> Int -> [a]
replaceZero (x:xs) position
        | position == 0 = 0:xs
        | otherwise = x : replaceZero xs (position-1)


-- Funcion que intentará dividir un polinomio por un monomio
(//) :: (IsMonomialOrder n ord, IsOrder n ord, KnownNat n) 
            =>  Polynomial' ord n  -> (Rational, OrderedMonomial ord n) ->  Polynomial' ord n
pol // (coeff, mon) = Algebra.Prelude.sum $ map (toPolynomial . (`tryDiv'` (coeff, mon)) . (snd &&& fst)) terms
            where
                    terms = MS.toList $ _terms pol

-- Funcion que obtiene el gcd de un polinomio, en este caso se refiere al termino en comun de todos los monomios que conforman el polinomio
-- commonMonomial ::(IsMonomialOrder n ord, IsOrder n ord, KnownNat n) 
--         =>  Polynomial' ord n  -> OrderedMonomial ord n
-- commonMonomial pol  =   foldr' foo (last monomials) (init monomials)
--                         where
--                                 foo monomial acc = gcdMonomial acc monomial
--                                 monomials = MS.keys $ _terms pol
                                

commonMonomial ::(IsMonomialOrder n ord, IsOrder n ord, KnownNat n) 
        =>  Polynomial' ord n  ->  OrderedMonomial ord n
commonMonomial pol  =  foldr' foo (last monomials) (init monomials) 
                        where
                                foo monomial acc = gcdMonomial acc monomial
                                monomials = MS.keys $ _terms pol
                       

commonTerm ::(IsMonomialOrder n ord, IsOrder n ord, KnownNat n) 
        =>  Polynomial' ord n  ->  (Rational, OrderedMonomial ord n)
commonTerm pol  =  (coeff ,foldr' foo (last monomials) (init monomials) )
                        where
                                foo monomial acc = gcdMonomial acc monomial
                                monomials = MS.keys $ _terms pol
                                coeff = foldl1 (gcdRational) $ MS.elems $ _terms pol

tryDiv' :: (Rational, OrderedMonomial ord n) -> (Rational, OrderedMonomial ord n) -> (Rational, OrderedMonomial ord n)
tryDiv' (a, f) (b, g)
        | g `divs` f = (a/b, OrderedMonomial $ zipWithSame (-) (getMonomial f) (getMonomial g))
        | otherwise  = error "cannot divide."
---------------------------------------------------------------------------------------------------------------------------------------------