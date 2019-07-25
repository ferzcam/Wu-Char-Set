module Polynomial.Wu where

-- | This algorithm was taken from the book "Ideals, Varieties and Algorithms" 4th ed.

-- | Algorithm to get characteristic set from a set of polynomials.
-- charSet :: [poly] -> [poly] -> Ordinal n -> [poly]
-- charSet [] a _ = a
-- charSet p a var
--     | lenS == 0 = charSet p a (var+1)
--     | lenS == 1 = charSet (quot p s) (a++s) (var+1)
--     | otherwise = case (existOneDegPoly s) of
--                     Just poly -> charSet (c++rem) (a++poly) (var+1)
--                     Nothing -> charSet (c++r++newS) a var

--     where
--         c = not s
--         s = foo p var
--         lenS = length s
--         rem = pseudos (dropPolys s poly) poly
--         (newS, r) = analizeS s var



