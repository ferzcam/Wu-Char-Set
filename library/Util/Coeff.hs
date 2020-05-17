module Util.Coeff
(
    Coeff(..),
    fromCoeff,
    toCoeff
) where

import Data.Char
import Symbolic.Expr
import Data.Map.Strict

-- Teorema de einsentein irreducibilidad de un polinomio

newtype Coeff = Coeff {getCoeff :: String} deriving (Eq)

instance Show Coeff where
    show (Coeff a) = show a

instance Ord Coeff where
    compare (Coeff a) (Coeff b) = compare' a b

compare' :: String -> String -> Ordering
compare' a b
    | length a < length b = LT
    | length a > length b = GT
    | a == b = EQ
compare' (x:xs) (y:ys)
    | x < y = LT
    | x > y = GT
    | x == y = compare' xs ys


instance Enum Coeff where
    succ (Coeff a) = Coeff $ succ' a
    enumFrom c = (c:enumFrom (succ c))

succ' :: String -> String
succ' [a]
    | a == 'w' = "y"
    | a == 'z' = 'a':"a"
    | a == 'Z' = 'A':"A"
    | otherwise = [succ a]
succ' a
    | last a == 'z' = succ' (init a) ++ "a"
    | last a == 'Z' = succ' (init a) ++ "A"
    | otherwise = (init a) ++ [succ $ last a]
    

fromCoeff :: Int -> Coeff -> Expr Integer
fromCoeff num coeff = Expr $ fromList [([getCoeff coeff ++ show num], 1)]

toCoeff :: Expr Integer -> Coeff
toCoeff (Expr a) = Coeff $ ( removeNumbers . showTermSym . fst . head . toList) a
    where
        removeNumbers xs = [x | x <- xs , not( x `elem` "1234567890" )]
