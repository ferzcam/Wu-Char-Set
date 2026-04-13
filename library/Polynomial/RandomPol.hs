module Polynomial.RandomPol where

import qualified Data.Vector.Unboxed as V
import Data.List ((\\))
import Polynomial.Poly
import System.Random
import System.Random.Shuffle

-- | Generate a random polynomial of the given arity, (approximate) number of
-- terms, and maximum total degree. @intGen@ is the seed.
randPol :: Int -> Int -> Int -> Int -> Poly
randPol arity sizePol deg intGen =
    fromTerms arity (zip monomials coeffs)
  where
    g = mkStdGen intGen
    rand i = mkStdGen $ randomRs (intGen :: Int, 500) g !! i
    initialArrays =
         arrayMonMax arity deg (rand 0)
       : [arrayMonRand arity deg (rand i) | i <- [2 .. sizePol]]
    monomials = map V.fromList initialArrays
    coeffs = map fromIntegral $
               take sizePol (randomRs (-500 :: Int, 500) (rand intGen))

arrayMonRand :: Int -> Int -> StdGen -> [Int]
arrayMonRand arity deg g = monomial
  where
    gen = snd $ split (snd $ next g)
    max_deg = fst $ randomR (1 :: Int, deg) gen
    idxs = [1 .. arity]
    monDic = monGen arity max_deg gen idxs []
    monomial = map snd (sortByFst monDic)

arrayMonMax :: Int -> Int -> StdGen -> [Int]
arrayMonMax arity deg g = monomial
  where
    gen = snd $ split (snd $ next g)
    idxs = [1 .. arity]
    monDic = monGen arity deg gen idxs []
    monomial = map snd (sortByFst monDic)

sortByFst :: Ord a => [(a, b)] -> [(a, b)]
sortByFst = foldr insert []
  where
    insert x [] = [x]
    insert x (y:ys) | fst x <= fst y = x : y : ys
                    | otherwise      = y : insert x ys

monGen :: Int -> Int -> StdGen -> [Int] -> [(Int, Int)] -> [(Int, Int)]
monGen 0 _ _ _ list = list
monGen arity 0 g idxs list = monGen (arity - 1) 0 gen_list nwidxs nwList
  where
    gen_list = snd $ next g
    indx = head $ shuf idxs gen_list
    nwidxs = idxs \\ [indx]
    nwList = list ++ [(indx, 0)]
monGen arity deg g idxs list = monGen (arity - 1) (deg - rand_int) gen nwidxs nwList
  where
    gen = snd $ next g
    rand_int = fst $ randomR (0 :: Int, deg) gen
    gen_list = snd $ next gen
    indx = head $ shuf idxs gen_list
    nwidxs = idxs \\ [indx]
    nwList = list ++ [(indx, rand_int)]

shuf :: [Int] -> StdGen -> [Int]
shuf list g = shuffle' list n g
  where
    n = length list
