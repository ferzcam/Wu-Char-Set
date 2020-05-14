 {-# LANGUAGE FlexibleContexts, DataKinds, TupleSections, TypeFamilies #-}

module Polynomial.RandomPol where

import Algebra.Ring.Polynomial hiding (leadingMonomial, leadingTerm)
import Polynomial.Prelude
import qualified Data.Map.Strict as MS
import Polynomial.Prelude
import Data.Matrix hiding (flatten, trace, zero)
import Data.List
import Debug.Trace
import Data.Maybe
import GHC.TypeLits
import Algebra.Ring.Polynomial
import System.Random 
import System.Random.Shuffle 



randPol :: (KnownNat n) => Int -> Int -> Int -> Int -> Polynomial' n
randPol arity sizePol deg intGen =  poly
    where
        g = mkStdGen intGen -- Define a random generator from an Int
        rand = \i ->  mkStdGen $ (randomRs (intGen:: Int, 500) g)!!i  -- Create a list of random numbers in a given interval
        initialArrays =  [ arrayMonMax arity deg (rand 0) ]++[ arrayMonRand arity deg (rand i) |  i<-[2..sizePol] ]
        -- Create an array of Monomials in list form [1,2,3,4]
        monomials = map toMonomial initialArrays -- Transform a list into monomials
        coeffs = take sizePol (randomRs (-500:: Int, 500) (rand intGen) )
        polTrans mon acc = acc + toPolynomial  (fromIntegral $ fst mon ,snd mon)
        monicPols = zip coeffs monomials
        poly = foldr polTrans 0 monicPols
        

arrayMonRand :: Int -> Int -> StdGen -> [Int]
arrayMonRand arity deg g =  monomial
        where 
            gen = snd $ split (snd $ next g) -- Generate a new random generator
            max_deg = fst $ randomR (1:: Int, deg) gen -- Max deg of the monomial
            idxs = [1..arity] -- List of available indices
            monDic = sort $ monGen arity max_deg gen idxs [] -- List of  ( position , value )
            monomial = map (snd) monDic 

arrayMonMax :: Int -> Int -> StdGen -> [Int]
arrayMonMax arity deg g  =  monomial
        where 
            gen = snd $ split (snd $ next g) -- Generate a new random generator
            idxs = [1..arity] -- List of available indices
            monDic = sort $ monGen arity deg gen idxs [] -- List of  ( position , value )
            monomial = map (snd) monDic            


monGen ::  Int -> Int -> StdGen -> [Int] -> [(Int,Int)] -> [(Int, Int)]
monGen  0 _ _ _ list = list
monGen  arity 0 g idxs list = monGen (arity -1) 0 gen_list nwidxs nwList
        where 
            gen_list = snd $ next g -- Generate a new random generator for the list shuffle
            indx = head $ shuf idxs gen_list -- Get a random index from a list of indices 
            nwidxs = idxs \\ [indx] -- Generate a new list of indices
            nwList = list ++ [(indx, 0)]
monGen arity deg g idxs list =  monGen (arity-1) (deg - rand_int) gen nwidxs nwList
        where
            gen = snd $ next g
            rand_int = fst $ randomR (0:: Int, deg) gen
            gen_list = snd $ next gen -- Generate a new random generator for the list shuffle
            indx = head $ shuf idxs gen_list -- Get a random index from a list of indices 
            nwidxs = idxs \\ [indx] -- Generate a new list of indices
            nwList = list ++ [(indx, rand_int)]


shuf :: [Int] -> StdGen -> [Int]
shuf list g = shuffle' list n g
    where 
        n = length list