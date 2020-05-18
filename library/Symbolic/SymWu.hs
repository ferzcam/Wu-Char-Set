{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude, FlexibleContexts #-}

module Symbolic.SymWu (charSetSym, stepCharSetSym, printWuSet, runMathematica) where

import AlgebraicPrelude hiding (appendFile, fromString , (\\))
import Algebra.Ring.Polynomial hiding (leadingMonomial, leadingTerm)
import Symbolic.Prelude
import Symbolic.SymRem
import Symbolic.Expr
import Util.Coeff
import GHC.TypeLits
import qualified Data.Sized.Builtin       as S
import qualified Data.Map.Strict        as M
import System.IO (writeFile, appendFile)
import Debug.Trace
import Data.Maybe
import Data.Singletons
import Data.List ((\\))
import System.Directory
import System.Process


-- | Given a two list of Symbolic Polynomials ([PolynomialSym n]) and an Integer return the Ascending Chain 
-- | of the first list of Symbolic Polynomials 
charSetSym :: (IsMonomialOrder n Grevlex, KnownNat n) 
    => [PolynomialSym n] -> [ PolynomialSym n] -> Int -> [ PolynomialSym n]
charSetSym [] a _ = map (simplifyPolynomial) a
charSetSym p a var
    | lenS == 0 = charSetSym p a (var+1)
    | lenS == 1 =  charSetSym c (a++s) (var+1)
    | otherwise = case existOneDegPoly s var of
                    Just poly ->  charSetSym (c ++ rem poly) (a++[poly]) (var+1)
                    Nothing -> charSetSym (c++r++newS) a var
    where
        c = dropPolys p s -- p/s
        s =  filter (`varInPoly` var) p 
        lenS = length s
        rem poly =  remaindersSym (dropPolys s [poly]) poly var
        (newS, r) = analizeS' s var

-- | Given a two list of Symbolic Polynomials ([PolynomialSym n]) and an Integer return 
-- | a triple with the ascendeing chain at that iteration; a list of symbolic polynomials
-- | for the next iterations; and the new var for the next iterations, respectively. 
stepCharSetSym :: (IsMonomialOrder n Grevlex, KnownNat n) 
    => [PolynomialSym n] -> Int -> ( PolynomialSym n, [PolynomialSym n], Int)
stepCharSetSym [] var = ( 0, [], var)
stepCharSetSym p var
    | lenS == 0 = (0, p, var+1)
    | lenS == 1 = (s!!0, c, var+1)
    | otherwise = case existOneDegPoly s var of
                    Just poly ->  (poly, c ++ rem poly, var+1) 
                    Nothing ->  (0, c++r++newS, var)
    where
        c = dropPolys p s -- p/s
        s =  filter (`varInPoly` var) p 
        lenS = length s
        rem poly =  remaindersSym (dropPolys s [poly]) poly var
        (newS, r) = analizeS' s var



-- | Given a list of Polynomials and a Integer, return the values of r and the max. See book Cox, David A, Little, John, Oshea, Donal .Ideals, Varieties, and Algorithms Page 338
analizeS' :: (IsMonomialOrder n Grevlex, KnownNat n) => [PolynomialSym n] -> Int -> ([PolynomialSym n], [PolynomialSym n])
analizeS' ls@(x:y:z) var
        | classVarDeg r var > 1 =  analizeS' newls var
        | classVarDeg r var == 1 = (newls, [])
        | classVarDeg r var == 0 =  (dropPolys newls [max], [r])
        where
            (r, max) = maxNpseudoSP x y var
            newls = replacePoly ls max r 
        
-- | Given two polynomials and an integer return the value of r and a polynomial. See book Cox, David A, Little, John, Oshea, Donal .Ideals, Varieties, and Algorithms Page 338
maxNpseudoSP :: (IsMonomialOrder n Grevlex, KnownNat n) => PolynomialSym n -> PolynomialSym n -> Int -> (PolynomialSym n, PolynomialSym n)
maxNpseudoSP f g var
    | classVarDeg f var >= classVarDeg g var = (r, f)
    | otherwise = (r, g)
    where
        [newF, newG] = if (classVarDeg f var) > (classVarDeg g var) then [f,g] else [g,f] 
        r = psRemSym newF newG var


-----------------------------------------------FUCNTIONS TO PRINT EACH STEP OF THE WU CHARACTERISTIC SET -----------------------------------------------------

-- | Given a polynomial and integer and a Coefficient return a Polynomial with variable changed
changeVariables :: (KnownNat n) => PolynomialSym n -> Int -> Coeff -> PolynomialSym n
changeVariables pol step coeff = Polynomial $ M.fromList $ (transPol++filtered)
        where
                polList = reverse $ M.toList $ terms  pol
                coeffs = (AlgebraicPrelude.map ( fromCoeff step) [coeff ..])
                funcZip (a,b) c = (a, c)
                expr = map (\(x,y) -> fromExpr y) polList
                filtered = filter (\(x,y) -> y == (fromInteger 1) || y == (fromInteger (-1)) ) polList
                toTrans = polList \\ filtered
                transPol = zipWith funcZip toTrans coeffs 
            

-- | Given a set of polynomials an integer and a Coefficient, return a the list of polynomials with the variables changed
changeVariablesList :: (KnownNat n) => [PolynomialSym n] -> Int -> Coeff -> [PolynomialSym n]
changeVariablesList [] _ _ = []
changeVariablesList (x:xs) step coeff = (newPolX : changeVariablesList xs step (succ lastCoeff))
        where
                newPolX = changeVariables x step coeff
                lastCoeff = (toCoeff . snd . head . M.toList . terms) newPolX

             

-- | Given a list of polynomials and a path, print the polynomials in the given path
printPolys :: (IsMonomialOrder n Grevlex, IsOrder n Grevlex, KnownNat n)
        => [PolynomialSym n] -> FilePath -> IO ()
printPolys [] _ = return ()
printPolys (x:xs) path = do
                        a <- appendFile path $  printPolynomial x ++ "\n"
                        printPolys xs path

-- | Let a and b be two lists, the function print the terms in the following way
-- a1 = b1
-- a2 = b2
--    .
--    .
-- an = bn
printList :: (Show k) => [k] -> [k] -> FilePath -> IO ()
printList [] _ _ = return ()
printList (x:xs) (y:ys) path = if show x == "1" || show x == "(-1)"
                                        then do printList xs ys path
                                        else do 
                                                a <- appendFile path $ show x ++ "=" ++ show y ++ "\n"
                                                printList xs ys path


-- | Given two list of polynomials and a path. The function print the coefficients of the two list of polynomials 
printCoeffs :: (IsMonomialOrder n Grevlex, IsOrder n Grevlex, KnownNat n)
        => [PolynomialSym n] -> [PolynomialSym n] -> FilePath -> IO ()
printCoeffs [] [] _ = return ()
printCoeffs new@(n:ns) old@(o:os) path = do
                                        let coeffsNew = ((map (snd)) . reverse . M.toList . terms) n
                                        let coeffsOld = ((map (snd)) . reverse . M.toList . terms) o
                                        printHead <- printList coeffsNew coeffsOld path
                                        printTail <- printCoeffs ns os path
                                        return ()


-- | Given a set of Polinomials print only the string part that apear on all the polinomials
printInitCoeffs :: (IsMonomialOrder n Grevlex, IsOrder n Grevlex, KnownNat n)
        => [PolynomialSym n] -> FilePath -> IO ()
printInitCoeffs pols path = do
                            let coeffs = sort $ foldl1 (++) $ map (\x -> nub $ foldl1 (++ ) $ foldl1 (++) $ foldl1  (++) $ map ( map (nub . fst) . M.toList. fromExpr)  $ map (snd) $ M.toList $ _terms x) pols
                            printHead <- printLine coeffs path
                            return ()

-- | Given a list print a line in a file 
printLine :: (Show k) => [k] -> FilePath -> IO ()
printLine x path = do 
                    a <- appendFile path $ show x
                    return () 

-- | Given a Polynomials generate a string to print it
printPolynomial :: (KnownNat n) => PolynomialSym n -> String 
printPolynomial pol = dropPlusSign $  showTerms $ reverse $ M.toList $ _terms pol

-- | Given A String add Strings between the chars
dropPlusSign :: String -> String
dropPlusSign [] = error "String too short in dropPlusSign function"
dropPlusSign [_] = error "String too short in dropPlusSign function"
dropPlusSign [_,_] = error "String too short in dropPlusSign function"
dropPlusSign s@(x:y:z:a)
    | (x:y:[z]) == " + " = a
    | otherwise = s

-- | Given a Monomial, the function transform it to a string
showTerms :: (Unital k, Eq k, Show k, KnownNat n) =>  [(OrderedMonomial Grevlex n, k)] -> String
showTerms [] = ""
showTerms (t:ts)
    | coeff == one = " + " ++ showMon monList ++ showTerms ts
    | mon == one =  " + " ++ show coeff ++ showTerms ts
    | otherwise = " + " ++ show coeff ++ "*" ++ showMon monList ++ showTerms ts
    where 
        coeff = snd t
        mon = fst t
        monList = S.toList $ getMonomial mon 

-- | Given a List of integers print it as a monomial
showMon :: [Int] -> String
showMon mon = idexToStr idxMon
    where 
        idxMon = getIndeces mon 0

-- | Auxiliar function to  print the monomials
idexToStr :: [(Int,Int)] -> String
idexToStr [] = ""
idexToStr [x]
    | snd x == 1 = "x" ++ show (fst x)
    | otherwise = "x" ++ show (fst x) ++ "^" ++ show (snd x)
idexToStr (x:xs)
    | sup == 1  =  "x"++ show var ++ "*" ++ idexToStr xs
    | otherwise  =  "x"++ show var ++ "^" ++ show sup ++ "*" ++ idexToStr xs 
    where 
        var = fst x
        sup = snd x 

-- | Auxiliar function to get the indices and positon of the indices
getIndeces :: [Int] -> Int -> [(Int, Int)]
getIndeces [] _ = []
getIndeces (x:xs) pos  
    | x /= 0 = [(pos, x)] ++ getIndeces xs (pos+1)
    | otherwise = getIndeces xs (pos + 1)
    

-----------------------------------------------------------------------------------------------------------------------------------------------------------------


-- | Given a list of polynomials and an integer run the Wu's characteristic set and print each step
printWuSet :: (KnownNat n1) => [PolynomialSym n1]  -> Int -> Int -> IO ()
printWuSet [] var cont = return ()
printWuSet  pols var cont = do
                placeToSaveInitPols <- fmap (++ ("/Output/InitPols.txt")) getCurrentDirectory
                placeToSaveAscChain <- fmap (++ ("/Output/AscChain.txt")) getCurrentDirectory
                placeToSaveNewSet <- fmap (++ ("/Output/NewSetStep_"++ (show var)++".txt")) getCurrentDirectory
                placeToSaveCoeffs <- fmap (++ ("/Output/CoeffsStep_"++ (show var)++".txt")) getCurrentDirectory
                placeToSaveInitCo <- fmap (++ ("/Output/SymVars.txt")) getCurrentDirectory
              --  when (cont == 0) (callCommand "rm ~/Proyectos/Haskell/Projects/Wu-Char-Set/Output/*.txt")
                when (cont == 0) (printInitCoeffs pols placeToSaveInitCo)
                when (cont == 0) (printPolys pols placeToSaveInitPols)
                let (chain, newPols, newVar) = ( \(a, b, c) -> (simplifyNumSym a, map simplifyNumSym b, c) ) $ stepCharSetSym pols var
                if (newVar == var) 
                        then do (printWuSet newPols newVar 1)
                        else do let newSet = changeVariablesList newPols var (Coeff "a")               
                                writeCoeffs <- printCoeffs newSet newPols placeToSaveCoeffs
                                writeAscChain <- printPolys [chain] placeToSaveAscChain
                                if length newSet == 1
                                        then do printWuSet newSet newVar 1
                                        else do writeNewSet <- printPolys newSet placeToSaveNewSet
                                                printWuSet newSet newVar 1

runMathematica :: IO ()
runMathematica = callCommand "mathematica ~/Proyectos/Haskell/Projects/Wu-Char-Set/MathematicaNoteBooks/wu_graphics.nb &"
                                