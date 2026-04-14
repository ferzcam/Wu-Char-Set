{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Core
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (when)
import Data.List (isSuffixOf, sort)
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import System.Directory (doesFileExist, listDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.IO (hFlush, stdout)
import System.Timeout (timeout)

defaultTimeoutSecs :: Int
defaultTimeoutSecs = 30

data Outcome
  = Proved
  | NotProved String
  | Errored String
  | TimedOut
  deriving (Show)

-- | Decide whether a run is a successful proof: all pseudoremainders vanish.
classify :: [Poly] -> Outcome
classify [] = NotProved "empty remainder chain"
classify rems
  | isZero (last rems) = Proved
  | otherwise          = NotProved "last pseudoremainder /= 0"

-- | Enrich a NotProved outcome with diagnostic info (Wu chain + final remainder).
addDebugInfo :: [Poly] -> [Poly] -> Outcome -> Outcome
addDebugInfo chain rems (NotProved msg)
  | not (null rems) && not (isZero (last rems)) =
      let lastRem = last rems
          nt = numTerms lastRem
          remStr | nt <= 200 = show lastRem
                 | otherwise = "(too large: " ++ show nt ++ " terms)"
          info = "\n  Wu chain: " ++ show (length chain) ++ " polynomials"
              ++ "\n  Remainder chain: " ++ show (length rems) ++ " steps"
              ++ "\n  Final remainder (" ++ show nt ++ " terms):"
              ++ "\n  " ++ remStr
      in NotProved (msg ++ info)
addDebugInfo _ _ outcome = outcome

-- | Pretty-print a polynomial with a size guard.
showPolySafe :: Poly -> String
showPolySafe p
  | nt <= 200 = show p
  | otherwise = "(too large: " ++ show nt ++ " terms)"
  where nt = numTerms p

-- | Run the theorem prover for a single algebraized problem.
runProblem :: Int -> Bool -> AlgResult -> IO Outcome
runProblem tmoSecs verbose ar = do
  let nDep = arNumXVars ar - length (arFreeVars ar)
      polys = generatePolynomials (arHypotheses ar) (arConclusion ar)
                                  (arFreeVars ar)
  case polys of
    [] -> pure (Errored "no polynomials generated")
    (concl:hyps) -> do
      let logIO s = putStrLn s >> hFlush stdout
      let action
            | verbose = do
                putStrLn "--- Polynomials (before pre-elimination) ---"
                putStrLn $ "  conclusion: " ++ showPolySafe concl
                mapM_ (\(i, h) -> putStrLn $ "  h" ++ show i ++ ": "
                                             ++ showPolySafe h)
                      (zip [(1::Int) ..] hyps)
                hFlush stdout
                (chain, rems, _hyps', _concl', _elims) <-
                  theoremProverIO logIO nDep hyps concl
                outcome <- evaluate (classify rems)
                pure (addDebugInfo chain rems outcome)
            | otherwise =
                evaluate (classify (theoremProver nDep hyps concl))
      result <- try (timeout (tmoSecs * 1000000) action)
      case result of
        Left (e :: SomeException) -> pure (Errored (show e))
        Right Nothing             -> pure TimedOut
        Right (Just outcome)      -> pure outcome

runProblemFile :: Int -> Bool -> FilePath -> IO ()
runProblemFile tmoSecs verbose path = do
  putStr (path ++ " ... ")
  hFlush stdout
  src <- readFile path
  parseResult <- try (evaluate (algebraize (parseProblem src)))
  case parseResult of
    Left (e :: SomeException) ->
      putStrLn ("PARSE-ERROR: " ++ show e)
    Right ar -> do
      let nX    = arNumXVars ar
          nFree = length (arFreeVars ar)
          nDep  = nX - nFree
      when verbose $ do
        putStrLn ""
        putStrLn "--- DSL source ---"
        putStr src
        putStrLn "--- Algebraization ---"
        putStr (showAlgResult ar)
        putStrLn "---"
        hFlush stdout
      t0 <- getCurrentTime
      outcome <- runProblem tmoSecs verbose ar
      t1 <- getCurrentTime
      let secs = realToFrac (diffUTCTime t1 t0) :: Double
      case outcome of
        Proved      -> putStrLn (showSecs secs ++ "s  OK  (nDep="
                      ++ show nDep ++ ", nFree=" ++ show nFree ++ ")")
        NotProved m -> putStrLn (showSecs secs ++ "s  FAIL: " ++ m)
        Errored m   -> putStrLn (showSecs secs ++ "s  ERR:  " ++ take 120 m)
        TimedOut    -> putStrLn (showSecs secs ++ "s  TIMEOUT (>"
                      ++ show tmoSecs ++ "s)")
  where
    showSecs s =
      let r = (fromIntegral (round (s * 1000) :: Int) :: Double) / 1000
      in show r

runAll :: Int -> Bool -> FilePath -> IO ()
runAll tmoSecs verbose dir = do
  entries <- listDirectory dir
  let files = sort [dir </> f | f <- entries, ".dsl" `isSuffixOf` f]
  mapM_ (runProblemFile tmoSecs verbose) files

-- | Parse optional flags; return @(timeout-secs, verbose, remaining-args)@.
parseArgs :: [String] -> (Int, Bool, [String])
parseArgs = go defaultTimeoutSecs False
  where
    go _ v ("--timeout":n:rest) = go (read n) v rest
    go t _ ("--verbose":rest)   = go t True rest
    go t v rest                 = (t, v, rest)

main :: IO ()
main = do
  rawArgs <- getArgs
  let (tmoSecs, verbose, args) = parseArgs rawArgs
  case args of
    []           -> runAll tmoSecs verbose "problems/dsl"
    ["--all"]    -> runAll tmoSecs verbose "problems/dsl"
    ["--dir", d] -> runAll tmoSecs verbose d
    paths -> do
      existing <- filterM' doesFileExist paths
      mapM_ (runProblemFile tmoSecs verbose) existing
  where
    filterM' p = foldr go (pure [])
      where
        go x acc = do
          keep <- p x
          xs <- acc
          pure (if keep then x : xs else xs)
