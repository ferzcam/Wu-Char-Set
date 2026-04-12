{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Core
import Control.Exception (SomeException, evaluate, try)
import Control.Monad (when)
import Data.Proxy (Proxy (..))
import Data.Time.Clock (diffUTCTime, getCurrentTime)
import GHC.TypeNats (KnownNat, SomeNat (..), someNatVal)
import System.Directory (doesFileExist, listDirectory)
import System.Environment (getArgs)
import System.FilePath ((</>))
import System.IO (hFlush, stdout)
import System.Timeout (timeout)
import Data.List (isSuffixOf, sort)

defaultTimeoutSecs :: Int
defaultTimeoutSecs = 30

data Outcome
  = Proved
  | NotProved String
  | Errored String
  | TimedOut
  deriving (Show)

-- | Decide whether a run is a successful proof: all pseudoremainders vanish.
classify :: (KnownNat n) => [Polynomial' n] -> Outcome
classify [] = NotProved "empty remainder chain"
classify rems
  | last rems == 0 = Proved
  | otherwise      = NotProved "last pseudoremainder /= 0"

-- | Enrich a NotProved outcome with diagnostic info (Wu chain + final remainder).
addDebugInfo :: (KnownNat n)
  => [Polynomial' n] -> [Polynomial' n] -> Outcome -> Outcome
addDebugInfo chain rems (NotProved msg)
  | not (null rems) && last rems /= 0 =
      let lastRem = last rems
          nt = polyNumTerms lastRem
          remStr = if nt <= 200
                   then show lastRem
                   else "(too large: " ++ show nt ++ " terms)"
          info = "\n  Wu chain: " ++ show (length chain) ++ " polynomials"
              ++ "\n  Remainder chain: " ++ show (length rems) ++ " steps"
              ++ "\n  Final remainder (" ++ show nt ++ " terms):"
              ++ "\n  " ++ remStr
      in NotProved (msg ++ info)
addDebugInfo _ _ outcome = outcome

-- | Pretty-print a polynomial with a size guard.
showPoly :: (KnownNat n) => Polynomial' n -> String
showPoly p
  | nt <= 200 = show p
  | otherwise = "(too large: " ++ show nt ++ " terms)"
  where nt = polyNumTerms p

-- | Run the theorem prover for a problem already parameterized on @n@.
-- Honors a per-problem timeout in seconds.
runAt
  :: forall n. (KnownNat n)
  => Int -> Bool -> Proxy n -> AlgResult -> IO Outcome
runAt tmoSecs verbose _ ar = do
  let nDep = arNumXVars ar - length (arFreeVars ar)
      polys = generatePolynomials (arHypotheses ar) (arConclusion ar)
                                  (arFreeVars ar)
              :: [Polynomial' n]
  case polys of
    []            -> pure (Errored "no polynomials generated")
    (concl:hyps) -> do
      let action
            | verbose = do
                -- Print polynomials BEFORE pre-elimination
                putStrLn "--- Polynomials (before pre-elimination) ---"
                putStrLn $ "  conclusion: " ++ showPoly concl
                mapM_ (\(i,h) -> putStrLn $ "  h" ++ show i ++ ": " ++ showPoly h)
                      (zip [(1::Int)..] hyps)
                hFlush stdout
                -- Run prover (includes pre-elimination + charSet + remWithChain)
                let (chain, rems, hyps', concl', elims) =
                      theoremProverVerbose nDep hyps concl
                -- Print polynomials AFTER pre-elimination
                putStrLn "--- Polynomials (after pre-elimination) ---"
                putStrLn $ "  Eliminated vars: " ++ show elims
                         ++ " (" ++ show (length elims)
                         ++ " of " ++ show nDep ++ " dependent)"
                putStrLn $ "  conclusion: " ++ showPoly concl'
                mapM_ (\(i,h) -> putStrLn $ "  h" ++ show i ++ ": " ++ showPoly h)
                      (zip [(1::Int)..] hyps')
                putStrLn "---"
                hFlush stdout
                outcome <- evaluate (classify rems)
                pure (addDebugInfo chain rems outcome)
            | otherwise = evaluate (classify (theoremProver nDep hyps concl))
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
      outcome <- case someNatVal (fromIntegral (max 1 nX)) of
        SomeNat (p :: Proxy n) -> runAt tmoSecs verbose p ar
      t1 <- getCurrentTime
      let secs = realToFrac (diffUTCTime t1 t0) :: Double
      case outcome of
        Proved       -> putStrLn (showSecs secs ++ "s  OK  (nDep="
                     ++ show nDep ++ ", nFree=" ++ show nFree ++ ")")
        NotProved m  -> putStrLn (showSecs secs ++ "s  FAIL: " ++ m)
        Errored m    -> putStrLn (showSecs secs ++ "s  ERR:  " ++ take 120 m)
        TimedOut     -> putStrLn (showSecs secs ++ "s  TIMEOUT (>" ++ show tmoSecs ++ "s)")
  where
    showSecs s = let r = (fromIntegral (round (s * 1000) :: Int) :: Double) / 1000
                 in show r

runAll :: Int -> Bool -> FilePath -> IO ()
runAll tmoSecs verbose dir = do
  entries <- listDirectory dir
  let files = sort [dir </> f | f <- entries, ".dsl" `isSuffixOf` f]
  mapM_ (runProblemFile tmoSecs verbose) files

-- | Parse optional flags; return (timeout-secs, verbose, remaining-args).
parseArgs :: [String] -> (Int, Bool, [String])
parseArgs = go defaultTimeoutSecs False
  where
    go t v ("--timeout":n:rest) = go (read n) v rest
    go t v ("--verbose":rest)   = go t True rest
    go t v rest                 = (t, v, rest)

main :: IO ()
main = do
  rawArgs <- getArgs
  let (tmoSecs, verbose, args) = parseArgs rawArgs
  case args of
    [] ->
      runAll tmoSecs verbose "problems/dsl"
    ["--all"] ->
      runAll tmoSecs verbose "problems/dsl"
    ["--dir", d] ->
      runAll tmoSecs verbose d
    paths -> do
      existing <- filterM doesFileExist paths
      mapM_ (runProblemFile tmoSecs verbose) existing
  where
    filterM p = foldr go (pure [])
      where
        go x acc = do
          keep <- p x
          xs <- acc
          pure (if keep then x : xs else xs)
