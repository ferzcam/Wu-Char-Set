{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Core
import Control.Exception (SomeException, evaluate, try)
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

-- | Run the theorem prover for a problem already parameterized on @n@.
-- Honors a per-problem timeout in seconds.
runAt
  :: forall n. (KnownNat n)
  => Int -> Bool -> Proxy n -> AlgResult -> IO Outcome
runAt tmoSecs verbose _ ar = do
  let polys = generatePolynomials (arHypotheses ar) (arConclusion ar)
              :: [Polynomial' n]
  case polys of
    []            -> pure (Errored "no polynomials generated")
    (concl:hyps) -> do
      let action
            | verbose = do
                let (chain, rems) = theoremProverVerbose hyps concl
                outcome <- evaluate (classify rems)
                pure (addDebugInfo chain rems outcome)
            | otherwise = evaluate (classify (theoremProver hyps concl))
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
      let nX = max 1 (arNumXVars ar)
      t0 <- getCurrentTime
      outcome <- case someNatVal (fromIntegral nX) of
        SomeNat (p :: Proxy n) -> runAt tmoSecs verbose p ar
      t1 <- getCurrentTime
      let secs = realToFrac (diffUTCTime t1 t0) :: Double
      case outcome of
        Proved       -> putStrLn (showSecs secs ++ "s  OK  (nX=" ++ show nX ++ ")")
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
