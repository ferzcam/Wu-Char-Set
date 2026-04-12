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

-- | Run the theorem prover for a problem already parameterized on @n@.
-- Honors a per-problem timeout in seconds.
runAt
  :: forall n. (KnownNat n)
  => Int -> Proxy n -> AlgResult -> IO Outcome
runAt tmoSecs _ ar = do
  let polys = generatePolynomials (arHypotheses ar) (arConclusion ar)
              :: [Polynomial' n]
  case polys of
    []            -> pure (Errored "no polynomials generated")
    (concl:hyps) -> do
      let action = evaluate (classify (theoremProver hyps concl))
      result <- try (timeout (tmoSecs * 1000000) action)
      case result of
        Left (e :: SomeException) -> pure (Errored (show e))
        Right Nothing             -> pure TimedOut
        Right (Just outcome)      -> pure outcome

runProblemFile :: Int -> FilePath -> IO ()
runProblemFile tmoSecs path = do
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
        SomeNat (p :: Proxy n) -> runAt tmoSecs p ar
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

runAll :: Int -> FilePath -> IO ()
runAll tmoSecs dir = do
  entries <- listDirectory dir
  let files = sort [dir </> f | f <- entries, ".dsl" `isSuffixOf` f]
  mapM_ (runProblemFile tmoSecs) files

-- | Parse optional @--timeout N@ flag; return (timeout-secs, remaining-args).
parseTimeout :: [String] -> (Int, [String])
parseTimeout ("--timeout":n:rest) = (read n, rest)
parseTimeout args                 = (defaultTimeoutSecs, args)

main :: IO ()
main = do
  rawArgs <- getArgs
  let (tmoSecs, args) = parseTimeout rawArgs
  case args of
    [] ->
      runAll tmoSecs "problems/dsl"
    ["--all"] ->
      runAll tmoSecs "problems/dsl"
    ["--dir", d] ->
      runAll tmoSecs d
    paths -> do
      existing <- filterM doesFileExist paths
      mapM_ (runProblemFile tmoSecs) existing
  where
    filterM p = foldr go (pure [])
      where
        go x acc = do
          keep <- p x
          xs <- acc
          pure (if keep then x : xs else xs)
