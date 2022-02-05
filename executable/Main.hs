{-#LANGUAGE DataKinds#-}

-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.
import Example
import Core
import AlgebraicPrelude
import Algebra.Ring.Polynomial hiding (leadingTerm)
import qualified Data.Map.Strict as M


main :: IO ()
main = putStrLn "Main"


