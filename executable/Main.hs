{-#LANGUAGE DataKinds#-}

-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.
import Example



-- import Algebra.Prelude hiding (Rational, map, findIndex, drop)
-- import qualified Data.Map.Strict as MS
-- import qualified Data.Sized.Builtin as S (toList)
-- import Data.List hiding (drop)


main :: IO ()
main = putStrLn "Hello World!"
