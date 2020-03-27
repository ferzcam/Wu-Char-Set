{-#LANGUAGE DataKinds#-}

-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.
import Example
import Polynomial.Wu
import Prelude
import Algebra.Prelude hiding ((*),(-),(+),(^), (/), gcd, lcm, fromRational, map)
import Polynomial.Prelude
import Util.Tokenizer


a = Point "A"
b = Point "B"
c = Point "C"
d = Point "D"

l1 = Line a b
l2 = Line b c

hyp = Colinear a b c

conc = Intersect l1 l2

structures = [P a, P b, P c, P d, L l1, L l2]

res :: [Polynomial' Grevlex 8]
res = generatePolynomials structures [hyp] conc


main :: IO ()
main = putStrLn "Hello World!"
