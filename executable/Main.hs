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
o = Point "O"

lab = Line a b
ldc = Line d c
lac = Line a c 
lbd = Line b d
lda = Line d a 
lbc = Line b c 
lao = Line a o
ldo = Line d o
angl_cbi = Angle c b o
angl_iba = Angle o b a

h1 = Parallel lab ldc
h2 = Parallel lda lbc
h3 = Colinear d o b
h4 = Colinear a c o
h5 = Acute angl_cbi angl_iba

conc = SameLen lao ldo

structures = [P a, P b, P c, P d, P o, L lab, L ldc, L lac, L lbd, L lda, L lbc, L lao, L ldo, A angl_cbi, A angl_iba]

res :: [Polynomial' Grevlex 10]
res = generatePolynomials structures [h1, h2, h3, h4, h5] conc


main :: IO ()
main = putStrLn "Hello World!"
