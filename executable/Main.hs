{-#LANGUAGE DataKinds#-}

-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.
import Example
--import Algebra.Prelude hiding ((*),(-),(+),(^), (/), gcd, lcm, fromRational, map)
import Core


a = Point (Z) (Z)
b = Point (U "u3") (Z)
c = Point (U "u5") (U "u6")
d = Point (X "x1") (X "x2")
o = Point (X "x3") (X "x4")

-- a = Point (Z) (Z)
-- b = Point (U "u1") (Z)
-- c = Point (U "u2") (U "u3")
-- d = Point (X "x1") (X "x2")
-- o = Point (X "x3") (X "x4")


lab = Line a b
lcd = Line c d
lad = Line a d 
lbc = Line b c 
lao = Line a o
lco = Line c o

lbo = Line b o
ldo = Line d o

h1 = Parallel lab lcd
h2 = Parallel lad lbc
h3 = Colinear b o d
h4 = Colinear a o c
h5 = SameLen lab lcd
h6 = SameLen lbc lad

--conc = SameLen lao lco
conc = SameLen lbo ldo
points = [P a, P b, P c, P d, P o]

polys :: [Polynomial' 10]
polys@(conclusion:hypotheses) = generatePolynomials points [h1, h2, h3, h4] conc


-- a = Point (Z) (Z)
-- b = Point (U "u1") (Z)
-- c = Point (U "u2") (U "u3")
-- d = Point (X "x1") (X "x2")
-- o = Point (X "x3") (X "x4")


-- lab = Line a b
-- lcd = Line c d
-- lad = Line a d 
-- lbc = Line b c 
-- lao = Line a o
-- lco = Line c o

-- lbo = Line b o
-- ldo = Line d o

-- h1 = Parallel lab lcd
-- h2 = Parallel lad lbc
-- h3 = Colinear d o b
-- h4 = Colinear a c o
-- h5 = SameLen lab lcd
-- h6 = SameLen lbc lad

-- conc = SameLen lao lco
-- --conc = SameLen lbo ldo
-- points = [P a, P b, P c, P d, P o]

-- polys :: [Polynomial' 4]
-- polys@(conclusion:hypotheses) = generatePolynomials points [h1, h2, h3, h4] conc

main :: IO ()
main = print (theoremProver hypotheses conclusion)
