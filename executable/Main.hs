{-#LANGUAGE DataKinds#-}

-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.
import Example
--import Algebra.Prelude hiding ((*),(-),(+),(^), (/), gcd, lcm, fromRational, map)
import Core



a = IndepPoint "A"
b = IndepPoint "B"
c = IndepPoint "C"
d = DepPoint "D"
e = DepPoint "E"
f = DepPoint "F"
o = DepPoint "O"
p = DepPoint "P"
q = DepPoint "Q"
s = DepPoint "S"

loa = Line o a
loc = Line o c
lob = Line o b
lod = Line o d
lof = Line o f
loe = Line o e


h1 = SameLen loa loc
h2 = SameLen loa lob
h3 = SameLen loa lod
h4 = SameLen loa lof
h5 = SameLen loa loe
h6 = Colinear p d f
h7 = Colinear q f e
h8 = Colinear q b c
h9 = Colinear s e a
h10 = Colinear s c d

g = Colinear q s p

structres = [P a, P b, P c, P d, P e, P f, P o, P p, P q, P s, L loa, L loc, L lob, L lod, L lof, L loe]
hypsGeom = [h1, h2, h3, h4, h5, h6, h7, h8, h9, h10]

polys :: [Polynomial' 14]
polys@(conclusion:hypotheses) = generatePolynomials structres hypsGeom g


main :: IO ()
main = print (theoremProver hypotheses conclusion)
