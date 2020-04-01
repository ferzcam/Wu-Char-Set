{-#LANGUAGE DataKinds#-}

-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.
import Example
--import Algebra.Prelude hiding ((*),(-),(+),(^), (/), gcd, lcm, fromRational, map)
import Core


a = Point (U "u1") (U "u1")
b = Point (U "u3") (U "u1")
c = Point (U "u5") (U "u6")
d = Point (X "x1") (U "u7")
e = Point (X "x2") (U "u8")
f = Point (X "x3") (U "u9")
o = Point (X "x4") (X "x5")
p = Point (X "x6") (U "u1")
q = Point (X "x7") (X "x8")
s = Point (X "x9") (X "xx10")

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
h11 = Colinear p a b
g = Colinear s q p

structres = [P a, P b, P c, P d, P e, P f, P o, P p, P q, P s, L loa, L loc, L lob, L lod, L lof, L loe]
hypsGeom = [h1, h2, h3, h4, h5, h6, h7, h8, h9, h10]

polys :: [Polynomial' 10]
polys@(conclusion:hypotheses) = generatePolynomials structres hypsGeom g


main :: IO ()
main = print (theoremProver hypotheses conclusion)
