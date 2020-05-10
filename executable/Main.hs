{-#LANGUAGE DataKinds#-}

-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.
import Example
import Core
import Algebra.Ring.Polynomial


a = Point (U "u1") (U "u1")
b = Point (X "x1") (U "u1")
c = Point (X "x2") (X "x3")
d = Point (X "x4") (X "x5")
e = Point (X "x6") (X "x7")
f = Point (X "x2") (U "u1")
m = Point (X "x8") (U "u1")
n = Point (X "x9") (X "xx10")

lad = Line a d
lcb = Line c b
leb = Line e b
lca = Line c a
lnf = Line n f
lne = Line n e
lnd = Line n d
lnm = Line n m

h1 = Colinear d b c 
h2 = Perpendicular lad lcb
h3 = Colinear e a c
h4 = Perpendicular leb lca
h5 = MidPoint a m b
h6 = SameLen lnf lne
h7 = SameLen lnf lnd

g = SameLen lnf lnm

polys :: [Polynomial' 10]
polys@(conclusion:hypotheses) = generatePolynomials [h1, h2, h3, h4, h5, h6, h7] g

<<<<<<< HEAD
-- x1 = var 7
-- x2 = var 6 
-- x3 = var 5
-- x4 = var 4
-- x5 = var 3
-- x6 = var 2
-- x7 = var 1
-- x8 = var 0
-- u1 = var 8
-- u2 = var 9

-- h1,h2,h3,h4,h5,h6,h7,h8,g :: Polynomial' 10
-- h1 = 2*x1- u1
-- h2 = 2*x2 - u2
-- h3 = 2*x3 - u1 
-- h4 = 2*x4 - u2
-- h5 = u2*x5 + u1*x6 - u1*u2
-- h6 = u1*x5 - u2*x6
-- h7 = x1^2 - x2^2 - 2*x1*x7 + 2*x2*x8
-- h8 = x1^2 - 2*x1*x7 - x3^2 + 2*x3*x7 - x4^2 + 2*x4*x8
-- g= (x5 - x7)^2 + (x6-x8)^2 - (x1-x7)^2 -x8^2 

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
h11 = Colinear p a b

g = Colinear q s p

structres = [P a, P b, P c, P d, P e, P f, P o, P p, P q, P s, L loa, L loc, L lob, L lod, L lof, L loe]
hypsGeom = [h1, h2, h3, h4, h5, h6, h7, h8, h9, h10, h11]

polys :: [Polynomial' 14]
polys@(conclusion:hypotheses) = generatePolynomials structres hypsGeom g
=======
>>>>>>> 007921bd7cb5639da4bc6327b225f73c643fb17a


main :: IO ()
main = putStrLn "dasd"
