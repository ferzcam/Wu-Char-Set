{-#LANGUAGE DataKinds#-}

-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.
import Example
import Core
import Algebra.Ring.Polynomial
import GHC.TypeLits


a = Point (U "u1") (U "u1")
b = Point (X "x1") (U "u1")
c = Point (X "x2") (X "x3")
c1 = Point (X "x5") (X "x4")
b1 = Point (X "x7") (X "x6")
a1 = Point (X "x9") (X "x8")
o = Point (X "xx11") (X "xx10")



lac1 = Line a c1
lc1b = Line c1 b
loa1 = Line o a1
lob1 = Line o b1
la1a = Line a1 a
lb1b = Line b1 b
loa = Line o a
lob = Line o b
lab = Line a b
lb1c = Line b1 c
lb1a = Line b1 a
la1b = Line a1 b
la1c = Line a1 c

ang1 = Angle c1 a b
ang2 = Angle c a b1
ang3 = Angle a1 b c

h1 = SameLen lac1 lc1b 
h2 = SameLen lac1 lab
h3 = SameLen lb1c lb1a
h4 = SameAcAngle ang1 ang2 
h5 = SameAcAngle ang1 ang3
h6 = SameLen la1b la1c
h7 = Collinear o a1 a 
h8 = Collinear o b1 b 


g = Collinear c1 c o

polys :: [Polynomial' 11]
polys@(conclusion:hypotheses) = generatePolynomials [h1, h2, h3, h4, h5, h6, h7, h8] g

genPol :: Int -> Polynomial' 3
genPol g = randPol 3 4 2 g

-- randPol arity size deg seed 
set, set1 :: [Polynomial' 3]
set = setRandPol 3 4 2 12 3
set1 = [genPol 3, genPol 4, genPol 5]


main :: IO ()
main = putStrLn "Main Excutable"
