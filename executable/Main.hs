{-#LANGUAGE DataKinds#-}

-- It is generally a good idea to keep all your business logic in your library
-- and only use it in the executable. Doing so allows others to use what you
-- wrote in their libraries.
import Example
import Polynomial.Wu
import Prelude
--import Algebra.Prelude hiding ((*),(-),(+),(^), (/), gcd, lcm, fromRational, map)
import Polynomial.Prelude

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

-- h1,h2,h3,h4,h5,h6,h7,h8,g,r1,test1 :: Polynomial' Lex 10
-- h1 = 2*x1 - u1
-- h2 = 2*x2 - u2
-- h3 = 2*x3 - u1 
-- h4 = 2*x4 - u2
-- h5 = u2*x5 + u1*x6 - u1*u2
-- h6 = u1*x5 - u2*x6
-- h7 = x1^2 - x2^2 - 2*x1*x7 + 2*x2*x8
-- h8 = x1^2 - 2*x1*x7 - x3^2 + 2*x3*x7 - x4^2 + 2*x4*x8
-- g= (x5 - x7)^2 + (x6-x8)^2 - (x1-x7)^2 -x8^2 
-- r1 = x1*x2^2 + 2*x2^2 - x1 - 1
-- test1 = 4*x1^2*x2*x3 + 4*x1^2*x2^2*x4 + 8*x1^2*x2*x5 + x1*x2 + x1*x3

main :: IO ()
main = putStrLn "Hello World!"
