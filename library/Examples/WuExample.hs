{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude              #-}
{-# LANGUAGE ConstraintKinds, DataKinds, GADTs, KindSignatures     #-}

module Examples.WuExample (testPrintWu1, testPrintWu2, d1) where 

import AlgebraicPrelude
import Algebra.Ring.Polynomial hiding (leadingTerm)
import qualified Data.Map.Strict as MS
import Polynomial.Prelude
import Symbolic.SymRem
import Symbolic.SymWu
import Control.Arrow
import Symbolic.Prelude
import Symbolic.Expr



x1 = var 0
x2 = var 1
x3 = var 2
x4 = var 3
x5 = var 4
x6 = var 5


a,b,c,d,e,f,g,h,i,j,k,l,m,q,o :: Expr Integer
a = varSym "a"
b = varSym "b"
c = varSym "c"
d = varSym "d"
e = varSym "e"
f = varSym "f"
g = varSym "g"
h = varSym "h"
i = varSym "i"
j = varSym "j"
k = varSym "k"
l = varSym "l"
m = varSym "m"
q = varSym "q"
o = varSym "o"



d1,d2,d3,d4,d5,d6 :: PolynomialSym 6
d1 = -(1 + a^2!*1 + b^2!*1 + c^2!*1)*x1 - (-1 + a^2!*1 + b^2!*1 + c^2!*1)*x2 + 2*(a!*x3 + b!*x4 + c!*x5) 
d2 = -(1 + d^2!*1 + e^2!*1 + f^2!*1)*x1 - (-1 + d^2!*1 + e^2!*1 + f^2!*1)*x2 + 2*(d!*x3 + e!*x4 + f!*x5)
d3 = -(1 + g^2!*1 + h^2!*1 + i^2!*1)*x1 - (-1 + g^2!*1 + h^2!*1 + i^2!*1)*x2 + 2*(g!*x3 + h!*x4 + i!*x5)
d4 = -(1 + j^2!*1 + k^2!*1 + l^2!*1)*x1 - (-1 + j^2!*1 + k^2!*1 + l^2!*1)*x2 + 2*(j!*x3 + k!*x4 + l!*x5)
d5 = -(1 + m^2!*1 + q^2!*1 + o^2!*1)*x1 - (-1 + m^2!*1 + q^2!*1 + o^2!*1)*x2 + 2*(m!*x3 + q!*x4 + o!*x5)
d6 = -x1^2 + x2^2 + x3^2 + x4^2 + x5^2 - x6^2
 


testPrintWu1 :: IO()
testPrintWu1 = printWuSet [d1,d2,d3,d4,d5,d6] 0 0

testPrintWu2 :: IO()
testPrintWu2 = printWuSet [d1] 0 0