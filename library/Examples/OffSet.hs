{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude              #-}
{-# LANGUAGE ConstraintKinds, DataKinds, GADTs, KindSignatures     #-}

module Examples.OffSet (offset, q1, q2, q3, q4, q5) where 

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
y1 = var 1
z1 = var 2
l1 = var 3
x = var 4
y = var 5
z = var 6
v = var 7

a,b,c,d,e,f,g,h,i,j :: Expr Integer
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

--
q1,q2,q3,q4,q5:: PolynomialSym 8
q1 = a !* x1^2 + b !* y1^2 + c !* z1^2 + d!* (x1 * y1) + e !* (x1 * z1)  + f!* (y1 * z1) +  g !* x1 +  h !* y1 + i !* z1 + j!* 1
q2 = x - x1  - l1 * ( (2 * a) !* x + d !* y + e !* z + g !* 1 )
q3 = y - y1 - l1 * ( (2 * b) !* y + d !* x + f !* z + h !* 1 )
q4 = z - z1 - l1 * ( (2 * c) !* z + e !* x + f !* y + i !* 1 )
q5 = (x - x1)^2 + (y - y1)^2 + (z - z1)^2 - v^2
 
offset :: IO()
offset = printWuSet [q1,q2,q3,q4,q5] 0 0
