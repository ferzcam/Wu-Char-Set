{-# LANGUAGE MultiParamTypeClasses, NoImplicitPrelude              #-}
{-# LANGUAGE ConstraintKinds, DataKinds, GADTs, KindSignatures     #-}

module Examples.WuExample (testPrintWu1, testPrintWu2, d1, d2) where 

import AlgebraicPrelude
import Algebra.Ring.Polynomial hiding (leadingTerm)
import qualified Data.Map.Strict as MS
import Polynomial.Prelude
import Symbolic.SymRem
import Symbolic.SymWu
import Control.Arrow
import Symbolic.Prelude
import Symbolic.Expr



x = var 0
y = var 1
t = var 2


a,b,c,d,e,f,g,h,i,s,v,u :: Expr Integer
a = varSym "a"
b = varSym "b"
c = varSym "c"
s = varSym "s"
d = varSym "d"
e = varSym "e"
f = varSym "f"
v = varSym "v"
g = varSym "g"
h = varSym "h"
i = varSym "i"
u = varSym "u"



d1,d2,d3:: PolynomialSym 3
d1 = (x - a!*1)^2 + (y - b!*1)^2  - (t - c!*1)^2 - s!*1
d2 = (x - d!*1)^2 + (y - e!*1)^2  - (t - f!*1)^2 - v!*1
d3 = (x - g!*1)^2 + (y - h!*1)^2  - (t - i!*1)^2 - u!*1


testPrintWu1 :: IO()
testPrintWu1 = printWuSet [d1,d2] 0 0

testPrintWu2 :: IO()
testPrintWu2 = printWuSet [d1,d2,d3] 0 0