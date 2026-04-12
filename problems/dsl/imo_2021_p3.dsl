# IMO 2021 P3
# AD bisector of ∠BAC; E on AC with ∠ADE=∠BCD; F on AB with ∠ADF=∠CBD.
# X on AC with BX=CX. O1 circumcenter(CDA); O2 circumcenter(EDX).
# Y = BC ∩ EF. Prove O1, O2, Y collinear.

triangle A B C
# D on the angle bisector of ∠BAC (1 DOF along the bisector)
incenter I A B C
on_line D A I
# E on AC such that ∠ADE = ∠BCD (determined)
dep_point E
collinear A E C
eqangle A D E B C D
# F on AB such that ∠ADF = ∠CBD (determined)
dep_point F
collinear A F B
eqangle A D F C B D
# X on AC with BX = CX (determined)
dep_point X
collinear A X C
cong B X C X
circumcenter O1 C D A
circumcenter O2 E D X
inter_ll Y B C E F
prove_collinear O1 O2 Y
