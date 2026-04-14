# imo_2000_p6
# Translated from simplegeometry .gex file.

triangle A B C
orthocenter H A B C
incenter I A B C
foot T2 I A C
inter_cl T3 I T2 A B
inter_cl T1 I T2 B C
foot H1 A B C
foot H2 B A C
foot H3 C A B
sym X1 H1 T2 T1
sym X2 H2 T2 T1
sym Y2 H2 T2 T3
sym Y3 H3 T2 T3
dep_point Z
para Y2 Z Y3 Y2
collinear X1 Z X2

prove_cong I Z I T1
