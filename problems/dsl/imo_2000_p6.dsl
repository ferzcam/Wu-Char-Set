# IMO 2000 P6
# Incenter I of ABC. T1,T2,T3 = feet of I on sides.
# H1,H2,H3 = feet of altitudes. Circle-circle intersections
# X1,X2,Y2,Y3. Z = X1X2 ∩ Y2Y3. Prove T1I = IZ.

triangle A B C
incenter I A B C
foot T1 I B C
foot T2 I A C
foot T3 I A B
foot H1 A B C
foot H2 B A C
foot H3 C A B
inter_cc X1 T1 H1 T2 H1
inter_cc X2 T1 H2 T2 H2
inter_cc Y2 T2 H2 T3 H2
inter_cc Y3 T2 H3 T3 H3
inter_ll Z X1 X2 Y2 Y3
prove_cong T1 I I Z
