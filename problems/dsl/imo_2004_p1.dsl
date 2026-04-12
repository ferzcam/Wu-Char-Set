# IMO 2004 P1
# Quadrilateral ABCO. M = circle(O,B) ∩ line AB; N = circle(O,B) ∩ line AC.
# R: AR bisects ∠BAC and OR bisects ∠MON.
# O1 = circumcenter(RBM), O2 = circumcenter(RCN).
# P = circle(O1,R) ∩ circle(O2,R). Prove B,C,P collinear.

triangle A B C
free O
inter_cl M O B A B
inter_cl N O B A C
dep_point R
eqangle B A R R A C
eqangle M O R R O N
circumcenter O1 R B M
circumcenter O2 R C N
inter_cc P O1 R O2 R
prove_collinear B C P
