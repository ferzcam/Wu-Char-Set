# imo_2019_p6
# Translated from simplegeometry .gex file.

triangle A B C
incenter I A B C
foot F I A B
inter_cl E I F A C
inter_cl D I F B C
dep_point R
perp D R E F
cong I R I F
dep_point P
para R P R A
cong I P I F
circumcenter O1 P C E
circumcenter O2 P B F
inter_cc Q O1 P O2 P
dep_point T
para I T D I
para Q T Q P

prove_perp A T A I
