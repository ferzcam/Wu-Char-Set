# imo_2004_p1
# Translated from simplegeometry .gex file.

free A
free B
free C
midpoint O C B
inter_cl M O B A B
inter_cl N O B A C
dep_point R
eqangle C A R R A B
eqangle M O R R O N
circumcenter O1 B M R
circumcenter O2 C N R
inter_cc P O1 R O2 R

prove_collinear P B C
