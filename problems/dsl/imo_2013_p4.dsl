# imo_2013_p4
# Translated from simplegeometry .gex file.

triangle A B C
orthocenter H A B C
dep_point N
para H N H C
dep_point M
para H M H B
collinear A M C
collinear A N B
on_line W B C
circumcenter O1 B N W
circumcenter O2 C M W
dep_point X
para O1 X O1 W
cong O1 X O1 B
dep_point Y
para O2 Y O2 W
cong O2 Y O2 C

prove_collinear X H Y
