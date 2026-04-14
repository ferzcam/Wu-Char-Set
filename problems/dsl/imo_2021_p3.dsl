# imo_2021_p3
# Translated from simplegeometry .gex file.
# WARNING: contains unsupported predicates: unknown: ON_ALINE E D A D C B, unknown: ON_ALINE F D A D B C

triangle A B C
angle_bisector D B A C
# UNSUPPORTED: ON_ALINE E D A D C B
on_line E A C
# UNSUPPORTED: ON_ALINE F D A D B C
on_line F A B
dep_point X
cong X B X C
collinear A X C
circumcenter O1 A D C
circumcenter O2 E X D
dep_point Y
para B Y C B
para F Y F E

prove_collinear Y O1 O2
