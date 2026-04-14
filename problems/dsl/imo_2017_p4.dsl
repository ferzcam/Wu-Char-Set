# imo_2017_p4
# Translated from simplegeometry .gex file.

free R
free S
mirror T R S
on_bline O R S
on_circle J O S
circumcenter O1 J S T
dep_point A
perp R A O R
cong O1 A O1 S
dep_point B
para A B A R
cong O1 B O1 S
dep_point K
para J K A J
cong O K O S

prove_perp K T O1 T
