# imo_2015_p4
# Translated from simplegeometry .gex file.

triangle A B C
circumcenter O A B C
on_line D B C
inter_cl E A D B C
inter_cc F O A A D
# radical-axis form for second intersection of same circles
dep_point G
perp G F O A
cong O G O A
circumcenter O1 F B D
circumcenter O2 G C E
inter_cl K O1 B A B
inter_cl L O2 C A C
dep_point X
para K X K F
para L X L G

prove_collinear A X O
