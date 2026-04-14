# imo_2000_p1
# Translated from simplegeometry .gex file.

free A
free B
on_tline G1 A B A
on_tline G2 B B A
inter_cc M G1 A G2 B
# radical-axis form for second intersection of same circles
dep_point N
perp N M G1 G2
cong G1 N G1 A
dep_point C
para M C B A
cong G1 C G1 A
dep_point D
para M D B A
cong G2 D G2 B
dep_point E
para A E C A
para B E D B
on_pline X C D C
inter_ll P A N C X
inter_ll Q B N C X

prove_cong E P E Q
