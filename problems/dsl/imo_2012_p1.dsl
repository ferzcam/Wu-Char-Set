# imo_2012_p1
# Translated from simplegeometry .gex file.

triangle A B C
on_pline TEMP1 A C A
on_pline TEMP2 B C B
dep_point J
eqangle TEMP1 A J J A B
eqangle TEMP2 B J J B A
foot K J A B
inter_cl M J K B C
inter_cl L J K A TEMP1
dep_point F
para M F L M
para B F J B
dep_point G
para M G M K
para C G J C
dep_point S
para F S F A
collinear B S TEMP2
dep_point T
para G T G A
para C T C B

prove_midpoint M S T
