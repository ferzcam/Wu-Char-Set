# imo_2016_p1
# Translated from simplegeometry .gex file.

triangle A B Z
dep_point F
eqangle B A F F A Z
cong F A F B
dep_point C
perp B C F B
para A C F A
dep_point D
cong D A D C
collinear A D Z
sym H C A Z
dep_point E
cong E A E D
collinear A E H
midpoint M F C
parallelogram X M A E
inter_ll Y F X E M

prove_collinear Y B D
