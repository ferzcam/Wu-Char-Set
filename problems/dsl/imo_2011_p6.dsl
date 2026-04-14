# imo_2011_p6
# Translated from simplegeometry .gex file.

triangle A B C
circumcenter O A B C
on_circle P O A
on_tline Q P P O
sym F P B C
sym G Q B C
sym H P A C
sym I Q A C
sym J P A B
sym K Q A B
dep_point A1
para J A1 K J
dep_point C1
para F C1 G F
para H A1 I H
inter_ll B1 F G H I
collinear J C1 K
circumcenter O1 A1 C1 B1
inter_cc X O A O1 A1

prove_collinear O X O1
