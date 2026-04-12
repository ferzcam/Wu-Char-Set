# IMO 2008 P6
# Quadrilateral ABCD inscribed in circle (O,R). Common tangent lines
# of circles (I1,|I1F1|) and (I2,|I2F2|) meet at K. Prove OK = OX.

triangle X Y Z
circumcenter O Y X Z
on_circle W O X
free A
perp A X O X
perp A Z O Z
free B
perp B W O W
perp B Z O Z
free C
perp C W O W
perp C Y O Y
free D
perp D X O X
perp D Y O Y
incenter I1 A B C
incenter I2 A C D
foot F1 I1 A C
foot F2 I2 A C
# Common tangent line QT of circles (I1,|I1F1|) and (I2,|I2F2|)
dep_point Q
dep_point T
cong I1 Q I1 F1
perp I1 Q Q T
cong I2 T I2 F2
perp I2 T Q T
# Other common tangent line PS
dep_point P
dep_point S
cong I1 P I1 F1
perp I1 P P S
cong I2 S I2 F2
perp I2 S P S
# K = intersection of the two tangent lines
inter_ll K P S Q T
prove_cong O K O X
