# IMO 2003 P4
# Let ABCD be a cyclic quadrilateral. Let P, Q, R be feet of perpendiculars
# from D to lines BC, CA, AB respectively. Show that PQ = QR if the bisectors
# of angles ABC and ADC meet on segment AC.
#
# AlphaGeometry translation:
# Let ABC be a triangle. Define O as circumcenter of CBA.
# Define B1 on circle(O,A) such that angle(B1AC) = angle(ACB1).
# Define D1 on circle(O,A) such that angle(ACD1) = angle(D1AC).
# Define X as intersection of lines AC and BB1.
# Define D as intersection of circle(O,A) and line D1X.
# Define P as foot of D on BC.
# Define Q as foot of D on AC.
# Define R as foot of D on AB.
# Prove PQ = QR.

triangle A B C
circumcenter O C B A

# B1 on circle(O,A) with angle constraint
on_circle B1 O A
eqangle B1 A C A C B1

# D1 on circle(O,A) with angle constraint
on_circle D1 O A
eqangle A C D1 D1 A C

# X = intersection of AC and BB1
inter_ll X A C B B1

# D = intersection of circle(O,A) and line D1X
inter_cl D O A D1 X

# Feet of perpendiculars from D
foot P D B C
foot Q D A C
foot R D A B

prove_cong P Q Q R
