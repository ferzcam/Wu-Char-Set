# IMO 2003 P4
# Let ABCD be a cyclic quadrilateral. Let P, Q, R be feet of perpendiculars
# from D to lines BC, CA, AB respectively. Show that PQ = QR if the bisectors
# of angles ABC and ADC meet on segment AC.
#
# AlphaGeometry formulation:
# B1 on circumcircle with angle(B1AC)=angle(ACB1).
# D1 on circumcircle with angle(ACD1)=angle(D1AC).
# X = AC ∩ BB1. D = circumcircle ∩ line(D1,X).
# P,Q,R feet from D. Prove PQ = QR.

triangle A B C
circumcenter O C B A

# B1 on circle(O,A) such that angle(B1AC) = angle(ACB1) (determined)
dep_point B1
cong O B1 O A
eqangle B1 A C A C B1

# D1 on circle(O,A) such that angle(ACD1) = angle(D1AC) (determined)
dep_point D1
cong O D1 O A
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
