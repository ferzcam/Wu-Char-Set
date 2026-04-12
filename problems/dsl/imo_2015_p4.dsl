# IMO 2015 P4 (from AlphaGeometry formulation)
# D on BC. E = other intersection of circle(A,D) and line BC.
# F,G = two intersections of circles (A,D) and (O,A).
# O1 = circumcenter(FBD), O2 = circumcenter(ECG).
# K = other intersection of circle(O1,B) and line AB.
# L = other intersection of circle(O2,C) and line AC.
# X = FK ∩ GL. Prove A, O, X collinear.
#
# Uses mirror trick for trivial intersections and for
# getting both circle-circle intersection points.

triangle A B C
circumcenter O C B A
on_line D B C
# E = other intersection of circle(A,D) and line BC
# (D is trivially on both; midpoint of DE = foot of A on BC)
foot MDE A B C
mirror E D MDE
# F = one intersection of circles (A,D) and (O,A)
inter_cc F A D O A
# G = other intersection (mirror F through line of centers AO)
foot HF F A O
mirror G F HF
circumcenter O1 F B D
circumcenter O2 E C G
# K = other intersection of circle(O1,B) and line AB (B is trivial)
foot MBK O1 A B
mirror K B MBK
# L = other intersection of circle(O2,C) and line AC (C is trivial)
foot MCL O2 A C
mirror L C MCL
inter_ll X F K G L
prove_collinear A O X
