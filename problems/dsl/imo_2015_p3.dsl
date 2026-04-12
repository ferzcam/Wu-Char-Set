# IMO 2015 P3
# D on BC; E = circle(A,D) ∩ BC (second); F,G = circle(A,D) ∩ (O,A).
# O1 circumcenter(FBD); O2 circumcenter(ECG).
# K = (O1,B) ∩ line AB; L = (O2,C) ∩ line AC.
# X = FK ∩ GL. Prove A, O, X collinear.

triangle A B C
circumcenter O C B A
on_line D B C
inter_cl E A D B C
inter_cc F A D O A
inter_cc G A D O A
circumcenter O1 F B D
circumcenter O2 E C G
inter_cl K O1 B A B
inter_cl L O2 C A C
inter_ll X F K G L
prove_collinear A O X
