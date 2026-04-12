# IMO 2018 P1
# D on AB; E = circle(A,D) ∩ line AC.
# F on circle(O,A) such that angle(BDF) = angle(FBD) (determined).
# G on circle(O,A) such that angle(CEG) = angle(GCE) (determined).
# Prove DE parallel FG.

triangle A B C
circumcenter O C B A
on_line D A B
inter_cl E A D A C
# F on circumcircle with isoceles condition BF=DF (equivalent to angle(BDF)=angle(FBD))
dep_point F
cong O F O A
cong B F D F
# G on circumcircle with isoceles condition CG=EG (equivalent to angle(CEG)=angle(GCE))
dep_point G
cong O G O A
cong C G E G
prove_para D E F G
