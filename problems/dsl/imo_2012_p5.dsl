# IMO 2012 P5
# Right triangle at C (CA perp CB). D = foot(C, AB). X on line CD.
# K on circle(B,C) ∩ line AX; L on circle(A,C) ∩ line BX.
# M = AL ∩ BK. Prove KM = LM.
#
# Note: B has 1 DOF (on perpendicular to CA through C), so we use
# semi_free + perp rather than triangle (which would make B fully
# free and the perp constraint becomes a violated constant).

free C
free A
semi_free B
perp C A C B
foot D C A B
on_line X C D
inter_cl K B C A X
inter_cl L A C B X
inter_ll M A L B K
prove_cong K M L M
