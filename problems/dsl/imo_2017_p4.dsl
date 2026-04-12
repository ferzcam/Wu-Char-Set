# IMO 2017 P4
# T mirror of R through S. O equidistant from R,S. J on (O,S).
# O1 circumcenter(SJT). A on (O1,S) with AR perp OR.
# K = (O,S) ∩ line AJ. Prove KT perp O1 T.

free R
free S
mirror T R S
free O
cong O R O S
on_circle J O S
circumcenter O1 S J T
on_circle A O1 S
perp A R O R
inter_cl K O S A J
prove_perp K T O1 T
