# IMO 2017 P4
# R, S free. T = mirror R through S. O with OR = OS (1 DOF).
# J on circle(O,S). O1 = circumcenter(SJT).
# A on circle(O1,S) with AR perp OR (determined).
# K = circle(O,S) ∩ line(AJ). Prove KT perp O1T.

free R
free S
mirror T R S
# O such that OR = OS (1 DOF: on perpendicular bisector of RS)
semi_free O
cong O R O S
on_circle J O S
circumcenter O1 S J T
# A on circle(O1,S) such that AR perp OR (determined: 2 constraints)
dep_point A
cong O1 A O1 S
perp A R O R
inter_cl K O S A J
prove_perp K T O1 T
