# IMO 2010 P4
# S,C free. P on circle(S,C). O with CO ⊥ CS.
# A on circle(O,C). B,M,L,K = other intersections of circle(O,C)
# with lines AS, CP, BP, AP respectively. Prove KM = LM.
#
# Uses mirror trick: for circle(O,C) ∩ line(PQ) where P is trivially
# on both, the midpoint of the chord = foot(O, P, Q), and the other
# intersection = mirror of P through that midpoint.

free S
free C
on_circle P S C
semi_free O
perp C O C S
on_circle A O C
# B = other intersection of circle(O,C) and line AS (A is trivial)
foot M1 O A S
mirror B A M1
# M = other intersection of circle(O,C) and line CP (C is trivial)
foot M2 O C P
mirror M C M2
# L = other intersection of circle(O,C) and line BP
foot M3 O B P
mirror L B M3
# K = other intersection of circle(O,C) and line AP
foot M4 O A P
mirror K A M4
prove_cong K M L M
