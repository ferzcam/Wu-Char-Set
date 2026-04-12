# IMO 2008 P1b
# Orthocenter H. Midpoints D, E, F of sides.
# A1,A2 = circle(D,H) ∩ line BC. B1,B2 similarly on AC. C1,C2 on AB.
# Prove C1 C2 B1 A1 cyclic.

triangle A B C
orthocenter H A B C
midpoint D B C
midpoint E A C
midpoint F A B
inter_cl A1 D H B C
inter_cl A2 D H B C
inter_cl B1 E H A C
inter_cl B2 E H A C
inter_cl C1 F H A B
inter_cl C2 F H A B
prove_cyclic C1 C2 B1 A1
