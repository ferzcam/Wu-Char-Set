# IMO 2004 P5a
# Define D such that AD = BC (we use parallelogram ABCD so AD || BC and |AD|=|BC|).
# E on line BC, F on line AD with BE = DF.
# P, Q, R intersections. M intersection of circles. Prove MPQR cyclic.

triangle A B C
parallelogram D A B C
on_line E B C
on_line F A D
cong B E D F
inter_ll P A C B D
inter_ll Q B D E F
inter_ll R A C E F
circumcenter O1 P D A
circumcenter O2 C B P
inter_cc M O1 P O2 P
prove_cyclic M P Q R
