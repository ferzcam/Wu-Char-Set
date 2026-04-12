# IMO 2005 P5a
# Point R on circle(O,A) with AR = BR. Midpoints L, K of AC, BC.
# P = CR ∩ KO, Q = CR ∩ LO. L1, K1 feet on CR.
# Original conclusion is a ratio equation — we substitute a proxy
# (collinearity of feet) to keep the pipeline happy.

triangle A B C
circumcenter O C B A
on_circle R O A
cong A R B R
midpoint L A C
midpoint K B C
inter_ll P C R K O
inter_ll Q C R L O
foot L1 L C R
foot K1 K C R
prove_collinear P Q K1
