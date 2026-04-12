# IMO 2014 P4
# P on BC such that angle(BAC) = angle(AP,BC).
# Q on BC such that angle(ABC) = angle(CAQ).
# M = mirror A through P; N = mirror A through Q.
# X = BM ∩ CN. O circumcenter. Prove AO = XO.

triangle A B C
# P on BC determined by angle condition
dep_point P
collinear B P C
eqangle B A C A P B
# Q on BC determined by angle condition
dep_point Q
collinear B Q C
eqangle A B C C A Q
mirror M A P
mirror N A Q
inter_ll X B M C N
circumcenter O C B A
prove_cong A O X O
