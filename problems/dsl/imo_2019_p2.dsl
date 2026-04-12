# IMO 2019 P2
# A1 on BC, B1 on AC. P on AA1; Q on BB1 with AB parallel PQ.
# P1 on B1P with angle BAC = angle PP1C.
# Q1 on A1Q with angle ABC = angle QQ1C.
# Prove P, P1, Q, Q1 cyclic.

triangle A B C
on_line A1 B C
on_line B1 A C
on_line P A A1
# Q is determined by: on BB1 and AB parallel PQ
dep_point Q
collinear B Q B1
para A B P Q
# P1 is determined by: on B1P and angle BAC = angle PP1C
dep_point P1
collinear B1 P1 P
eqangle B A C P P1 C
# Q1 is determined by: on A1Q and angle ABC = angle QQ1C
dep_point Q1
collinear A1 Q1 Q
eqangle A B C Q Q1 C
prove_cyclic P P1 Q Q1
