# IMO 2022 P4
# E with BC=DE. T with BT=DT and CT=ET. A with ∠AET = ∠TBA.
# P = AB ∩ CD; Q = AB ∩ CT; R = AE ∩ CD; S = AE ∩ DT.
# Prove P, Q, R, S cyclic.

triangle B C D
free E
cong B C D E
free T
cong B T D T
cong C T E T
free A
eqangle A E T T B A
inter_ll P A B C D
inter_ll Q A B C T
inter_ll R A E C D
inter_ll S A E D T
prove_cyclic P Q R S
