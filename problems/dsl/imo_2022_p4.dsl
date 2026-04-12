# IMO 2022 P4
# Triangle BCD. E with BC=DE (1 DOF on circle).
# T with BT=DT and CT=ET (determined).
# A with angle(AET)=angle(TBA) (1 DOF).
# P = AB ∩ CD; Q = AB ∩ CT; R = AE ∩ CD; S = AE ∩ DT.
# Prove P, Q, R, S cyclic.

triangle B C D
# E such that BC = DE (1 DOF: on circle centered at D with radius BC)
semi_free E
cong B C D E
# T such that BT = DT and CT = ET (determined: 2 constraints)
dep_point T
cong B T D T
cong C T E T
# A such that angle(AET) = angle(TBA) (1 DOF)
semi_free A
eqangle A E T T B A
inter_ll P A B C D
inter_ll Q A B C T
inter_ll R A E C D
inter_ll S A E D T
prove_cyclic P Q R S
