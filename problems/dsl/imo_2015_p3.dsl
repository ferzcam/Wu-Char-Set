# IMO 2015 P3
# H = orthocenter. F = AH ∩ BC. M = midpoint BC. O = circumcenter.
# Q on circumcircle with AQ ⊥ HQ. K on circumcircle with HK ⊥ KQ.
# O1 = circumcenter(KHQ), O2 = circumcenter(FMK).
# Prove K, O1, O2 collinear.

triangle A B C
orthocenter H A B C
inter_ll F A H B C
midpoint M B C
circumcenter O C B A
# Q on circumcircle such that AQ ⊥ HQ (determined)
dep_point Q
cong O Q O A
perp A Q H Q
# K on circumcircle such that HK ⊥ KQ (determined)
dep_point K
cong O K O A
perp H K K Q
circumcenter O1 K H Q
circumcenter O2 F M K
prove_collinear K O1 O2
