# IMO 2007 P4
# R on circumcircle with AR=BR. L,K midpoints. P = CR ∩ KO; Q = CR ∩ LO.
# L1 = foot(L, CR); K1 = foot(K, CR).
# Prove KK1/LL1 = RQ/RP.
#
# NOTE: The conclusion is a ratio equality which cannot be expressed
# in the current DSL (only cong/collinear/cyclic/para/perp/eqangle).
# This problem is SKIPPED — it will always FAIL.

triangle A B C
circumcenter O C B A
dep_point R
cong O R O A
cong A R B R
midpoint L A C
midpoint K B C
inter_ll P C R K O
inter_ll Q C R L O
foot L1 L C R
foot K1 K C R
# Cannot express prove_ratio KK1 LL1 RQ RP
# Placeholder conclusion that will fail:
prove_cong K K1 L L1
