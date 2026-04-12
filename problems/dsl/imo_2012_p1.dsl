# IMO 2012 P1
# J is the incenter (A- and C-angle bisectors).
# M,L,K feet of J on BC, AC, AB.
# F = BJ ∩ LM, G = CJ ∩ KM; S,T on BC via AF,AG.
# Prove MS = MT.

triangle A B C
incenter J A B C
foot M J B C
foot L J A C
foot K J A B
inter_ll F B J L M
inter_ll G C J K M
inter_ll S A F B C
inter_ll T A G B C
prove_cong M S M T
