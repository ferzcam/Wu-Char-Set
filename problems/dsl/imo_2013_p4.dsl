# IMO 2013 P4
# H orthocenter; F=AH∩BC; M midpoint BC; O circumcenter.
# Q on (O,A) with AQ perp HQ; K on (O,A) with HK perp KQ.
# O1 circumcenter(KHQ); O2 circumcenter(FMK).
# Prove K, O1, O2 collinear.

triangle A B C
orthocenter H A B C
inter_ll F A H B C
midpoint M B C
circumcenter O C B A
on_circle Q O A
perp A Q H Q
on_circle K O A
perp H K K Q
circumcenter O1 K H Q
circumcenter O2 F M K
prove_collinear K O1 O2
