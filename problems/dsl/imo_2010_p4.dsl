# IMO 2010 P4
# H orthocenter; M = AC ∩ BH; N = AB ∩ CH.
# W on BC; O1 circumcenter(WBN); O2 circumcenter(WCM).
# X = mirror W through O1; Y = mirror W through O2.
# Prove H, X, Y collinear.

triangle A B C
orthocenter H A B C
inter_ll M A C B H
inter_ll N A B C H
on_line W B C
circumcenter O1 W B N
circumcenter O2 W C M
mirror X W O1
mirror Y W O2
prove_collinear H X Y
