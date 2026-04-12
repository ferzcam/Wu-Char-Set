# IMO 2015 P4
# Angle-chase heavy; translation is a compressed best-effort.
# Chain: F with AF=BF and bisector; C on AF with BC perp BF;
# D on AZ with AD=CD; E analogous. M midpoint CF; X via parallelogram;
# Y = EM ∩ FX. Prove B, D, Y collinear.

triangle A B Z
free F
cong A F B F
eqangle B A F F A Z
on_line C A F
perp B C B F
on_line D A Z
cong A D C D
eqangle A C D D A C
free E
cong A E D E
eqangle C A D D A E
midpoint M C F
free X
para A E M X
para A M E X
inter_ll Y E M F X
prove_collinear B D Y
