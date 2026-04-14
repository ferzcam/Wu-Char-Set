# imo_2012_p5
# Translated from simplegeometry .gex file.

free B
free C
on_tline A C C B
foot D C B A
on_line X C D
inter_cl K B C A X
inter_cl L A C B X
inter_ll M B K A L

prove_cong M K M L
