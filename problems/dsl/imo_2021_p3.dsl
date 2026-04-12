# IMO 2021 P3
# AD bisector of ∠BAC; E on AC with ∠ADE=∠BCD; F on AB with ∠ADF=∠CBD.
# X on AC with BX=CX and isoceles angle condition.
# O1 circumcenter(CDA); O2 circumcenter(EDX).
# Y = BC ∩ EF. Prove O1, O2, Y collinear.

triangle A B C
free D
eqangle B A D D A C
on_line E A C
eqangle A D E B C D
on_line F A B
eqangle A D F C B D
on_line X A C
cong B X C X
eqangle B C X X B C
circumcenter O1 C D A
circumcenter O2 E D X
inter_ll Y B C E F
prove_collinear O1 O2 Y
