# IMO 2020 P1
# Nested angle-chase construction; translation is approximate.
# BX, AY angle bisectors; Z, T, D, U, V, C defined via angle equalities;
# O with DO bisecting ∠PDA and CO bisecting ∠PCB.
# Prove AO = BO.

triangle P A B
free X
eqangle P B X X B A
free Y
eqangle P A Y Y A B
free Z
eqangle P A Z X B A
free T
eqangle P A Z T P A
on_line D A Z
eqangle P B A D P T
free U
eqangle P B U Y A B
free V
eqangle P B U V P B
on_line C B U
eqangle P A B C P V
free O
eqangle P D O O D A
eqangle P C O O C B
prove_cong A O B O
