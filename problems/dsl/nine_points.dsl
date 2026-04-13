# Nine-point circle (4 points sub-case)
# A, B, C triangle. D = foot of perpendicular from A to BC.
# E = midpoint of BA, F = midpoint of CB, G = midpoint of AC.
# Prove: D, G, E, F are concyclic.
#
# This mirrors the construction Java maths.CharSet handles quickly:
# characteristic set is linear in the class variable at every step,
# pseudoremainder chain stays under 40 terms throughout.

triangle A B C
foot D A B C
midpoint E B A
midpoint F C B
midpoint G A C
prove_cyclic D G E F
