# IMO 2008 P1A
# Let H be the orthocenter of triangle ABC. Circles centered at midpoints
# of sides, passing through H, intersect the sides. Prove B1,B2,C1,C2 cyclic.
#
# AlphaGeometry translation:
# Let ABC be a triangle. Define H such that AH perp BC.
# Define E = midpoint(AC), F = midpoint(AB).
# Define B1 = intersection of circle(E,H) and line AC.
# Define B2 = intersection of circle(E,H) and line AC. (second intersection)
# Define C1 = intersection of circle(F,H) and line AB.
# Define C2 = intersection of circle(F,H) and line AB. (second intersection)
# Prove B1,B2,C1,C2 are cyclic.

triangle A B C
orthocenter H A B C
midpoint E A C
midpoint F A B

# B1, B2 = intersections of circle(E,H) with line AC
inter_cl B1 E H A C
inter_cl B2 E H A C

# C1, C2 = intersections of circle(F,H) with line AB
inter_cl C1 F H A B
inter_cl C2 F H A B

prove_cyclic B1 B2 C1 C2
