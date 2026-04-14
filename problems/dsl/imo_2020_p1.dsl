# imo_2020_p1
# Translated from simplegeometry .gex file.
# WARNING: contains unsupported predicates: unknown: ON_ALINE Z A P A B X, unknown: ON_ALINE T P A P A Z, unknown: ON_ALINE D P T P B A, unknown: ON_ALINE U B P B A Y, unknown: ON_ALINE V P B P B U, unknown: ON_ALINE C P V P A B

free Z
free T
free U
free V
triangle P A B
angle_bisector X P B A
angle_bisector Y P A B
# UNSUPPORTED: ON_ALINE Z A P A B X
# UNSUPPORTED: ON_ALINE T P A P A Z
# UNSUPPORTED: ON_ALINE D P T P B A
on_line D A Z
# UNSUPPORTED: ON_ALINE U B P B A Y
# UNSUPPORTED: ON_ALINE V P B P B U
# UNSUPPORTED: ON_ALINE C P V P A B
on_line C B U
dep_point O
eqangle A D O O D P
eqangle P C O O C B

prove_cong O A O B
