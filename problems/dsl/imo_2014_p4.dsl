# imo_2014_p4
# Translated from simplegeometry .gex file.
# WARNING: contains unsupported predicates: unknown: ON_ALINE P A B B C A, unknown: ON_ALINE Q A C C B A

triangle A B C
# UNSUPPORTED: ON_ALINE P A B B C A
on_line P B C
# UNSUPPORTED: ON_ALINE Q A C C B A
on_line Q B C
mirror M A P
mirror N A Q
dep_point X
para M X M B
para N X N C
circumcenter O A B C

prove_cong O X O A
