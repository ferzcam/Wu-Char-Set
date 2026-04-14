# imo_2003_p4
# Translated from simplegeometry .gex file.

triangle A B C
circumcenter O A B C
dep_point B1
cong B1 A B1 C
dep_point D1
cong D1 A D1 C
cong O B1 O A
cong O D1 O A
inter_ll X A C B B1
dep_point D
para X D X D1
cong O D O A
foot P D B C
foot Q D A C
foot R D A B

prove_cong P Q Q R
