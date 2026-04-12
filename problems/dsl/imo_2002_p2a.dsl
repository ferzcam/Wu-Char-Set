# IMO 2002 P2a
# B, C free. O midpoint of BC. A on circle(O,B).
# D = circumcenter of BAO.
# E on circle(O,B) with AE=EO; F on circle(O,B) with AF=FO.
# J on line AC with AD || JO. Prove EJ bisects angle CEF.

free B
free C
midpoint O B C
on_circle A O B
circumcenter D B A O
# E on circle(O,B) such that AE = EO (determined: 2 constraints)
dep_point E
cong O E O B
cong A E E O
# F on circle(O,B) such that AF = FO (determined: 2 constraints)
dep_point F
cong O F O B
cong A F F O
# J on line AC such that AD parallel JO (determined: 2 constraints)
dep_point J
collinear A J C
para A D J O
prove_eqangle C E J J E F
