# IMO 2002 P2a
# B, C free. O midpoint of BC. A on circle(O,B).
# D = circumcenter of BAO, E, F on circle(O,B) with isoceles conditions.
# J on line AC with AD || JO. Prove EJ bisects <CEF.

free B
free C
midpoint O B C
on_circle A O B
circumcenter D B A O
on_circle E O B
cong A E E O
on_circle F O B
cong A F F O
on_line J A C
para A D J O
prove_eqangle C E J J E F
