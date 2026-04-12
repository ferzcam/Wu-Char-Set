# IMO 2000 P6
# NOTE: original AlphaGeometry translation is malformed (self-equal angle
# "]AEO = ]AEO"). Best-effort translation drops the degenerate constraint.
# Prove EJ is bisector of <CEF => angle(CEJ) = angle(JEF)

free B
free C
midpoint O B C
on_circle A O B
circumcenter D B A O
# E on circle(O,B) with AE = EO (isoceles triangle AEO)
on_circle E O B
cong A E E O
# F on circle(O,B) with AF = FO (isoceles triangle AFO)
on_circle F O B
cong A F F O
# J on line AC with AD parallel JO
on_line J A C
para A D J O
prove_eqangle C E J J E F
