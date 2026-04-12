# IMO 2002 P2b
# NOTE: the AlphaGeometry text defines E and F as the SAME point
# (both "circumcenter of BAO"). Likely corruption; we keep both
# to preserve the pipeline but this problem will be degenerate.

free B
free C
on_line O B C
cong B O C O
on_circle A O B
circumcenter E B A O
circumcenter F B A O
on_line J A C
prove_eqangle E C J J C F
