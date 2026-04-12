# IMO 2011 P6
# Heavy translation: P on (O,A), Q with OP perp PQ; PA,PB,PC,QA,QB,QC
# are second intersections of pairs of equal-radius circles. Triangles
# A1B1C1 built from line intersections; O1 its circumcenter.

triangle A B C
circumcenter O C B A
on_circle P O A
free Q
perp O P P Q
inter_cc PA B P C P
inter_cc PB A P C P
inter_cc PC A P B P
inter_cc QA B Q C Q
inter_cc QB A Q C Q
inter_cc QC A Q B Q
inter_ll A1 PB QB PC QC
inter_ll B1 PA QA PC QC
inter_ll C1 PA QA PB QB
circumcenter O1 B1 A1 C1
inter_cc X O A O1 A1
prove_collinear X O O1
