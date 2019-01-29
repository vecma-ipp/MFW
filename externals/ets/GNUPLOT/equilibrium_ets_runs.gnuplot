set term postscript color solid 20
set out 'equilibrium_ets_runs.ps'

set style data lp
set xlabel 'rho_tor'

set title 'OUTPUT'
set ylabel 'ne'
plot \
     "case_5_44/eq_ets_data/OUTPUT/OUT0000100.DAT" using 1:3 with lp lt 1 pt 1 title 'Cyl',\
     "case_5_45/eq_ets_data/OUTPUT/OUT0000100.DAT" using 1:3 with lp lt 2 pt 2 title 'BDSEQ',\
     "case_5_46/eq_ets_data/OUTPUT/OUT0000100.DAT" using 1:3 with lp lt 3 pt 3 title 'EMEQ',\
     "case_5_47/eq_ets_data/OUTPUT/OUT0000100.DAT" using 1:3 with lp lt 4 pt 4 title 'HELENA'

set ylabel 'Ti'
plot \
     "case_5_44/eq_ets_data/OUTPUT/OUT0000100.DAT" using 1:4 with lp lt 1 pt 1 title 'Cyl',\
     "case_5_45/eq_ets_data/OUTPUT/OUT0000100.DAT" using 1:4 with lp lt 2 pt 2 title 'BDSEQ',\
     "case_5_46/eq_ets_data/OUTPUT/OUT0000100.DAT" using 1:4 with lp lt 3 pt 3 title 'EMEQ',\
     "case_5_47/eq_ets_data/OUTPUT/OUT0000100.DAT" using 1:4 with lp lt 4 pt 4 title 'HELENA'

set ylabel 'Te'
plot \
     "case_5_44/eq_ets_data/OUTPUT/OUT0000100.DAT" using 1:5 with lp lt 1 pt 1 title 'Cyl',\
     "case_5_45/eq_ets_data/OUTPUT/OUT0000100.DAT" using 1:5 with lp lt 2 pt 2 title 'BDSEQ',\
     "case_5_46/eq_ets_data/OUTPUT/OUT0000100.DAT" using 1:5 with lp lt 3 pt 3 title 'EMEQ',\
     "case_5_47/eq_ets_data/OUTPUT/OUT0000100.DAT" using 1:5 with lp lt 4 pt 4 title 'HELENA'

set ylabel 'q '
plot \
     "case_5_44/eq_ets_data/OUTPUT/OUT0000100.DAT" using 1:9 with lp lt 1 pt 1 title 'Cyl',\
     "case_5_45/eq_ets_data/OUTPUT/OUT0000100.DAT" using 1:9 with lp lt 2 pt 2 title 'BDSEQ',\
     "case_5_46/eq_ets_data/OUTPUT/OUT0000100.DAT" using 1:9 with lp lt 3 pt 3 title 'EMEQ',\
     "case_5_47/eq_ets_data/OUTPUT/OUT0000100.DAT" using 1:9 with lp lt 4 pt 4 title 'HELENA'

set title 'EQOUTPUT'
set ylabel 'q '
plot \
     "case_5_44/eq_ets_data/OUTPUT/EQOUT0000100.DAT" using 1:2 with lp lt 1 pt 1 title 'Cyl',\
     "case_5_45/eq_ets_data/OUTPUT/EQOUT0000100.DAT" using 1:2 with lp lt 2 pt 2 title 'BDSEQ',\
     "case_5_46/eq_ets_data/OUTPUT/EQOUT0000100.DAT" using 1:2 with lp lt 3 pt 3 title 'EMEQ',\
     "case_5_47/eq_ets_data/OUTPUT/EQOUT0000100.DAT" using 1:2 with lp lt 4 pt 4 title 'HELENA'

set ylabel 'Pressure'
plot \
     "case_5_44/eq_ets_data/OUTPUT/EQOUT0000100.DAT" using 1:3 with lp lt 1 pt 1 title 'Cyl',\
     "case_5_45/eq_ets_data/OUTPUT/EQOUT0000100.DAT" using 1:3 with lp lt 2 pt 2 title 'BDSEQ',\
     "case_5_46/eq_ets_data/OUTPUT/EQOUT0000100.DAT" using 1:3 with lp lt 3 pt 3 title 'EMEQ',\
     "case_5_47/eq_ets_data/OUTPUT/EQOUT0000100.DAT" using 1:3 with lp lt 4 pt 4 title 'HELENA'

set ylabel 'jparallel'
plot \
     "case_5_44/eq_ets_data/OUTPUT/EQOUT0000100.DAT" using 1:4 with lp lt 1 pt 1 title 'Cyl',\
     "case_5_45/eq_ets_data/OUTPUT/EQOUT0000100.DAT" using 1:4 with lp lt 2 pt 2 title 'BDSEQ',\
     "case_5_46/eq_ets_data/OUTPUT/EQOUT0000100.DAT" using 1:4 with lp lt 3 pt 3 title 'EMEQ',\
     "case_5_47/eq_ets_data/OUTPUT/EQOUT0000100.DAT" using 1:4 with lp lt 4 pt 4 title 'HELENA'

set ylabel 'volume'
plot \
     "case_5_44/eq_ets_data/OUTPUT/EQOUT0000100.DAT" using 1:12 with lp lt 1 pt 1 title 'Cyl',\
     "case_5_45/eq_ets_data/OUTPUT/EQOUT0000100.DAT" using 1:12 with lp lt 2 pt 2 title 'BDSEQ',\
     "case_5_46/eq_ets_data/OUTPUT/EQOUT0000100.DAT" using 1:12 with lp lt 3 pt 3 title 'EMEQ',\
     "case_5_47/eq_ets_data/OUTPUT/EQOUT0000100.DAT" using 1:12 with lp lt 4 pt 4 title 'HELENA'

set ylabel 'vprime'
plot \
     "case_5_44/eq_ets_data/OUTPUT/EQOUT0000100.DAT" using 1:13 with lp lt 1 pt 1 title 'Cyl',\
     "case_5_45/eq_ets_data/OUTPUT/EQOUT0000100.DAT" using 1:13 with lp lt 2 pt 2 title 'BDSEQ',\
     "case_5_46/eq_ets_data/OUTPUT/EQOUT0000100.DAT" using 1:13 with lp lt 3 pt 3 title 'EMEQ',\
     "case_5_47/eq_ets_data/OUTPUT/EQOUT0000100.DAT" using 1:13 with lp lt 4 pt 4 title 'HELENA'

