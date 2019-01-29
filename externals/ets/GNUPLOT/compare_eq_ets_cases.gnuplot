set term postscript color solid 24
set out 'compare_eq_ets_cases.ps'
set xlabel 'Time (s)'
set ylabel 'Ni(0)'
plot \
     "< head -1 -q case_5_44/eq_ets_data/OUTPUT/OUT*.DAT | nl" using (($1-1)*1e-1):($3) title "CYL",\
     "< head -1 -q case_5_45/eq_ets_data/OUTPUT/OUT*.DAT | nl" using (($1-1)*1e-1):($3) title "BDSEQ",\
     "< head -1 -q case_5_46/eq_ets_data/OUTPUT/OUT*.DAT | nl" using (($1-1)*1e-1):($3) title "EMEQ",\
     "< head -1 -q case_5_47/eq_ets_data/OUTPUT/OUT*.DAT | nl" using (($1-1)*1e-1):($3) title "HELENA"
set ylabel 'Ti(0)'
plot \
     "< head -1 -q case_5_44/eq_ets_data/OUTPUT/OUT*.DAT | nl" using (($1-1)*1e-1):($5) title "CYL",\
     "< head -1 -q case_5_45/eq_ets_data/OUTPUT/OUT*.DAT | nl" using (($1-1)*1e-1):($5) title "BDSEQ",\
     "< head -1 -q case_5_46/eq_ets_data/OUTPUT/OUT*.DAT | nl" using (($1-1)*1e-1):($5) title "EMEQ",\
     "< head -1 -q case_5_47/eq_ets_data/OUTPUT/OUT*.DAT | nl" using (($1-1)*1e-1):($5) title "HELENA"
set ylabel 'Te(0)'
plot \
     "< head -1 -q case_5_44/eq_ets_data/OUTPUT/OUT*.DAT | nl" using (($1-1)*1e-1):($6) title "CYL",\
     "< head -1 -q case_5_45/eq_ets_data/OUTPUT/OUT*.DAT | nl" using (($1-1)*1e-1):($6) title "BDSEQ",\
     "< head -1 -q case_5_46/eq_ets_data/OUTPUT/OUT*.DAT | nl" using (($1-1)*1e-1):($6) title "EMEQ",\
     "< head -1 -q case_5_47/eq_ets_data/OUTPUT/OUT*.DAT | nl" using (($1-1)*1e-1):($6) title "HELENA"
set ylabel 'Vtor(0)'
plot \
     "< head -1 -q case_5_44/eq_ets_data/OUTPUT/OUT*.DAT | nl" using (($1-1)*1e-1):($7) title "CYL",\
     "< head -1 -q case_5_45/eq_ets_data/OUTPUT/OUT*.DAT | nl" using (($1-1)*1e-1):($7) title "BDSEQ",\
     "< head -1 -q case_5_46/eq_ets_data/OUTPUT/OUT*.DAT | nl" using (($1-1)*1e-1):($7) title "EMEQ",\
     "< head -1 -q case_5_47/eq_ets_data/OUTPUT/OUT*.DAT | nl" using (($1-1)*1e-1):($7) title "HELENA"
set ylabel 'Psi(0)'
plot \
     "< head -1 -q case_5_44/eq_ets_data/OUTPUT/OUT*.DAT | nl" using (($1-1)*1e-1):($8) title "CYL",\
     "< head -1 -q case_5_45/eq_ets_data/OUTPUT/OUT*.DAT | nl" using (($1-1)*1e-1):($8) title "BDSEQ",\
     "< head -1 -q case_5_46/eq_ets_data/OUTPUT/OUT*.DAT | nl" using (($1-1)*1e-1):($8) title "EMEQ",\
     "< head -1 -q case_5_47/eq_ets_data/OUTPUT/OUT*.DAT | nl" using (($1-1)*1e-1):($8) title "HELENA"
set ylabel 'jtot(0)'
plot \
     "< head -1 -q case_5_44/eq_ets_data/OUTPUT/OUT*.DAT | nl" using (($1-1)*1e-1):($9) title "CYL",\
     "< head -1 -q case_5_45/eq_ets_data/OUTPUT/OUT*.DAT | nl" using (($1-1)*1e-1):($9) title "BDSEQ",\
     "< head -1 -q case_5_46/eq_ets_data/OUTPUT/OUT*.DAT | nl" using (($1-1)*1e-1):($9) title "EMEQ",\
     "< head -1 -q case_5_47/eq_ets_data/OUTPUT/OUT*.DAT | nl" using (($1-1)*1e-1):($9) title "HELENA"
set ylabel 'q(0)'
plot \
     "< head -1 -q case_5_44/eq_ets_data/OUTPUT/OUT*.DAT | nl" using (($1-1)*1e-1):($10) title "CYL",\
     "< head -1 -q case_5_45/eq_ets_data/OUTPUT/OUT*.DAT | nl" using (($1-1)*1e-1):($10) title "BDSEQ",\
     "< head -1 -q case_5_46/eq_ets_data/OUTPUT/OUT*.DAT | nl" using (($1-1)*1e-1):($10) title "EMEQ",\
     "< head -1 -q case_5_47/eq_ets_data/OUTPUT/OUT*.DAT | nl" using (($1-1)*1e-1):($10) title "HELENA"
