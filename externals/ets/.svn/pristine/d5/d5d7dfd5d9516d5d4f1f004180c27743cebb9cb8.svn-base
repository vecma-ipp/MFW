set term postscript color solid
set out 'q_ets.ps'
set xlabel 'rho_tor'
set ylabel 'q'
set title "`pwd`"
set key right bottom

plot \
     'eq_ets_data/OUTPUT/OUT0000100.DAT' using 1:9 title '10s',\
     'eq_ets_data/OUTPUT/OUT0000200.DAT' using 1:9 title '20s',\
     'eq_ets_data/OUTPUT/OUT0000300.DAT' using 1:9 title '30s',\
     'eq_ets_data/OUTPUT/OUT0000400.DAT' using 1:9 title '40s',\
     'eq_ets_data/OUTPUT/OUT0000500.DAT' using 1:9 title '50s'
