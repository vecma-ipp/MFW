set term postscript color solid
set out 'q.ps'
set xlabel 'rho_tor'
set ylabel 'q'
set title "`pwd`"
set key left

plot \
     'eq_ets_data/OUTPUT/EQOUT0000499.DAT' using 1:2,\
     'eq_ets_data/OUTPUT/EQOUT0000500.DAT' using 1:2,\
     'eq_ets_data/OUTPUT/OUT0000499.DAT' using 1:9,\
     'eq_ets_data/OUTPUT/OUT0000500.DAT' using 1:9
