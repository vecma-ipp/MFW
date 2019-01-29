set term postscript color solid 20
set out 'eq_ets_test.ps'

set style data lp

set title '4.06d; dy in "current" from equations; sigma_source=0' 
set title '4.06d; dy from solver; sigma_source=0' 
set title '4.06d; dy from solver with corrections; sigma_source=0' 
set title '4.07a; dy in "current" from equations; sigma_source=0' 

set xlabel 'rho_tor'

set ylabel 'q'
plot 'eq_ets_data/OUTPUT/EQOUT0000000.DAT' using 1:2 title '000',\
     'eq_ets_data/OUTPUT/EQOUT0000050.DAT' using 1:2 title '050',\
     'eq_ets_data/OUTPUT/EQOUT0000100.DAT' using 1:2 title '100'

set ylabel 'pressure'
plot 'eq_ets_data/OUTPUT/EQOUT0000000.DAT' using 1:3 title '000',\
     'eq_ets_data/OUTPUT/EQOUT0000050.DAT' using 1:3 title '050',\
     'eq_ets_data/OUTPUT/EQOUT0000100.DAT' using 1:3 title '100'
