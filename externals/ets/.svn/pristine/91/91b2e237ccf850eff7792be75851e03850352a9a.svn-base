set term postscript color solid
set out 'uloop_ets.ps'
set xlabel 'rho_tor'
set ylabel 'uloop'
set title "`pwd`"
set key right bottom

plot \
     "< paste eq_ets_data/OUTPUT/OUT0000099.DAT eq_ets_data/OUTPUT/OUT0000100.DAT | awk '{print $1,($17-$7)/0.1}'" title '10s',\
     "< paste eq_ets_data/OUTPUT/OUT0000199.DAT eq_ets_data/OUTPUT/OUT0000200.DAT | awk '{print $1,($17-$7)/0.1}'" title '20s',\
     "< paste eq_ets_data/OUTPUT/OUT0000299.DAT eq_ets_data/OUTPUT/OUT0000300.DAT | awk '{print $1,($17-$7)/0.1}'" title '30s',\
     "< paste eq_ets_data/OUTPUT/OUT0000399.DAT eq_ets_data/OUTPUT/OUT0000400.DAT | awk '{print $1,($17-$7)/0.1}'" title '40s',\
     "< paste eq_ets_data/OUTPUT/OUT0000499.DAT eq_ets_data/OUTPUT/OUT0000500.DAT | awk '{print $1,($17-$7)/0.1}'" title '50s'
