set term postscript color solid 20
set out 'compare_convective_heat_transport.ps'

set style data lp
set xlabel 'rho_tor'

set title 'TOROIDAL'
set ylabel 'ne'
plot \
     "TOROIDAL_0.0/eq_ets_data/OUTPUT/OUT0000500.DAT" using 1:3 with points lt 1 pt 1 title '0.0',\
     "TOROIDAL_1.5/eq_ets_data/OUTPUT/OUT0000500.DAT" using 1:3 with points lt 2 pt 1 title '1.5',\
     "TOROIDAL_2.5/eq_ets_data/OUTPUT/OUT0000500.DAT" using 1:3 with points lt 3 pt 1 title '2.5'

set ylabel 'Ti'
plot \
     "TOROIDAL_0.0/eq_ets_data/OUTPUT/OUT0000500.DAT" using 1:4 with points lt 1 pt 1 title '0.0',\
     "TOROIDAL_1.5/eq_ets_data/OUTPUT/OUT0000500.DAT" using 1:4 with points lt 2 pt 1 title '1.5',\
     "TOROIDAL_2.5/eq_ets_data/OUTPUT/OUT0000500.DAT" using 1:4 with points lt 3 pt 1 title '2.5'

set ylabel 'Te'
plot \
     "TOROIDAL_0.0/eq_ets_data/OUTPUT/OUT0000500.DAT" using 1:5 with points lt 1 pt 1 title '0.0',\
     "TOROIDAL_1.5/eq_ets_data/OUTPUT/OUT0000500.DAT" using 1:5 with points lt 2 pt 1 title '1.5',\
     "TOROIDAL_2.5/eq_ets_data/OUTPUT/OUT0000500.DAT" using 1:5 with points lt 3 pt 1 title '2.5'

set ylabel 'q '
plot \
     "TOROIDAL_0.0/eq_ets_data/OUTPUT/OUT0000500.DAT" using 1:9 with points lt 1 pt 1 title '0.0',\
     "TOROIDAL_1.5/eq_ets_data/OUTPUT/OUT0000500.DAT" using 1:9 with points lt 2 pt 1 title '1.5',\
     "TOROIDAL_2.5/eq_ets_data/OUTPUT/OUT0000500.DAT" using 1:9 with points lt 3 pt 1 title '2.5'

set title 'CYLINDRICAL'
set ylabel 'ne'
plot \
     "CYLINDRICAL_0.0/eq_ets_data/OUTPUT/OUT0000500.DAT" using 1:3 with points lt 1 pt 1 title '0.0',\
     "CYLINDRICAL_1.5/eq_ets_data/OUTPUT/OUT0000500.DAT" using 1:3 with points lt 2 pt 1 title '1.5',\
     "CYLINDRICAL_2.5/eq_ets_data/OUTPUT/OUT0000500.DAT" using 1:3 with points lt 3 pt 1 title '2.5'

set ylabel 'Ti'
plot \
     "CYLINDRICAL_0.0/eq_ets_data/OUTPUT/OUT0000500.DAT" using 1:4 with points lt 1 pt 1 title '0.0',\
     "ASTRA_CYLINDRICAL/0.0.dat" using ($1*2):($2*1000) with points lt 1 pt 2 title '0.0 ASTRA',\
     "CYLINDRICAL_1.5/eq_ets_data/OUTPUT/OUT0000500.DAT" using 1:4 with points lt 2 pt 1 title '1.5',\
     "ASTRA_CYLINDRICAL/1.5.dat" using ($1*2):($2*1000) with points lt 2 pt 2 title '1.5 ASTRA',\
     "CYLINDRICAL_2.5/eq_ets_data/OUTPUT/OUT0000500.DAT" using 1:4 with points lt 3 pt 1 title '2.5',\
     "ASTRA_CYLINDRICAL/2.5.dat" using ($1*2):($2*1000) with points lt 3 pt 2 title '2.5 ASTRA'

set ylabel 'Te'
plot \
     "CYLINDRICAL_0.0/eq_ets_data/OUTPUT/OUT0000500.DAT" using 1:5 with points lt 1 pt 1 title '0.0',\
     "ASTRA_CYLINDRICAL/0.0.dat" using ($1*2):($2*1000) with points lt 1 pt 2 title '0.0 ASTRA',\
     "CYLINDRICAL_1.5/eq_ets_data/OUTPUT/OUT0000500.DAT" using 1:5 with points lt 2 pt 1 title '1.5',\
     "ASTRA_CYLINDRICAL/1.5.dat" using ($1*2):($2*1000) with points lt 2 pt 2 title '1.5 ASTRA',\
     "CYLINDRICAL_2.5/eq_ets_data/OUTPUT/OUT0000500.DAT" using 1:5 with points lt 3 pt 1 title '2.5',\
     "ASTRA_CYLINDRICAL/2.5.dat" using ($1*2):($2*1000) with points lt 3 pt 2 title '2.5 ASTRA'

set ylabel 'q '
plot \
     "CYLINDRICAL_0.0/eq_ets_data/OUTPUT/OUT0000500.DAT" using 1:9 with points lt 1 pt 1 title '0.0',\
     "CYLINDRICAL_1.5/eq_ets_data/OUTPUT/OUT0000500.DAT" using 1:9 with points lt 2 pt 1 title '1.5',\
     "CYLINDRICAL_2.5/eq_ets_data/OUTPUT/OUT0000500.DAT" using 1:9 with points lt 3 pt 1 title '2.5'
