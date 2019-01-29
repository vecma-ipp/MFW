set term postscript color solid 20
set out 'eq_test.ps'

# temporary addition until helena2 works
!sed -i~ '/NaN/d' eq_helena2.out
!echo "################################"
!echo "Removed NaNs from eq_helena2.out"
!echo "################################"

set xlabel 'rho_vol'
set key left
set ylabel 'rho_tor'
plot 'eq_test.out' using 7:1 title 'bds','eq_emeq.out'  using 17:1 title 'emeq','eq_helena.out' using 6:(1/0) title 'helena'
# ,'eq_helena2.out' using 15:(1/0) title 'helena2'
set ylabel 'psi'
plot 'eq_test.out' using 7:8 title 'bds','eq_emeq.out'  using 17:(1/0) title 'emeq','eq_helena.out' using 6:4 title 'helena'
# ,'eq_helena2.out' using 15:5 title 'helena2'
set ylabel 'phi'
plot 'eq_test.out' using 7:9 title 'bds','eq_emeq.out'  using 17:(1/0) title 'emeq','eq_helena.out' using 6:12 title 'helena'
# ,'eq_helena2.out' using 15:6 title 'helena2'
set key right
set ylabel 'p'
plot 'eq_test.out' using 7:2 title 'bds','eq_emeq.out'  using 17:2 title 'emeq','eq_helena.out' using 6:1 title 'helena'
# ,'eq_helena2.out' using 15:1 title 'helena2'
set ylabel 'jpar'
plot 'eq_test.out' using 7:3 title 'bds','eq_emeq.out'  using 17:3 title 'emeq','eq_helena.out' using 6:2 title 'helena'
# ,'eq_helena2.out' using 15:2 title 'helena2'
set key bottom
set ylabel 'F_dia'
plot 'eq_test.out' using 7:20 title 'bds','eq_emeq.out'  using 17:16 title 'emeq','eq_helena.out' using 6:7 title 'helena'
# ,'eq_helena2.out' using 15:18 title 'helena2'
set key left
set ylabel "p'"
plot 'eq_test.out' using 7:18 title 'bds','eq_emeq.out'  using 17:(1/0) title 'emeq','eq_helena.out' using 6:8 title 'helena'
# ,'eq_helena2.out' using 15:16 title 'helena2'
set key top
set ylabel "FF'"
plot 'eq_test.out' using 7:19 title 'bds','eq_emeq.out'  using 17:(1/0) title 'emeq','eq_helena.out' using 6:9 title 'helena'
# ,'eq_helena2.out' using 15:17 title 'helena2'
set ylabel 'volume'
plot 'eq_test.out' using 7:5 title 'bds','eq_emeq.out'  using 17:12 title 'emeq','eq_helena.out' using 6:5 title 'helena'
# ,'eq_helena2.out' using 15:3 title 'helena2'
set ylabel 'q'
plot 'eq_test.out' using 7:6 title 'bds','eq_emeq.out'  using 17:4 title 'emeq','eq_helena.out' using 6:3 title 'helena'
# ,'eq_helena2.out' using 15:4 title 'helena2'
set ylabel 'vprime'
plot 'eq_test.out' using 7:10 title 'bds','eq_emeq.out'  using 17:13 title 'emeq','eq_helena.out' using 6:13 title 'helena'
# ,'eq_helena2.out' using 15:7 title 'helena2'
set ylabel 'gm1'
plot 'eq_test.out' using 7:11 title 'bds','eq_emeq.out'  using 17:5 title 'emeq','eq_helena.out' using 6:15 title 'helena'
# ,'eq_helena2.out' using 15:8 title 'helena2'
set ylabel 'gm2'
plot 'eq_test.out' using 7:12 title 'bds','eq_emeq.out'  using 17:6 title 'emeq','eq_helena.out' using 6:15 title 'helena'
# ,'eq_helena2.out' using 15:9 title 'helena2'
set ylabel 'gm3'
plot 'eq_test.out' using 7:13 title 'bds','eq_emeq.out'  using 17:7 title 'emeq','eq_helena.out' using 6:16 title 'helena'
# ,'eq_helena2.out' using 15:10 title 'helena2'
set key right
set ylabel 'gm4'
plot 'eq_test.out' using 7:14 title 'bds','eq_emeq.out'  using 17:8 title 'emeq','eq_helena.out' using 6:17 title 'helena'
# ,'eq_helena2.out' using 15:11 title 'helena2'
set key left
set ylabel 'gm5'
plot 'eq_test.out' using 7:15 title 'bds','eq_emeq.out'  using 17:9 title 'emeq','eq_helena.out' using 6:18 title 'helena'
# ,'eq_helena2.out' using 15:12 title 'helena2'
set ylabel 'gm6'
plot 'eq_test.out' using 7:16 title 'bds','eq_emeq.out'  using 17:10 title 'emeq','eq_helena.out' using 6:19 title 'helena'
# ,'eq_helena2.out' using 15:13 title 'helena2'
set ylabel 'gm7'
plot 'eq_test.out' using 7:17 title 'bds','eq_emeq.out'  using 17:11 title 'emeq','eq_helena.out' using 6:20 title 'helena'
# ,'eq_helena2.out' using 15:14 title 'helena2'
