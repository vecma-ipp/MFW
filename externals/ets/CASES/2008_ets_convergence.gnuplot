#! /usr/bin/gnuplot
set term postscript enhanced color solid lw 5 20
set pointsize 2

set style data lp
set log xy

set out '2008_ets_convergence_dx.ps'
set key right bottom width -13
set xrange [0.0008:0.3]
set xlabel '1/NX'
set title 'Relative density deviation at {/Symbol r}=0.5 for solver 3'
plot \
    "< awk '$1==3&&$2==1e-1{print 1/$3,sqrt((($5-$6)/$6)**2)}' SUMMARY" title '{/Symbol d}t=10^{-1}',\
    "< awk '$1==3&&$2==1e-2{print 1/$3,sqrt((($5-$6)/$6)**2)}' SUMMARY" title '{/Symbol d}t=10^{-2}',\
    "< awk '$1==3&&$2==1e-3{print 1/$3,sqrt((($5-$6)/$6)**2)}' SUMMARY" title '{/Symbol d}t=10^{-3}',\
    "< awk '$1==3&&$2==1e-4{print 1/$3,sqrt((($5-$6)/$6)**2)}' SUMMARY" title '{/Symbol d}t=10^{-4}'

set out '2008_ets_convergence_dx_dt=1e-4.ps'
set key left top width -13
set xrange [0.0008:0.3]
set xlabel '1/NX'
set title 'Relative density deviation at {/Symbol r}=0.5 for solver 1, 3 & 4'
plot \
    "< awk '$1==1&&$2==1e-4{print 1/$3,sqrt((($5-$6)/$6)**2)}' SUMMARY" title 'S=1 {/Symbol d}t=10^{-4}',\
    "< awk '$1==3&&$2==1e-4{print 1/$3,sqrt((($5-$6)/$6)**2)}' SUMMARY" title 'S=3 {/Symbol d}t=10^{-4}',\
    "< awk '$1==4&&$2==1e-4{print 1/$3,sqrt((($5-$6)/$6)**2)}' SUMMARY" title 'S=4 {/Symbol d}t=10^{-4}'


set out '2008_ets_convergence_dt.ps'
set key top left width 0
set xrange [0.00008:0.12]
set xlabel '{/Symbol d}t'
set title 'Relative density deviation at {/Symbol r}=0.5 for solver 3'
plot \
    "< awk '$1==3&&$3==65{print $2,sqrt((($5-$6)/$6)**2)}'   SUMMARY" title 'NX=65',\
    "< awk '$1==3&&$3==129{print $2,sqrt((($5-$6)/$6)**2)}'  SUMMARY" title 'NX=129',\
    "< awk '$1==3&&$3==257{print $2,sqrt((($5-$6)/$6)**2)}'  SUMMARY" title 'NX=257',\
    "< awk '$1==3&&$3==513{print $2,sqrt((($5-$6)/$6)**2)}'  SUMMARY" title 'NX=513',\
    "< awk '$1==3&&$3==1025{print $2,sqrt((($5-$6)/$6)**2)}' SUMMARY" title 'NX=1025'

set out '2008_ets_convergence_dt_nx=1025.ps'
set key top left width 0
set xrange [0.00008:0.12]
set xlabel '{/Symbol d}t'
set title 'Relative density deviation at {/Symbol r}=0.5 for solver 1, 3 & 4'
plot \
    "< awk '$1==1&&$3==1025{print $2,sqrt((($5-$6)/$6)**2)}' SUMMARY" title 'S=1 NX=1025',\
    "< awk '$1==3&&$3==1025{print $2,sqrt((($5-$6)/$6)**2)}' SUMMARY" title 'S=3 NX=1025',\
    "< awk '$1==4&&$3==1025{print $2,sqrt((($5-$6)/$6)**2)}' SUMMARY" title 'S=4 NX=1025'
set nolog y
