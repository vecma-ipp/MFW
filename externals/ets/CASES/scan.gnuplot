#! /usr/bin/gnuplot
set term postscript enhanced color solid 20
set out 'scan.ps'

set style data lp
set log x

set xrange [0.0008:0.3]
set xlabel '1/NX'
set title 'Density at {/Symbol r}=0.5 for different solvers and time-steps'
set key left bottom width -13
plot \
    0.85E+19 lw 5,\
    "< awk '$1==1&&$2==1e-1{print 1/$3,$5}' SUMMARY" title 'S=1 {/Symbol d}t=10^{-1}',\
    "< awk '$1==1&&$2==1e-2{print 1/$3,$5}' SUMMARY" title 'S=1 {/Symbol d}t=10^{-2}',\
    "< awk '$1==1&&$2==1e-3{print 1/$3,$5}' SUMMARY" title 'S=1 {/Symbol d}t=10^{-3}',\
    "< awk '$1==1&&$2==1e-4{print 1/$3,$5}' SUMMARY" title 'S=1 {/Symbol d}t=10^{-4}',\
    "< awk '$1==2&&$2==1e-1{print 1/$3,$5}' SUMMARY" title 'S=2 {/Symbol d}t=10^{-1}',\
    "< awk '$1==2&&$2==1e-2{print 1/$3,$5}' SUMMARY" title 'S=2 {/Symbol d}t=10^{-2}',\
    "< awk '$1==2&&$2==1e-3{print 1/$3,$5}' SUMMARY" title 'S=2 {/Symbol d}t=10^{-3}',\
    "< awk '$1==2&&$2==1e-4{print 1/$3,$5}' SUMMARY" title 'S=2 {/Symbol d}t=10^{-4}',\
    "< awk '$1==3&&$2==1e-1{print 1/$3,$5}' SUMMARY" title 'S=3 {/Symbol d}t=10^{-1}',\
    "< awk '$1==3&&$2==1e-2{print 1/$3,$5}' SUMMARY" title 'S=3 {/Symbol d}t=10^{-2}',\
    "< awk '$1==3&&$2==1e-3{print 1/$3,$5}' SUMMARY" title 'S=3 {/Symbol d}t=10^{-3}',\
    "< awk '$1==3&&$2==1e-4{print 1/$3,$5}' SUMMARY" title 'S=3 {/Symbol d}t=10^{-4}',\
    "< awk '$1==7&&$2==1e-1{print 1/$3,$5}' SUMMARY" title 'S=7 {/Symbol d}t=10^{-1}',\
    "< awk '$1==7&&$2==1e-2{print 1/$3,$5}' SUMMARY" title 'S=7 {/Symbol d}t=10^{-2}',\
    "< awk '$1==7&&$2==1e-3{print 1/$3,$5}' SUMMARY" title 'S=7 {/Symbol d}t=10^{-3}',\
    "< awk '$1==7&&$2==1e-4{print 1/$3,$5}' SUMMARY" title 'S=7 {/Symbol d}t=10^{-4}',\
    "< awk '$1==8&&$2==1e-1{print 1/$3,$5}' SUMMARY" title 'S=8 {/Symbol d}t=10^{-1}',\
    "< awk '$1==8&&$2==1e-2{print 1/$3,$5}' SUMMARY" title 'S=8 {/Symbol d}t=10^{-2}',\
    "< awk '$1==8&&$2==1e-3{print 1/$3,$5}' SUMMARY" title 'S=8 {/Symbol d}t=10^{-3}',\
    "< awk '$1==8&&$2==1e-4{print 1/$3,$5}' SUMMARY" title 'S=8 {/Symbol d}t=10^{-4}',\
    "< awk '$1==10&&$2==1e-1{print 1/$3,$5}' SUMMARY" title 'S=10 {/Symbol d}t=10^{-1}',\
    "< awk '$1==10&&$2==1e-2{print 1/$3,$5}' SUMMARY" title 'S=10 {/Symbol d}t=10^{-2}',\
    "< awk '$1==10&&$2==1e-3{print 1/$3,$5}' SUMMARY" title 'S=10 {/Symbol d}t=10^{-3}',\
    "< awk '$1==10&&$2==1e-4{print 1/$3,$5}' SUMMARY" title 'S=10 {/Symbol d}t=10^{-4}'

set title 'Density at {/Symbol r}=0.5 for solver 1'
set key left top width -13
plot \
    0.85E+19 lw 5,\
    "< awk '$1==1&&$2==1e-1{print 1/$3,$5}' SUMMARY" title '{/Symbol d}t=10^{-1}',\
    "< awk '$1==1&&$2==1e-2{print 1/$3,$5}' SUMMARY" title '{/Symbol d}t=10^{-2}',\
    "< awk '$1==1&&$2==1e-3{print 1/$3,$5}' SUMMARY" title '{/Symbol d}t=10^{-3}',\
    "< awk '$1==1&&$2==1e-4{print 1/$3,$5}' SUMMARY" title '{/Symbol d}t=10^{-4}'

set title 'Density at {/Symbol r}=0.5 for solver 2'
set key left bottom width -13
plot \
    0.85E+19 lw 5,\
    "< awk '$1==2&&$2==1e-1{print 1/$3,$5}' SUMMARY" title '{/Symbol d}t=10^{-1}',\
    "< awk '$1==2&&$2==1e-2{print 1/$3,$5}' SUMMARY" title '{/Symbol d}t=10^{-2}',\
    "< awk '$1==2&&$2==1e-3{print 1/$3,$5}' SUMMARY" title '{/Symbol d}t=10^{-3}',\
    "< awk '$1==2&&$2==1e-4{print 1/$3,$5}' SUMMARY" title '{/Symbol d}t=10^{-4}'

set title 'Density at {/Symbol r}=0.5 for solver 3'
set key left bottom width -13
plot \
    0.85E+19 lw 5,\
    "< awk '$1==3&&$2==1e-1{print 1/$3,$5}' SUMMARY" title '{/Symbol d}t=10^{-1}',\
    "< awk '$1==3&&$2==1e-2{print 1/$3,$5}' SUMMARY" title '{/Symbol d}t=10^{-2}',\
    "< awk '$1==3&&$2==1e-3{print 1/$3,$5}' SUMMARY" title '{/Symbol d}t=10^{-3}',\
    "< awk '$1==3&&$2==1e-4{print 1/$3,$5}' SUMMARY" title '{/Symbol d}t=10^{-4}'

set title 'Density at {/Symbol r}=0.5 for solver 10'
set key left bottom width -13
plot \
    0.85E+19 lw 5,\
    "< awk '$1==10&&$2==1e-1{print 1/$3,$5}' SUMMARY" title '{/Symbol d}t=10^{-1}',\
    "< awk '$1==10&&$2==1e-2{print 1/$3,$5}' SUMMARY" title '{/Symbol d}t=10^{-2}',\
    "< awk '$1==10&&$2==1e-3{print 1/$3,$5}' SUMMARY" title '{/Symbol d}t=10^{-3}',\
    "< awk '$1==10&&$2==1e-4{print 1/$3,$5}' SUMMARY" title '{/Symbol d}t=10^{-4}'

set title 'Relative density deviation at {/Symbol r}=0.5 for solver 3'
set key left bottom width -13
plot \
    "< awk '$1==3&&$2==1e-1{print 1/$3,($5-$6)/$6}' SUMMARY" title '{/Symbol d}t=10^{-1}',\
    "< awk '$1==3&&$2==1e-2{print 1/$3,($5-$6)/$6}' SUMMARY" title '{/Symbol d}t=10^{-2}',\
    "< awk '$1==3&&$2==1e-3{print 1/$3,($5-$6)/$6}' SUMMARY" title '{/Symbol d}t=10^{-3}',\
    "< awk '$1==3&&$2==1e-4{print 1/$3,($5-$6)/$6}' SUMMARY" title '{/Symbol d}t=10^{-4}'

set title 'Relative density deviation at {/Symbol r}=0.5 for solver 1, 2, 3 & 10'
set key left top width -13
set log y
plot \
    "< awk '$1==1&&$2==1e-4{print 1/$3,sqrt((($5-$6)/$6)**2)}' SUMMARY" title 'S=1 {/Symbol d}t=10^{-4}',\
    "< awk '$1==2&&$2==1e-4{print 1/$3,sqrt((($5-$6)/$6)**2)}' SUMMARY" title 'S=2 {/Symbol d}t=10^{-4}',\
    "< awk '$1==3&&$2==1e-4{print 1/$3,sqrt((($5-$6)/$6)**2)}' SUMMARY" title 'S=3 {/Symbol d}t=10^{-4}',\
    "< awk '$1==10&&$2==1e-4{print 1/$3,sqrt((($5-$6)/$6)**2)}' SUMMARY" title 'S=10 {/Symbol d}t=10^{-4}'
set title 'Relative density deviation at {/Symbol r}=0.5 for solver 2, 3 & 10'
set key left top width -13
set log y
plot \
    "< awk '$1==2&&$2==1e-4{print 1/$3,sqrt((($5-$6)/$6)**2)}' SUMMARY" title 'S=2 {/Symbol d}t=10^{-4}',\
    "< awk '$1==3&&$2==1e-4{print 1/$3,sqrt((($5-$6)/$6)**2)}' SUMMARY" title 'S=3 {/Symbol d}t=10^{-4}',\
    "< awk '$1==10&&$2==1e-4{print 1/$3,sqrt((($5-$6)/$6)**2)}' SUMMARY" title 'S=10 {/Symbol d}t=10^{-4}'
set nolog y
set title 'Relative density deviation at {/Symbol r}=0.5 for solver 2'
set key left top width -13
set log y
plot \
    "< awk '$1==2&&$2==1e-4{print 1/$3,sqrt((($5-$6)/$6)**2)}' SUMMARY" title 'S=2 {/Symbol d}t=10^{-4}'
set nolog y


set xrange [0.00008:0.12]
set xlabel '{/Symbol d}t'
set title 'Density at {/Symbol r}=0.5 for different solvers and NX'
set key left bottom width 0
plot \
    0.85E+19 lw 5,\
    "< awk '$1==1&&$3==5{print $2,$5}' SUMMARY" title 'S=1 NX=5',\
    "< awk '$1==1&&$3==9{print $2,$5}' SUMMARY" title 'S=1 NX=9',\
    "< awk '$1==1&&$3==17{print $2,$5}' SUMMARY" title 'S=1 NX=17',\
    "< awk '$1==1&&$3==33{print $2,$5}' SUMMARY" title 'S=1 NX=33',\
    "< awk '$1==1&&$3==65{print $2,$5}' SUMMARY" title 'S=1 NX=65',\
    "< awk '$1==1&&$3==129{print $2,$5}' SUMMARY" title 'S=1 NX=129',\
    "< awk '$1==1&&$3==257{print $2,$5}' SUMMARY" title 'S=1 NX=257',\
    "< awk '$1==1&&$3==513{print $2,$5}' SUMMARY" title 'S=1 NX=513',\
    "< awk '$1==1&&$3==1025{print $2,$5}' SUMMARY" title 'S=1 NX=1025',\
    "< awk '$1==2&&$3==5{print $2,$5}' SUMMARY" title 'S=2 NX=5',\
    "< awk '$1==2&&$3==9{print $2,$5}' SUMMARY" title 'S=2 NX=9',\
    "< awk '$1==2&&$3==17{print $2,$5}' SUMMARY" title 'S=2 NX=17',\
    "< awk '$1==2&&$3==33{print $2,$5}' SUMMARY" title 'S=2 NX=33',\
    "< awk '$1==2&&$3==65{print $2,$5}' SUMMARY" title 'S=2 NX=65',\
    "< awk '$1==2&&$3==129{print $2,$5}' SUMMARY" title 'S=2 NX=129',\
    "< awk '$1==2&&$3==257{print $2,$5}' SUMMARY" title 'S=2 NX=257',\
    "< awk '$1==2&&$3==513{print $2,$5}' SUMMARY" title 'S=2 NX=513',\
    "< awk '$1==2&&$3==1025{print $2,$5}' SUMMARY" title 'S=2 NX=1025',\
    "< awk '$1==3&&$3==5{print $2,$5}' SUMMARY" title 'S=3 NX=5',\
    "< awk '$1==3&&$3==9{print $2,$5}' SUMMARY" title 'S=3 NX=9',\
    "< awk '$1==3&&$3==17{print $2,$5}' SUMMARY" title 'S=3 NX=17',\
    "< awk '$1==3&&$3==33{print $2,$5}' SUMMARY" title 'S=3 NX=33',\
    "< awk '$1==3&&$3==65{print $2,$5}' SUMMARY" title 'S=3 NX=65',\
    "< awk '$1==3&&$3==129{print $2,$5}' SUMMARY" title 'S=3 NX=129',\
    "< awk '$1==3&&$3==257{print $2,$5}' SUMMARY" title 'S=3 NX=257',\
    "< awk '$1==3&&$3==513{print $2,$5}' SUMMARY" title 'S=3 NX=513',\
    "< awk '$1==3&&$3==1025{print $2,$5}' SUMMARY" title 'S=3 NX=1025',\
    "< awk '$1==7&&$3==5{print $2,$5}' SUMMARY" title 'S=7 NX=5',\
    "< awk '$1==7&&$3==9{print $2,$5}' SUMMARY" title 'S=7 NX=9',\
    "< awk '$1==7&&$3==17{print $2,$5}' SUMMARY" title 'S=7 NX=17',\
    "< awk '$1==7&&$3==33{print $2,$5}' SUMMARY" title 'S=7 NX=33',\
    "< awk '$1==7&&$3==65{print $2,$5}' SUMMARY" title 'S=7 NX=65',\
    "< awk '$1==7&&$3==129{print $2,$5}' SUMMARY" title 'S=7 NX=129',\
    "< awk '$1==7&&$3==257{print $2,$5}' SUMMARY" title 'S=7 NX=257',\
    "< awk '$1==7&&$3==513{print $2,$5}' SUMMARY" title 'S=7 NX=513',\
    "< awk '$1==7&&$3==1025{print $2,$5}' SUMMARY" title 'S=7 NX=1025',\
    "< awk '$1==8&&$3==5{print $2,$5}' SUMMARY" title 'S=8 NX=5',\
    "< awk '$1==8&&$3==9{print $2,$5}' SUMMARY" title 'S=8 NX=9',\
    "< awk '$1==8&&$3==17{print $2,$5}' SUMMARY" title 'S=8 NX=17',\
    "< awk '$1==8&&$3==33{print $2,$5}' SUMMARY" title 'S=8 NX=33',\
    "< awk '$1==8&&$3==65{print $2,$5}' SUMMARY" title 'S=8 NX=65',\
    "< awk '$1==8&&$3==129{print $2,$5}' SUMMARY" title 'S=8 NX=129',\
    "< awk '$1==8&&$3==257{print $2,$5}' SUMMARY" title 'S=8 NX=257',\
    "< awk '$1==8&&$3==513{print $2,$5}' SUMMARY" title 'S=8 NX=513',\
    "< awk '$1==8&&$3==1025{print $2,$5}' SUMMARY" title 'S=8 NX=1025',\
    "< awk '$1==10&&$3==5{print $2,$5}' SUMMARY" title 'S=10 NX=5',\
    "< awk '$1==10&&$3==9{print $2,$5}' SUMMARY" title 'S=10 NX=9',\
    "< awk '$1==10&&$3==17{print $2,$5}' SUMMARY" title 'S=10 NX=17',\
    "< awk '$1==10&&$3==33{print $2,$5}' SUMMARY" title 'S=10 NX=33',\
    "< awk '$1==10&&$3==65{print $2,$5}' SUMMARY" title 'S=10 NX=65',\
    "< awk '$1==10&&$3==129{print $2,$5}' SUMMARY" title 'S=10 NX=129',\
    "< awk '$1==10&&$3==257{print $2,$5}' SUMMARY" title 'S=10 NX=257',\
    "< awk '$1==10&&$3==513{print $2,$5}' SUMMARY" title 'S=10 NX=513',\
    "< awk '$1==10&&$3==1025{print $2,$5}' SUMMARY" title 'S=10 NX=1025'

set title 'Density at {/Symbol r}=0.5 for solver 1'
set key top right
plot \
    0.85E+19 lw 5,\
    "< awk '$1==1&&$3==5{print $2,$5}' SUMMARY" title 'NX=5',\
    "< awk '$1==1&&$3==9{print $2,$5}' SUMMARY" title 'NX=9',\
    "< awk '$1==1&&$3==17{print $2,$5}' SUMMARY" title 'NX=17',\
    "< awk '$1==1&&$3==33{print $2,$5}' SUMMARY" title 'NX=33',\
    "< awk '$1==1&&$3==65{print $2,$5}' SUMMARY" title 'NX=65',\
    "< awk '$1==1&&$3==129{print $2,$5}' SUMMARY" title 'NX=129',\
    "< awk '$1==1&&$3==257{print $2,$5}' SUMMARY" title 'NX=257',\
    "< awk '$1==1&&$3==513{print $2,$5}' SUMMARY" title 'NX=513',\
    "< awk '$1==1&&$3==1025{print $2,$5}' SUMMARY" title 'NX=1025'

set title 'Density at {/Symbol r}=0.5 for solver 2'
set key bottom right
plot \
    0.85E+19 lw 5,\
    "< awk '$1==2&&$3==5{print $2,$5}' SUMMARY" title 'NX=5',\
    "< awk '$1==2&&$3==9{print $2,$5}' SUMMARY" title 'NX=9',\
    "< awk '$1==2&&$3==17{print $2,$5}' SUMMARY" title 'NX=17',\
    "< awk '$1==2&&$3==33{print $2,$5}' SUMMARY" title 'NX=33',\
    "< awk '$1==2&&$3==65{print $2,$5}' SUMMARY" title 'NX=65',\
    "< awk '$1==2&&$3==129{print $2,$5}' SUMMARY" title 'NX=129',\
    "< awk '$1==2&&$3==257{print $2,$5}' SUMMARY" title 'NX=257',\
    "< awk '$1==2&&$3==513{print $2,$5}' SUMMARY" title 'NX=513',\
    "< awk '$1==2&&$3==1025{print $2,$5}' SUMMARY" title 'NX=1025'

set title 'Density at {/Symbol r}=0.5 for solver 3'
set key top left
plot \
    0.85E+19 lw 5,\
    "< awk '$1==3&&$3==5{print $2,$5}' SUMMARY" title 'NX=5',\
    "< awk '$1==3&&$3==9{print $2,$5}' SUMMARY" title 'NX=9',\
    "< awk '$1==3&&$3==17{print $2,$5}' SUMMARY" title 'NX=17',\
    "< awk '$1==3&&$3==33{print $2,$5}' SUMMARY" title 'NX=33',\
    "< awk '$1==3&&$3==65{print $2,$5}' SUMMARY" title 'NX=65',\
    "< awk '$1==3&&$3==129{print $2,$5}' SUMMARY" title 'NX=129',\
    "< awk '$1==3&&$3==257{print $2,$5}' SUMMARY" title 'NX=257',\
    "< awk '$1==3&&$3==513{print $2,$5}' SUMMARY" title 'NX=513',\
    "< awk '$1==3&&$3==1025{print $2,$5}' SUMMARY" title 'NX=1025'

set title 'Density at {/Symbol r}=0.5 for solver 10'
set key top left
plot \
    0.85E+19 lw 5,\
    "< awk '$1==10&&$3==5{print $2,$5}' SUMMARY" title 'NX=5',\
    "< awk '$1==10&&$3==9{print $2,$5}' SUMMARY" title 'NX=9',\
    "< awk '$1==10&&$3==17{print $2,$5}' SUMMARY" title 'NX=17',\
    "< awk '$1==10&&$3==33{print $2,$5}' SUMMARY" title 'NX=33',\
    "< awk '$1==10&&$3==65{print $2,$5}' SUMMARY" title 'NX=65',\
    "< awk '$1==10&&$3==129{print $2,$5}' SUMMARY" title 'NX=129',\
    "< awk '$1==10&&$3==257{print $2,$5}' SUMMARY" title 'NX=257',\
    "< awk '$1==10&&$3==513{print $2,$5}' SUMMARY" title 'NX=513',\
    "< awk '$1==10&&$3==1025{print $2,$5}' SUMMARY" title 'NX=1025'

set title 'Relative density deviation at {/Symbol r}=0.5 for solver 3'
set key top left
plot \
    "< awk '$1==3&&$3==5{print $2,($5-$6)/$6}' SUMMARY" title 'NX=5',\
    "< awk '$1==3&&$3==9{print $2,($5-$6)/$6}' SUMMARY" title 'NX=9',\
    "< awk '$1==3&&$3==17{print $2,($5-$6)/$6}' SUMMARY" title 'NX=17',\
    "< awk '$1==3&&$3==33{print $2,($5-$6)/$6}' SUMMARY" title 'NX=33',\
    "< awk '$1==3&&$3==65{print $2,($5-$6)/$6}' SUMMARY" title 'NX=65',\
    "< awk '$1==3&&$3==129{print $2,($5-$6)/$6}' SUMMARY" title 'NX=129',\
    "< awk '$1==3&&$3==257{print $2,($5-$6)/$6}' SUMMARY" title 'NX=257',\
    "< awk '$1==3&&$3==513{print $2,($5-$6)/$6}' SUMMARY" title 'NX=513',\
    "< awk '$1==3&&$3==1025{print $2,($5-$6)/$6}' SUMMARY" title 'NX=1025'

set title 'Relative density deviation at {/Symbol r}=0.5 for solver 1, 2, 3 & 4'
set log y
plot \
    "< awk '$1==1&&$3==1025{print $2,sqrt((($5-$6)/$6)**2)}' SUMMARY" title 'S=1 NX=1025',\
    "< awk '$1==2&&$3==1025{print $2,sqrt((($5-$6)/$6)**2)}' SUMMARY" title 'S=2 NX=1025',\
    "< awk '$1==3&&$3==1025{print $2,sqrt((($5-$6)/$6)**2)}' SUMMARY" title 'S=3 NX=1025',\
    "< awk '$1==10&&$3==1025{print $2,sqrt((($5-$6)/$6)**2)}' SUMMARY" title 'S=10 NX=1025'
set nolog y
