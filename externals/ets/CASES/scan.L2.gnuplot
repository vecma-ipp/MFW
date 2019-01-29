set term png large size 800,600
# set term png font Vera 14 size 800,600
set pointsize 3
set out "scan_L2.ps" ; set term postscript color solid 20
set log xy
set key below
set data style lp
set xlabel "1/NRHO"
set xrange [8e-4:0.3]
set title "L2 Normed Residual for S=1, dt=1e-1"
plot \
     "< awk '$1==1&&$2==1e-1{print $0}' SUMMARY.L2" using (1/$3):5 title 'NI',\
     "< awk '$1==1&&$2==1e-1{print $0}' SUMMARY.L2" using (1/$3):7 title 'NE',\
     "< awk '$1==1&&$2==1e-1{print $0}' SUMMARY.L2" using (1/$3):9 title 'TI',\
     "< awk '$1==1&&$2==1e-1{print $0}' SUMMARY.L2" using (1/$3):11 title 'TE',\
     "< awk '$1==1&&$2==1e-1{print $0}' SUMMARY.L2" using (1/$3):13 title 'VTOR',\
     "< awk '$1==1&&$2==1e-1{print $0}' SUMMARY.L2" using (1/$3):15 title 'PSI'
set title "L2 Normed Residual for S=1, dt=1e-2"
plot \
     "< awk '$1==1&&$2==1e-2{print $0}' SUMMARY.L2" using (1/$3):5 title 'NI',\
     "< awk '$1==1&&$2==1e-2{print $0}' SUMMARY.L2" using (1/$3):7 title 'NE',\
     "< awk '$1==1&&$2==1e-2{print $0}' SUMMARY.L2" using (1/$3):9 title 'TI',\
     "< awk '$1==1&&$2==1e-2{print $0}' SUMMARY.L2" using (1/$3):11 title 'TE',\
     "< awk '$1==1&&$2==1e-2{print $0}' SUMMARY.L2" using (1/$3):13 title 'VTOR',\
     "< awk '$1==1&&$2==1e-2{print $0}' SUMMARY.L2" using (1/$3):15 title 'PSI'
set title "L2 Normed Residual for S=1, dt=1e-3"
plot \
     "< awk '$1==1&&$2==1e-3{print $0}' SUMMARY.L2" using (1/$3):5 title 'NI',\
     "< awk '$1==1&&$2==1e-3{print $0}' SUMMARY.L2" using (1/$3):7 title 'NE',\
     "< awk '$1==1&&$2==1e-3{print $0}' SUMMARY.L2" using (1/$3):9 title 'TI',\
     "< awk '$1==1&&$2==1e-3{print $0}' SUMMARY.L2" using (1/$3):11 title 'TE',\
     "< awk '$1==1&&$2==1e-3{print $0}' SUMMARY.L2" using (1/$3):13 title 'VTOR',\
     "< awk '$1==1&&$2==1e-3{print $0}' SUMMARY.L2" using (1/$3):15 title 'PSI'
set title "L2 Normed Residual for S=1, dt=1e-4"
plot \
     "< awk '$1==1&&$2==1e-4{print $0}' SUMMARY.L2" using (1/$3):5 title 'NI',\
     "< awk '$1==1&&$2==1e-4{print $0}' SUMMARY.L2" using (1/$3):7 title 'NE',\
     "< awk '$1==1&&$2==1e-4{print $0}' SUMMARY.L2" using (1/$3):9 title 'TI',\
     "< awk '$1==1&&$2==1e-4{print $0}' SUMMARY.L2" using (1/$3):11 title 'TE',\
     "< awk '$1==1&&$2==1e-4{print $0}' SUMMARY.L2" using (1/$3):13 title 'VTOR',\
     "< awk '$1==1&&$2==1e-4{print $0}' SUMMARY.L2" using (1/$3):15 title 'PSI'
set title "L2 Normed Residual for S=2, dt=1e-1"
plot \
     "< awk '$1==2&&$2==1e-1{print $0}' SUMMARY.L2" using (1/$3):5 title 'NI',\
     "< awk '$1==2&&$2==1e-1{print $0}' SUMMARY.L2" using (1/$3):7 title 'NE',\
     "< awk '$1==2&&$2==1e-1{print $0}' SUMMARY.L2" using (1/$3):9 title 'TI',\
     "< awk '$1==2&&$2==1e-1{print $0}' SUMMARY.L2" using (1/$3):11 title 'TE',\
     "< awk '$1==2&&$2==1e-1{print $0}' SUMMARY.L2" using (1/$3):13 title 'VTOR',\
     "< awk '$1==2&&$2==1e-1{print $0}' SUMMARY.L2" using (1/$3):15 title 'PSI'
set title "L2 Normed Residual for S=2, dt=1e-2"
plot \
     "< awk '$1==2&&$2==1e-2{print $0}' SUMMARY.L2" using (1/$3):5 title 'NI',\
     "< awk '$1==2&&$2==1e-2{print $0}' SUMMARY.L2" using (1/$3):7 title 'NE',\
     "< awk '$1==2&&$2==1e-2{print $0}' SUMMARY.L2" using (1/$3):9 title 'TI',\
     "< awk '$1==2&&$2==1e-2{print $0}' SUMMARY.L2" using (1/$3):11 title 'TE',\
     "< awk '$1==2&&$2==1e-2{print $0}' SUMMARY.L2" using (1/$3):13 title 'VTOR',\
     "< awk '$1==2&&$2==1e-2{print $0}' SUMMARY.L2" using (1/$3):15 title 'PSI'
set title "L2 Normed Residual for S=2, dt=1e-3"
plot \
     "< awk '$1==2&&$2==1e-3{print $0}' SUMMARY.L2" using (1/$3):5 title 'NI',\
     "< awk '$1==2&&$2==1e-3{print $0}' SUMMARY.L2" using (1/$3):7 title 'NE',\
     "< awk '$1==2&&$2==1e-3{print $0}' SUMMARY.L2" using (1/$3):9 title 'TI',\
     "< awk '$1==2&&$2==1e-3{print $0}' SUMMARY.L2" using (1/$3):11 title 'TE',\
     "< awk '$1==2&&$2==1e-3{print $0}' SUMMARY.L2" using (1/$3):13 title 'VTOR',\
     "< awk '$1==2&&$2==1e-3{print $0}' SUMMARY.L2" using (1/$3):15 title 'PSI'
set title "L2 Normed Residual for S=2, dt=1e-4"
plot \
     "< awk '$1==2&&$2==1e-4{print $0}' SUMMARY.L2" using (1/$3):5 title 'NI',\
     "< awk '$1==2&&$2==1e-4{print $0}' SUMMARY.L2" using (1/$3):7 title 'NE',\
     "< awk '$1==2&&$2==1e-4{print $0}' SUMMARY.L2" using (1/$3):9 title 'TI',\
     "< awk '$1==2&&$2==1e-4{print $0}' SUMMARY.L2" using (1/$3):11 title 'TE',\
     "< awk '$1==2&&$2==1e-4{print $0}' SUMMARY.L2" using (1/$3):13 title 'VTOR',\
     "< awk '$1==2&&$2==1e-4{print $0}' SUMMARY.L2" using (1/$3):15 title 'PSI'
set title "L2 Normed Residual for S=3, dt=1e-1"
plot \
     "< awk '$1==3&&$2==1e-1{print $0}' SUMMARY.L2" using (1/$3):5 title 'NI',\
     "< awk '$1==3&&$2==1e-1{print $0}' SUMMARY.L2" using (1/$3):7 title 'NE',\
     "< awk '$1==3&&$2==1e-1{print $0}' SUMMARY.L2" using (1/$3):9 title 'TI',\
     "< awk '$1==3&&$2==1e-1{print $0}' SUMMARY.L2" using (1/$3):11 title 'TE',\
     "< awk '$1==3&&$2==1e-1{print $0}' SUMMARY.L2" using (1/$3):13 title 'VTOR',\
     "< awk '$1==3&&$2==1e-1{print $0}' SUMMARY.L2" using (1/$3):15 title 'PSI'
set title "L2 Normed Residual for S=3, dt=1e-2"
plot \
     "< awk '$1==3&&$2==1e-2{print $0}' SUMMARY.L2" using (1/$3):5 title 'NI',\
     "< awk '$1==3&&$2==1e-2{print $0}' SUMMARY.L2" using (1/$3):7 title 'NE',\
     "< awk '$1==3&&$2==1e-2{print $0}' SUMMARY.L2" using (1/$3):9 title 'TI',\
     "< awk '$1==3&&$2==1e-2{print $0}' SUMMARY.L2" using (1/$3):11 title 'TE',\
     "< awk '$1==3&&$2==1e-2{print $0}' SUMMARY.L2" using (1/$3):13 title 'VTOR',\
     "< awk '$1==3&&$2==1e-2{print $0}' SUMMARY.L2" using (1/$3):15 title 'PSI'
set title "L2 Normed Residual for S=3, dt=1e-3"
plot \
     "< awk '$1==3&&$2==1e-3{print $0}' SUMMARY.L2" using (1/$3):5 title 'NI',\
     "< awk '$1==3&&$2==1e-3{print $0}' SUMMARY.L2" using (1/$3):7 title 'NE',\
     "< awk '$1==3&&$2==1e-3{print $0}' SUMMARY.L2" using (1/$3):9 title 'TI',\
     "< awk '$1==3&&$2==1e-3{print $0}' SUMMARY.L2" using (1/$3):11 title 'TE',\
     "< awk '$1==3&&$2==1e-3{print $0}' SUMMARY.L2" using (1/$3):13 title 'VTOR',\
     "< awk '$1==3&&$2==1e-3{print $0}' SUMMARY.L2" using (1/$3):15 title 'PSI'
set title "L2 Normed Residual for S=3, dt=1e-4"
plot \
     "< awk '$1==3&&$2==1e-4{print $0}' SUMMARY.L2" using (1/$3):5 title 'NI',\
     "< awk '$1==3&&$2==1e-4{print $0}' SUMMARY.L2" using (1/$3):7 title 'NE',\
     "< awk '$1==3&&$2==1e-4{print $0}' SUMMARY.L2" using (1/$3):9 title 'TI',\
     "< awk '$1==3&&$2==1e-4{print $0}' SUMMARY.L2" using (1/$3):11 title 'TE',\
     "< awk '$1==3&&$2==1e-4{print $0}' SUMMARY.L2" using (1/$3):13 title 'VTOR',\
     "< awk '$1==3&&$2==1e-4{print $0}' SUMMARY.L2" using (1/$3):15 title 'PSI'
set title "L2 Normed Residual for S=7, dt=1e-1"
plot \
     "< awk '$1==7&&$2==1e-1{print $0}' SUMMARY.L2" using (1/$3):5 title 'NI',\
     "< awk '$1==7&&$2==1e-1{print $0}' SUMMARY.L2" using (1/$3):7 title 'NE',\
     "< awk '$1==7&&$2==1e-1{print $0}' SUMMARY.L2" using (1/$3):9 title 'TI',\
     "< awk '$1==7&&$2==1e-1{print $0}' SUMMARY.L2" using (1/$3):11 title 'TE',\
     "< awk '$1==7&&$2==1e-1{print $0}' SUMMARY.L2" using (1/$3):13 title 'VTOR',\
     "< awk '$1==7&&$2==1e-1{print $0}' SUMMARY.L2" using (1/$3):15 title 'PSI'
set title "L2 Normed Residual for S=7, dt=1e-2"
plot \
     "< awk '$1==7&&$2==1e-2{print $0}' SUMMARY.L2" using (1/$3):5 title 'NI',\
     "< awk '$1==7&&$2==1e-2{print $0}' SUMMARY.L2" using (1/$3):7 title 'NE',\
     "< awk '$1==7&&$2==1e-2{print $0}' SUMMARY.L2" using (1/$3):9 title 'TI',\
     "< awk '$1==7&&$2==1e-2{print $0}' SUMMARY.L2" using (1/$3):11 title 'TE',\
     "< awk '$1==7&&$2==1e-2{print $0}' SUMMARY.L2" using (1/$3):13 title 'VTOR',\
     "< awk '$1==7&&$2==1e-2{print $0}' SUMMARY.L2" using (1/$3):15 title 'PSI'
set title "L2 Normed Residual for S=7, dt=1e-3"
plot \
     "< awk '$1==7&&$2==1e-3{print $0}' SUMMARY.L2" using (1/$3):5 title 'NI',\
     "< awk '$1==7&&$2==1e-3{print $0}' SUMMARY.L2" using (1/$3):7 title 'NE',\
     "< awk '$1==7&&$2==1e-3{print $0}' SUMMARY.L2" using (1/$3):9 title 'TI',\
     "< awk '$1==7&&$2==1e-3{print $0}' SUMMARY.L2" using (1/$3):11 title 'TE',\
     "< awk '$1==7&&$2==1e-3{print $0}' SUMMARY.L2" using (1/$3):13 title 'VTOR',\
     "< awk '$1==7&&$2==1e-3{print $0}' SUMMARY.L2" using (1/$3):15 title 'PSI'
set title "L2 Normed Residual for S=7, dt=1e-4"
plot \
     "< awk '$1==7&&$2==1e-4{print $0}' SUMMARY.L2" using (1/$3):5 title 'NI',\
     "< awk '$1==7&&$2==1e-4{print $0}' SUMMARY.L2" using (1/$3):7 title 'NE',\
     "< awk '$1==7&&$2==1e-4{print $0}' SUMMARY.L2" using (1/$3):9 title 'TI',\
     "< awk '$1==7&&$2==1e-4{print $0}' SUMMARY.L2" using (1/$3):11 title 'TE',\
     "< awk '$1==7&&$2==1e-4{print $0}' SUMMARY.L2" using (1/$3):13 title 'VTOR',\
     "< awk '$1==7&&$2==1e-4{print $0}' SUMMARY.L2" using (1/$3):15 title 'PSI'
set title "L2 Normed Residual for S=10, dt=1e-1"
plot \
     "< awk '$1==10&&$2==1e-1{print $0}' SUMMARY.L2" using (1/$3):5 title 'NI',\
     "< awk '$1==10&&$2==1e-1{print $0}' SUMMARY.L2" using (1/$3):7 title 'NE',\
     "< awk '$1==10&&$2==1e-1{print $0}' SUMMARY.L2" using (1/$3):9 title 'TI',\
     "< awk '$1==10&&$2==1e-1{print $0}' SUMMARY.L2" using (1/$3):11 title 'TE',\
     "< awk '$1==10&&$2==1e-1{print $0}' SUMMARY.L2" using (1/$3):13 title 'VTOR',\
     "< awk '$1==10&&$2==1e-1{print $0}' SUMMARY.L2" using (1/$3):15 title 'PSI'
set title "L2 Normed Residual for S=10, dt=1e-2"
plot \
     "< awk '$1==10&&$2==1e-2{print $0}' SUMMARY.L2" using (1/$3):5 title 'NI',\
     "< awk '$1==10&&$2==1e-2{print $0}' SUMMARY.L2" using (1/$3):7 title 'NE',\
     "< awk '$1==10&&$2==1e-2{print $0}' SUMMARY.L2" using (1/$3):9 title 'TI',\
     "< awk '$1==10&&$2==1e-2{print $0}' SUMMARY.L2" using (1/$3):11 title 'TE',\
     "< awk '$1==10&&$2==1e-2{print $0}' SUMMARY.L2" using (1/$3):13 title 'VTOR',\
     "< awk '$1==10&&$2==1e-2{print $0}' SUMMARY.L2" using (1/$3):15 title 'PSI'
set title "L2 Normed Residual for S=10, dt=1e-3"
plot \
     "< awk '$1==10&&$2==1e-3{print $0}' SUMMARY.L2" using (1/$3):5 title 'NI',\
     "< awk '$1==10&&$2==1e-3{print $0}' SUMMARY.L2" using (1/$3):7 title 'NE',\
     "< awk '$1==10&&$2==1e-3{print $0}' SUMMARY.L2" using (1/$3):9 title 'TI',\
     "< awk '$1==10&&$2==1e-3{print $0}' SUMMARY.L2" using (1/$3):11 title 'TE',\
     "< awk '$1==10&&$2==1e-3{print $0}' SUMMARY.L2" using (1/$3):13 title 'VTOR',\
     "< awk '$1==10&&$2==1e-3{print $0}' SUMMARY.L2" using (1/$3):15 title 'PSI'
set title "L2 Normed Residual for S=10, dt=1e-4"
plot \
     "< awk '$1==10&&$2==1e-4{print $0}' SUMMARY.L2" using (1/$3):5 title 'NI',\
     "< awk '$1==10&&$2==1e-4{print $0}' SUMMARY.L2" using (1/$3):7 title 'NE',\
     "< awk '$1==10&&$2==1e-4{print $0}' SUMMARY.L2" using (1/$3):9 title 'TI',\
     "< awk '$1==10&&$2==1e-4{print $0}' SUMMARY.L2" using (1/$3):11 title 'TE',\
     "< awk '$1==10&&$2==1e-4{print $0}' SUMMARY.L2" using (1/$3):13 title 'VTOR',\
     "< awk '$1==10&&$2==1e-4{print $0}' SUMMARY.L2" using (1/$3):15 title 'PSI'
set xlabel "DT"
set xrange [5e-5:0.2]
set title "L2 Normed Residual for S=1, NRHO=5"
plot \
     "< awk '$1==1&&$3==5{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==1&&$3==5{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==1&&$3==5{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==1&&$3==5{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==1&&$3==5{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==1&&$3==5{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=1, NRHO=9"
plot \
     "< awk '$1==1&&$3==9{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==1&&$3==9{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==1&&$3==9{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==1&&$3==9{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==1&&$3==9{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==1&&$3==9{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=1, NRHO=17"
plot \
     "< awk '$1==1&&$3==17{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==1&&$3==17{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==1&&$3==17{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==1&&$3==17{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==1&&$3==17{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==1&&$3==17{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=1, NRHO=33"
plot \
     "< awk '$1==1&&$3==33{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==1&&$3==33{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==1&&$3==33{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==1&&$3==33{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==1&&$3==33{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==1&&$3==33{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=1, NRHO=65"
plot \
     "< awk '$1==1&&$3==65{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==1&&$3==65{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==1&&$3==65{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==1&&$3==65{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==1&&$3==65{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==1&&$3==65{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=1, NRHO=129"
plot \
     "< awk '$1==1&&$3==129{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==1&&$3==129{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==1&&$3==129{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==1&&$3==129{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==1&&$3==129{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==1&&$3==129{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=1, NRHO=257"
plot \
     "< awk '$1==1&&$3==257{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==1&&$3==257{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==1&&$3==257{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==1&&$3==257{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==1&&$3==257{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==1&&$3==257{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=1, NRHO=513"
plot \
     "< awk '$1==1&&$3==513{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==1&&$3==513{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==1&&$3==513{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==1&&$3==513{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==1&&$3==513{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==1&&$3==513{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=1, NRHO=1025"
plot \
     "< awk '$1==1&&$3==1025{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==1&&$3==1025{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==1&&$3==1025{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==1&&$3==1025{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==1&&$3==1025{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==1&&$3==1025{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=2, NRHO=5"
plot \
     "< awk '$1==2&&$3==5{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==2&&$3==5{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==2&&$3==5{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==2&&$3==5{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==2&&$3==5{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==2&&$3==5{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=2, NRHO=9"
plot \
     "< awk '$1==2&&$3==9{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==2&&$3==9{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==2&&$3==9{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==2&&$3==9{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==2&&$3==9{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==2&&$3==9{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=2, NRHO=17"
plot \
     "< awk '$1==2&&$3==17{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==2&&$3==17{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==2&&$3==17{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==2&&$3==17{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==2&&$3==17{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==2&&$3==17{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=2, NRHO=33"
plot \
     "< awk '$1==2&&$3==33{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==2&&$3==33{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==2&&$3==33{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==2&&$3==33{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==2&&$3==33{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==2&&$3==33{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=2, NRHO=65"
plot \
     "< awk '$1==2&&$3==65{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==2&&$3==65{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==2&&$3==65{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==2&&$3==65{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==2&&$3==65{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==2&&$3==65{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=2, NRHO=129"
plot \
     "< awk '$1==2&&$3==129{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==2&&$3==129{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==2&&$3==129{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==2&&$3==129{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==2&&$3==129{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==2&&$3==129{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=2, NRHO=257"
plot \
     "< awk '$1==2&&$3==257{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==2&&$3==257{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==2&&$3==257{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==2&&$3==257{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==2&&$3==257{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==2&&$3==257{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=2, NRHO=513"
plot \
     "< awk '$1==2&&$3==513{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==2&&$3==513{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==2&&$3==513{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==2&&$3==513{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==2&&$3==513{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==2&&$3==513{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=2, NRHO=1025"
plot \
     "< awk '$1==2&&$3==1025{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==2&&$3==1025{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==2&&$3==1025{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==2&&$3==1025{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==2&&$3==1025{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==2&&$3==1025{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=3, NRHO=5"
plot \
     "< awk '$1==3&&$3==5{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==3&&$3==5{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==3&&$3==5{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==3&&$3==5{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==3&&$3==5{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==3&&$3==5{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=3, NRHO=9"
plot \
     "< awk '$1==3&&$3==9{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==3&&$3==9{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==3&&$3==9{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==3&&$3==9{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==3&&$3==9{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==3&&$3==9{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=3, NRHO=17"
plot \
     "< awk '$1==3&&$3==17{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==3&&$3==17{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==3&&$3==17{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==3&&$3==17{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==3&&$3==17{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==3&&$3==17{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=3, NRHO=33"
plot \
     "< awk '$1==3&&$3==33{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==3&&$3==33{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==3&&$3==33{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==3&&$3==33{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==3&&$3==33{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==3&&$3==33{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=3, NRHO=65"
plot \
     "< awk '$1==3&&$3==65{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==3&&$3==65{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==3&&$3==65{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==3&&$3==65{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==3&&$3==65{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==3&&$3==65{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=3, NRHO=129"
plot \
     "< awk '$1==3&&$3==129{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==3&&$3==129{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==3&&$3==129{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==3&&$3==129{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==3&&$3==129{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==3&&$3==129{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=3, NRHO=257"
plot \
     "< awk '$1==3&&$3==257{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==3&&$3==257{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==3&&$3==257{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==3&&$3==257{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==3&&$3==257{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==3&&$3==257{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=3, NRHO=513"
plot \
     "< awk '$1==3&&$3==513{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==3&&$3==513{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==3&&$3==513{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==3&&$3==513{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==3&&$3==513{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==3&&$3==513{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=3, NRHO=1025"
plot \
     "< awk '$1==3&&$3==1025{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==3&&$3==1025{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==3&&$3==1025{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==3&&$3==1025{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==3&&$3==1025{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==3&&$3==1025{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=7, NRHO=5"
plot \
     "< awk '$1==7&&$3==5{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==7&&$3==5{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==7&&$3==5{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==7&&$3==5{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==7&&$3==5{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==7&&$3==5{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=7, NRHO=9"
plot \
     "< awk '$1==7&&$3==9{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==7&&$3==9{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==7&&$3==9{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==7&&$3==9{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==7&&$3==9{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==7&&$3==9{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=7, NRHO=17"
plot \
     "< awk '$1==7&&$3==17{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==7&&$3==17{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==7&&$3==17{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==7&&$3==17{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==7&&$3==17{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==7&&$3==17{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=7, NRHO=33"
plot \
     "< awk '$1==7&&$3==33{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==7&&$3==33{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==7&&$3==33{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==7&&$3==33{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==7&&$3==33{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==7&&$3==33{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=7, NRHO=65"
plot \
     "< awk '$1==7&&$3==65{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==7&&$3==65{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==7&&$3==65{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==7&&$3==65{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==7&&$3==65{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==7&&$3==65{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=7, NRHO=129"
plot \
     "< awk '$1==7&&$3==129{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==7&&$3==129{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==7&&$3==129{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==7&&$3==129{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==7&&$3==129{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==7&&$3==129{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=7, NRHO=257"
plot \
     "< awk '$1==7&&$3==257{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==7&&$3==257{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==7&&$3==257{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==7&&$3==257{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==7&&$3==257{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==7&&$3==257{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=7, NRHO=513"
plot \
     "< awk '$1==7&&$3==513{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==7&&$3==513{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==7&&$3==513{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==7&&$3==513{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==7&&$3==513{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==7&&$3==513{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=7, NRHO=1025"
plot \
     "< awk '$1==7&&$3==1025{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==7&&$3==1025{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==7&&$3==1025{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==7&&$3==1025{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==7&&$3==1025{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==7&&$3==1025{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=10, NRHO=5"
plot \
     "< awk '$1==10&&$3==5{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==10&&$3==5{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==10&&$3==5{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==10&&$3==5{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==10&&$3==5{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==10&&$3==5{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=10, NRHO=9"
plot \
     "< awk '$1==10&&$3==9{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==10&&$3==9{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==10&&$3==9{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==10&&$3==9{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==10&&$3==9{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==10&&$3==9{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=10, NRHO=17"
plot \
     "< awk '$1==10&&$3==17{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==10&&$3==17{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==10&&$3==17{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==10&&$3==17{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==10&&$3==17{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==10&&$3==17{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=10, NRHO=33"
plot \
     "< awk '$1==10&&$3==33{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==10&&$3==33{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==10&&$3==33{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==10&&$3==33{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==10&&$3==33{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==10&&$3==33{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=10, NRHO=65"
plot \
     "< awk '$1==10&&$3==65{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==10&&$3==65{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==10&&$3==65{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==10&&$3==65{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==10&&$3==65{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==10&&$3==65{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=10, NRHO=129"
plot \
     "< awk '$1==10&&$3==129{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==10&&$3==129{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==10&&$3==129{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==10&&$3==129{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==10&&$3==129{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==10&&$3==129{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=10, NRHO=257"
plot \
     "< awk '$1==10&&$3==257{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==10&&$3==257{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==10&&$3==257{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==10&&$3==257{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==10&&$3==257{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==10&&$3==257{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=10, NRHO=513"
plot \
     "< awk '$1==10&&$3==513{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==10&&$3==513{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==10&&$3==513{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==10&&$3==513{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==10&&$3==513{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==10&&$3==513{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
set title "L2 Normed Residual for S=10, NRHO=1025"
plot \
     "< awk '$1==10&&$3==1025{print $0}' SUMMARY.L2" using 2:5 title 'NI',\
     "< awk '$1==10&&$3==1025{print $0}' SUMMARY.L2" using 2:7 title 'NE',\
     "< awk '$1==10&&$3==1025{print $0}' SUMMARY.L2" using 2:9 title 'TI',\
     "< awk '$1==10&&$3==1025{print $0}' SUMMARY.L2" using 2:11 title 'TE',\
     "< awk '$1==10&&$3==1025{print $0}' SUMMARY.L2" using 2:13 title 'VTOR',\
     "< awk '$1==10&&$3==1025{print $0}' SUMMARY.L2" using 2:15 title 'PSI'
