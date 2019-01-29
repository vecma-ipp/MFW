#! /bin/csh -f
echo "set term postscript color solid 20" > gnuplot.cmd
echo "set out 'compare.ps'" >> gnuplot.cmd
echo "set log y" >> gnuplot.cmd
foreach i (data/OUTPUT/OUT*.DAT)
  echo "set title '`basename $PWD`/$i'" >> gnuplot.cmd
  echo "plot '$i'"' using 1:(abs(($3-$2)/$3)) title '"'f.err.NI',\" >> gnuplot.cmd
  echo "     '$i'"' using 1:(abs(($5-$4)/$5)) title '"'f.err.NE',\" >> gnuplot.cmd
  echo "     '$i'"' using 1:(abs(($7-$6)/$7)) title '"'f.err.TI',\" >> gnuplot.cmd
  echo "     '$i'"' using 1:(abs(($9-$8)/$9)) title '"'f.err.TE',\" >> gnuplot.cmd
  echo "     '$i'"' using 1:(abs(($11-$10)/$11)) title '"'f.err.VTOR',\" >> gnuplot.cmd
  echo "     '$i'"' using 1:(abs(($13-$12)/($13+1))) title '"'f.err.PSI'" >> gnuplot.cmd
end
gnuplot gnuplot.cmd
psnup -pa4 -Pa4 -l -100 compare.ps compare.100.ps
sed -i~ -e 's/^%%BoundingBox: 50 50 554 770/%%BoundingBox: 0 0 595 842/' compare.100.ps
