from pylab import *

equil_code = 'chease'

do_equil = False

execfile("/pfs/home/olivh/public/pyscripts/plots/slice-bind.py")

freq = 1

flux=True


shot=28906
run=4
delta_t=10


if flux:
    execfile("/u/olivh/public/pyscripts/plots/vcfmovie.py")
else:
    execfile("/u/olivh/public/pyscripts/plots/vcmovie.py")

