from __future__ import print_function
import ual
import ascii_cpo
import os
import matplotlib
import re
import sys


if not os.environ.has_key('DISPLAY'):
    print('No display available: use the Agg back-end')
    matplotlib.use('Agg')


if 'do_equil' not in locals():
    do_equil = True
else:
    print('Rebuild equilibrium CPO: '+str(do_equil))

if 'DATA_DIR' not in locals():
    DATA_DIR = os.environ['PWD']
else:
    print('Target data directory: '+str(DATA_DIR))

if 'CHUNCK' not in locals():
    CHUNCK = 20
else:
    print('Target chunck size for parallel treatment: '+str(CHUNCK))
    
PREV_DIR=os.getcwd()


# allocate equilibrium CPO object
if 'db' not in locals():
    cpo = ual.itm()
else:
    print('use DB shot='+str(db.shot)+' run='+str(db.run))
    cpo = db



os.chdir(DATA_DIR)
# browse files in local directory
print("Finding all data files... ")
corep_pattern = re.compile('coreprof_.....\.cpo')
if (do_equil):
    equil_pattern = re.compile('equilibrium_.....\.cpo')
coret_pattern = re.compile('coretransp_.....\.cpo')

ll = os.listdir('.')
ll.sort()

lcorep = corep_pattern.findall(ll.__str__())
corep_len = len(lcorep)
cpo.coreprofArray.resize(corep_len)

if (do_equil):
    lequil = equil_pattern.findall(ll.__str__())
    equil_len = len(lequil)
    cpo.equilibriumArray.resize(equil_len)

lcoret = coret_pattern.findall(ll.__str__())
coret_len = len(lcoret)
cpo.coretranspArray.resize(coret_len)
print("OK!")


# Print iterations progress
def print_progress(iteration, total, prefix='Progress', suffix='complete', decimals=1, bar_length=50):
    """
    Call in a loop to create terminal progress bar
    @params:
        iteration   - Required  : current iteration (Int)
        total       - Required  : total iterations (Int)
        prefix      - Optional  : prefix string (Str)
        suffix      - Optional  : suffix string (Str)
        decimals    - Optional  : positive number of decimals in percent complete (Int)
        bar_length  - Optional  : character length of bar (Int)
    """
    str_format = "{0:." + str(decimals) + "f}"
    percents = str_format.format(100 * (iteration / float(total)))
    filled_length = int(round(bar_length * iteration / float(total)))
    bar = '#' * filled_length + '-' * (bar_length - filled_length)

    sys.stdout.write('\r%s |%s| %s%s %s' % (prefix, bar, percents, '%', suffix)),

    if iteration == total:
        sys.stdout.write('\n')
    sys.stdout.flush()



def build_cpo(filelist,cponame,cpoarray):
    l = len(filelist)
    #printProgressBar(0, l)
    for i in range(l):
        ascii_cpo.read(filelist[i],cponame,cpoarray[i])
        print_progress(i + 1, l)
        #print('#',end='')
        #sys.stdout.flush()
    #print('')



print('Build coreprof array')
build_cpo(lcorep,'coreprof',cpo.coreprofArray.array)

if (do_equil):
    print('Build equilibrium array')
    build_cpo(lequil,'equilibrium',cpo.equilibriumArray.array)

print('Build coretransp array')
build_cpo(lcoret,'coretransp',cpo.coretranspArray.array)

corep=cpo.coreprofArray.array
coret=cpo.coretranspArray.array
if (do_equil):
    equil=cpo.equilibriumArray.array

tsize=len(coret)

print('Concat '+str(tsize)+' slices together')


os.chdir(PREV_DIR)
