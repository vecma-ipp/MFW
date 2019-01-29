import ual
import ascii_cpo
import os
import matplotlib
import re
import sys


if not os.environ.has_key('DISPLAY'):
    print 'No display available: use the Agg back-end'
    matplotlib.use('Agg')


if 'transp_code' not in locals():
    transp_code = 'ets'
    print 'transp model set to default: ets'

if 'equil_code' not in locals():
    equil_code = 'bdseq'
    print 'equil model set to default: bdseq'

if 'turb_code' not in locals():
    turb_code = 'gem'
    print 'turb model set to default: gem'

if 'do_equil' not in locals():
    do_equil = True
else:
    print 'Rebuild equilibrium CPO: '+str(do_equil)


HOME=os.environ['HOME']
SHARED=os.environ['PWD']

NAME='ets_'

DATA=SHARED
if (len(sys.argv)>1):
    DATA=SHARED+'/'+sys.argv[1]


DATAPATH=DATA+'/'
##needed?
#PLOTPATH='/pfs/home/olivh/public/pyscripts/plots/'

print 'DATAPATH = '+DATAPATH

#VISUPATH=HOME+'/mapper/visu/'


# allocate equilibrium CPO object
if 'db' not in locals():
    cpo = ual.itm()
else:
    print 'use DB shot='+str(db.shot)+' run='+str(db.run)
    cpo = db


# load file containing plot functions for equilibrium / find a copy under shared python_ual directory
#execfile(PLOTPATH+'eq_coreprof.py')


os.chdir(DATAPATH)
# browse files in local directory
corep_pattern = re.compile(transp_code+'_coreprof_....\.cpo')
if (do_equil):
    equil_pattern = re.compile(equil_code+'_equilibrium_....\.cpo')
coret_pattern = re.compile(turb_code+'_coretransp_....\.cpo')

ll = os.listdir(DATAPATH+'.')
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




def build_cpo(filelist,codename,cponame,cpoarray):
    #from IPython.parallel import Client
    #c = Client()
    #csize = len(c.ids)
    #print 'exec on '+str(csize)+' processes'
    l = len(filelist)
    for i in range(l):
        cpoarray[i] = ascii_cpo.read(filelist[i],cponame,cpoarray[i])
        #cpoarray[i] = c[i%4].apply_async(ascii_cpo.read,filelist[i],cponame)
    #return cpoarray    
    

print 'build coreprof array'
build_cpo(lcorep,transp_code,'coreprof',cpo.coreprofArray.array)

if (do_equil):
    print 'build equilibrium array'
    build_cpo(lequil,equil_code,'equilibrium',cpo.equilibriumArray.array)

print 'build coretransp array'
build_cpo(lcoret,turb_code,'coretransp',cpo.coretranspArray.array)


#icorep=0
#iequil=0
#icoret=0
#for l in lcorep:
#    name = corep_pattern.match(l)
 #   if name != None:
  #      fname = name.string
   #     cpo.coreprofArray.array[icorep] = ascii_cpo.read(fname,'coreprof')
    #    icorep = icorep+1

#    else:
#for l in lequil:
 #   name = equil_pattern.match(l)
  #  if name != None:
   #     fname = name.string
    #    cpo.equilibriumArray.array[iequil] = ascii_cpo.read(fname,'equilibrium')
     #   iequil = iequil + 1
        
#else:
#for l in lcoret: 
 #   name = coret_pattern.match(l)
  #  if name != None:
   #     fname = name.string
    #    cpo.coretranspArray.array[icoret] = ascii_cpo.read(fname,'coretransp')
     #   icoret = icoret + 1


corep=cpo.coreprofArray.array
coret=cpo.coretranspArray.array
if (do_equil):
    equil=cpo.equilibriumArray.array

tsize=len(corep)

print 'concat '+str(tsize)+' slices together'

# plot in specified file
#plot_psi_coord(cpo)
