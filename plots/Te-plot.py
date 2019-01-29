# load data #
equil_code = "chease"
do_equil = False
execfile("./slice-bind.py")

import numpy as np
import matplotlib.pyplot as plt
from matplotlib import gridspec
from scipy.interpolate import interp1d

#plt.rcParams['axes.facecolor'] = 'black'

# fluxtubes #
nft = 8						# number of fluxtubes
ft_rho_tor = cpo.coretranspArray[0].values[0].rho_tor	# rho_tor of each fluxtube

# time array #
time = range(coret_len-1)			# time step ID (-1 to exclude the incomplete step)

# Te array from coreprof #
rho_tor_grid = cpo.coreprofArray[0].rho_tor	# grid location

# bin size for averaging Te #
tstep_per_bin = 100				# number of time steps per bin
num_bins = int((coret_len-1)/tstep_per_bin)	# number of bins
bin_id = range(num_bins)			# ID for each bin


# Te from coreprof #
Te_cp = []
for i in time:
    Te_cp.append(cpo.coreprofArray[i].te.value)
Te_coreprof = np.array(Te_cp)


# Te interpolated at fluxtube locations #
Te_int = []
for i in time:
    Te_int.append(interp1d(rho_tor_grid,Te_coreprof[i])(ft_rho_tor))
Te_interp = np.array(Te_int)


# averaging Te and calculating standard dev. for every time bin #
Te_avg = np.empty([num_bins,nft])
stdev = np.empty([num_bins,nft])
for i in bin_id:
    a = i*tstep_per_bin
    b = (i+1)*tstep_per_bin-1

    for j in range(nft):
        Te_avg[i,j] = np.mean(Te_interp[a:b,j])
        stdev[i,j] = np.std(Te_interp[a:b,j])


#colormap = plt.cm.gist_ncar
#plt.gca().set_color_cycle([colormap(i) for i in np.linspace(0.0,0.9,nft)])



# plot averaged value of Te at every time bin
plt.figure(1)
plt.suptitle("Time-averaged Te ("+str(tstep_per_bin)+" tsteps per bin) spread, tau=0.01, Te_lim=0.2, dTe_lim=0.1, ndg=50")

for i in range(nft):
    plt.errorbar(bin_id, Te_avg[:,i], stdev[:,i], linestyle='--', marker='o', linewidth=2, capsize=10, label=str(round(cpo.coretranspArray[0].values[0].rho_tor_norm[i],3)))

#plt.errorbar(bin_id, Te_avg[:,0], stdev[:,0], linestyle='--', marker='o', label=str(round(cpo.coretranspArray[0].values[0].rho_tor_norm[0],3)))
#plt.errorbar(bin_id, Te_avg[:,1], stdev[:,1], linestyle='--', marker='s', color='r', label=str(round(cpo.coretranspArray[0].values[0].rho_tor_norm[1],3)))
#plt.errorbar(bin_id, Te_avg[:,2], stdev[:,2], linestyle='--', marker='^', color='g', label=str(round(cpo.coretranspArray[0].values[0].rho_tor_norm[2],3)))
#plt.errorbar(bin_id, Te_avg[:,3], stdev[:,3], linestyle='--', marker='o', color='k', label=str(round(cpo.coretranspArray[0].values[0].rho_tor_norm[3],3)))
#plt.errorbar(bin_id, Te_avg[:,4], stdev[:,4], linestyle='--', marker='s', color='m', label=str(round(cpo.coretranspArray[0].values[0].rho_tor_norm[4],3)))
#plt.errorbar(bin_id, Te_avg[:,5], stdev[:,5], linestyle='--', marker='^', color='c', label=str(round(cpo.coretranspArray[0].values[0].rho_tor_norm[5],3)))
#plt.errorbar(bin_id, Te_avg[:,6], stdev[:,6], linestyle='--', marker='o', color='y', label=str(round(cpo.coretranspArray[0].values[0].rho_tor_norm[6],3)))
#plt.errorbar(bin_id, Te_avg[:,7], stdev[:,7], linestyle='--', marker='s', color='b', label=str(round(cpo.coretranspArray[0].values[0].rho_tor_norm[7],3)))

plt.legend(title=r'$||\rho_{tor}||$')
plt.xlim((-1, num_bins))
plt.xlabel('time bin ID ('+str(tstep_per_bin)+' tsteps / bin)')
plt.ylabel('<Te>')
plt.show()
