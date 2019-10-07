# load data #
#equil_code = "chease"
execfile("/pfs/home/g2oluk/Garching/compat/trunk/plots/slice-bind.py")

import numpy as np
import matplotlib.pyplot as plt
from matplotlib import gridspec
from scipy.interpolate import interp1d


# fluxtubes #
nft = 8							# number of fluxtubes
ft_rho_tor = cpo.coretranspArray[0].values[0].rho_tor	# rho_tor of each fluxtube

# time array #
time = range(coret_len)			# time step ID

# Te array from coreprof #
rho_tor_grid = cpo.coreprofArray[0].rho_tor	# grid location


t = []
for i in time:
    t.append(cpo.coreprofArray[i].time)

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




# Ti from coreprof #
Ti_cp = []
for i in time:
    Ti_cp.append(cpo.coreprofArray[i].ti.value[:,0])
Ti_coreprof = np.array(Ti_cp)


# Ti interpolated at fluxtube locations #
Ti_int = []
for i in time:
    Ti_int.append(interp1d(rho_tor_grid,Ti_coreprof[i])(ft_rho_tor))
Ti_interp = np.array(Ti_int)




#######
# calculate average values
group_bin = 200					# number of bin in each group
num_group_bins = int((coret_len-1)/group_bin)	# total number of groups
group_id = range(num_group_bins)
#group_limit = 0.05			# variation allowed within averaged Te value
sat_dur = 5				# number of previous consecutive groups 
					# with <Te> within current group_limit to be 
					# considered as saturation satisfied


Te_group_avg = np.empty([num_group_bins,nft])	# averaged Te within group
group_stdev = np.empty([num_group_bins,nft])	# standard dev of each group
upper = np.empty([num_group_bins,nft])		# upper limit of variation
lower = np.empty([num_group_bins,nft])		# lower limit of variation
Ti_group_avg = np.empty([num_group_bins,nft])   # averaged Ti within group
Ti_group_stdev = np.empty([num_group_bins,nft])    # standard dev of each Ti group

for i in group_id:
    a = i*group_bin
    b = (i+1)*group_bin-1
    print "i="+str(i)

    for j in range(nft):
        Te_group_avg[i,j] = np.mean(Te_interp[a:b,j])
        print "j="+str(j)+", Te_group_avg="+str(Te_group_avg[i,j])
    
        group_stdev[i,j] = np.std(Te_interp[a:b,j])
        upper[i,j] = Te_group_avg[i,j] + 1.0*group_stdev[i,j]
        lower[i,j] = Te_group_avg[i,j] - 1.0*group_stdev[i,j]

        Ti_group_avg[i,j] = np.mean(Ti_interp[a:b,j])
        Ti_group_stdev[i,j] = np.std(Ti_interp[a:b,j])

            
#        upper[i,j] = (1.0 + group_limit)*Te_group_avg[i,j]
#        lower[i,j] = (1.0 - group_limit)*Te_group_avg[i,j]

        for k in range(sat_dur):
            if (i-k-1 >= 0):
                if not (lower[i,j] <= Te_group_avg[i-k-1,j] <= upper[i,j]):
                    print "Te_group_avg["+str(i-k-1)+","+str(j)+"] = "+str(Te_group_avg[i-k-1,j])+" exceeds by "
                    print str(abs(Te_group_avg[i-k-1,j]-Te_group_avg[i,j])/Te_group_avg[i,j]*100)+"%"

#print "group_bin = "+str(group_bin)
#print "x0 = "+str(cpo.coretranspArray[0].values[0].rho_tor_norm[:])
#print "x2000 = "+str(cpo.coretranspArray[2000].values[0].rho_tor_norm[:])
#print "n_group_avg[9,:] = "+str(Te_group_avg[9,:])
#print "group_stdev[9,:] = "+str(group_stdev[9,:])

#######
plt.figure(1)

x =np.empty(nft)
for j in range(nft):
    x[j] = cpo.coretranspArray[0].values[0].rho_tor_norm[j]



# plot averaged value of Te at every group bin
plt.suptitle("Time-averaged electron temperature (<"+r'$T_e$'+">) at "+str(group_bin)+" time steps / group\nAUG#28906/6,  GEM0+IMP4DV, min(T_flux)=0.0, steps 0-20: "+r'$Te_{lim}=0.2, dTe_{lim}=0.2, max(\tau)=1e-2$'+"\n GEM+IMP4DV, min(T_flux)=0.0, ndg=50, steps 21-9999: "+r'$Te_{lim}=0.1, dTe_{lim}=0.05, max(\tau)=1e-2$')

for i in range(nft):
    plt.errorbar(group_id, Te_group_avg[:,i], group_stdev[:,i], linestyle='--', marker='o', linewidth=2, capsize=10, label=str(round(cpo.coretranspArray[0].values[0].rho_tor_norm[i],3)))
plt.vlines(5, 10, 3000, linestyles='dotted')
for i in range(nft):
    plt.hlines(Te_group_avg[5,i], 0, num_group_bins-1, linestyles='dotted')

plt.legend(title=r'$||\rho_{tor}||$')
plt.xlabel('time group ID ('+str(group_bin)+' tsteps / group)')
plt.ylabel(r'$<T_e>$'+' (eV)')
plt.show()

#plt.figure(2)
#plt.suptitle("T-profiles at different various time groups, with "+str(group_bin)+" time steps / group\nAUG#28906/6,  GEM0+IMP4DV, min(T_flux)=0.0, steps 0-20: "+r'$Te_{lim}=0.2, dTe_{lim}=0.2, max(\tau)=1e-2$'+"\n GEM+IMP4DV, min(T_flux)=0.0, ndg=50, steps 21-9000: "+r'$Te_{lim}=0.1, dTe_{lim}=0.05, max(\tau)=1e-2$')
#plt.subplot(211)
#plt.errorbar(cpo.coretranspArray[2999].values[0].rho_tor_norm[:], Te_group_avg[14,:], group_stdev[14,:], marker='o', linewidth=2, capsize=10, label="14, "+str(round(cpo.coreprofArray[200*(14+1)-1].time,3)))
#plt.errorbar(cpo.coretranspArray[4999].values[0].rho_tor_norm[:], Te_group_avg[24,:], group_stdev[24,:], marker='o', linewidth=2, capsize=10, label="24, "+str(round(cpo.coreprofArray[200*(24+1)-1].time,3)))
#plt.errorbar(cpo.coretranspArray[6999].values[0].rho_tor_norm[:], Te_group_avg[34,:], group_stdev[34,:], marker='o', linewidth=2, capsize=10, label="34, "+str(round(cpo.coreprofArray[200*(34+1)-1].time,3)))
#plt.errorbar(cpo.coretranspArray[8999].values[0].rho_tor_norm[:], Te_group_avg[44,:], group_stdev[44,:], marker='o', linewidth=2, capsize=10, label="44, "+str(round(cpo.coreprofArray[200*(44+1)-1].time,3)))
#plt.legend(title='group #, time(s)', fontsize=14)
#plt.ylabel(r'$<T_e>$'+' (eV)', fontsize=14)
#
#plt.subplot(212)
#plt.errorbar(cpo.coretranspArray[2999].values[0].rho_tor_norm[:], Ti_group_avg[14,:], Ti_group_stdev[14,:], marker='o', linewidth=2, capsize=10, label=str(round(cpo.coreprofArray[200*(14+1)-1].time,3)))
#plt.errorbar(cpo.coretranspArray[4999].values[0].rho_tor_norm[:], Ti_group_avg[24,:], Ti_group_stdev[24,:], marker='o', linewidth=2, capsize=10, label=str(round(cpo.coreprofArray[200*(24+1)-1].time,3)))
#plt.errorbar(cpo.coretranspArray[6999].values[0].rho_tor_norm[:], Ti_group_avg[34,:], Ti_group_stdev[34,:], marker='o', linewidth=2, capsize=10, label=str(round(cpo.coreprofArray[200*(34+1)-1].time,3)))
#plt.errorbar(cpo.coretranspArray[8999].values[0].rho_tor_norm[:], Ti_group_avg[44,:], Ti_group_stdev[44,:], marker='o', linewidth=2, capsize=10, label=str(round(cpo.coreprofArray[200*(44+1)-1].time,3)))
#plt.xlabel(r'$||\rho_{tor}||$', fontsize=14)
#plt.ylabel(r'$<T_i>$'+' (eV)', fontsize=14)
plt.show()
