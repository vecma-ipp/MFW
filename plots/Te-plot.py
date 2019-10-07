# load data #
equil_code = "chease"
do_equil = False
execfile("/pfs/home/g2oluk/Garching/compat/trunk/plots/slice-bind.py")
#execfile("./slice-bind.py")

import numpy as np
import matplotlib.pyplot as plt
from matplotlib import gridspec
from scipy.interpolate import interp1d

#plt.rcParams['axes.facecolor'] = 'black'

# fluxtubes #
nft = 8						# number of fluxtubes
ft_rho_tor = cpo.coretranspArray[0].values[0].rho_tor	# rho_tor of each fluxtube

# time array #
time = range(coret_len)			# time step ID (-1 to exclude the incomplete step)
#time = range(4000)

# Te array from coreprof #
rho_tor_grid = cpo.coreprofArray[0].rho_tor	# grid location

# bin size for averaging Te #
tstep_per_bin = 1				# number of time steps per bin
num_bins = int((coret_len-1)/tstep_per_bin)	# number of bins
#num_bins = int((4000)/tstep_per_bin)     # number of bins
bin_id = range(num_bins)			# ID for each bin

t = []
for i in time:
    t.append(cpo.coreprofArray[i].time)

# Te from coreprof #
Te_cp = []
for i in time:
    Te_cp.append(cpo.coreprofArray[i].ne.value)#[:,0])
Te_coreprof = np.array(Te_cp)


# Te interpolated at fluxtube locations #
Te_int = []
for i in time:
    Te_int.append(interp1d(rho_tor_grid,Te_coreprof[i])(ft_rho_tor))
Te_interp = np.array(Te_int)
#print "Te_interp(tstep=1000)="+str(Te_interp[1000])


# averaging Te and calculating standard dev. for every time bin #
Te_avg = np.empty([num_bins,nft])
stdev = np.empty([num_bins,nft])
for i in bin_id:
    a = i*tstep_per_bin
    b = (i+1)*tstep_per_bin-1

    for j in range(nft):
        Te_avg[i,j] = np.mean(Te_interp[a:b,j])
        stdev[i,j] = np.std(Te_interp[a:b,j])


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


#colormap = plt.cm.gist_ncar
#plt.gca().set_color_cycle([colormap(i) for i in np.linspace(0.0,0.9,nft)])



#*#*#*
f = open('ComPat_Te.out','w')

for i in time:
    f.write("%.5f %.12f %.12f %.12f %.12f %.12f %.12f %.12f %.12f\n" % (t[i], Te_interp[i,0], Te_interp[i,1], Te_interp[i,2], Te_interp[i,3], Te_interp[i,4], Te_interp[i,5], Te_interp[i,6], Te_interp[i,7]))

f.close()
#*#*#*
#######
# calculate average values
group_bin = 200
#group_bin = 250				# number of bin in each group
num_group_bins = int((coret_len-1)/group_bin)
#num_group_bins = int((4000)/group_bin)
#num_group_bins = num_bins-group_bin+1	# total number of groups
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
#    a = i*tstep_per_bin
    b = (i+1)*group_bin-1
#    b = (i+group_bin)*tstep_per_bin-1
    print "i="+str(i)

    for j in range(nft):
        Te_group_avg[i,j] = np.mean(Te_interp[a:b,j])
        print "j="+str(j)+", Te_group_avg="+str(Te_group_avg[i,j])
    
        group_stdev[i,j] = np.std(Te_interp[a:b,j])
        upper[i,j] = Te_group_avg[i,j] + 1.0*group_stdev[i,j]
        lower[i,j] = Te_group_avg[i,j] - 1.0*group_stdev[i,j]

        Ti_group_avg[i,j] = np.mean(Ti_interp[a:b,j])
        Ti_group_stdev[i,j] = np.std(Ti_interp[a:b,j])

#        if (i == 0):
#	    print "at group_bin = "+str(i)+", and fluxtube #"+str(j)
#	    print "Te_group_avg = "+str(Te_group_avg[i,j])
#            print "group_stdev =  "+str(group_stdev[i,j])
#        elif (i == 15):
#            print "at group_bin = "+str(i)+", and fluxtube #"+str(j)
#            print "Te_group_avg = "+str(Te_group_avg[i,j])
#            print "group_stdev =  "+str(group_stdev[i,j])
            
#        upper[i,j] = (1.0 + group_limit)*Te_group_avg[i,j]
#        lower[i,j] = (1.0 - group_limit)*Te_group_avg[i,j]

#        for k in range(group_bin):
#            if not (lower[i,j] <= Te_avg[i+k,j] <= upper[i,j]):
#                print "Te_avg["+str(i+k)+","+str(j)+"] = "+str(Te_avg[i+k,j])
        for k in range(sat_dur):
            if (i-k-1 >= 0):
#                print "i-k-1="+str(i-k-1)
                if not (lower[i,j] <= Te_group_avg[i-k-1,j] <= upper[i,j]):
#                if not (lower[i,j] <= Te_group_avg[i-k-1,j] <= upper[i,j]):
                    print "Te_group_avg["+str(i-k-1)+","+str(j)+"] = "+str(Te_group_avg[i-k-1,j])+" exceeds by "
                    print str(abs(Te_group_avg[i-k-1,j]-Te_group_avg[i,j])/Te_group_avg[i,j]*100)+"%"

print "group_bin = "+str(group_bin)
print "x0 = "+str(cpo.coretranspArray[0].values[0].rho_tor_norm[:])
print "x2000 = "+str(cpo.coretranspArray[2000].values[0].rho_tor_norm[:])
print "n_group_avg[9,:] = "+str(Te_group_avg[9,:])
print "group_stdev[9,:] = "+str(group_stdev[9,:])

#######
# plot averaged value of Te at every time bin
#-#plt.figure(1)
#plt.suptitle("Time-averaged Te ("+str(tstep_per_bin)+" tsteps per bin) imp4dv,\nspread, tau=0.01, Te_lim=0.2, dTe_lim=0.1, ndg=50")
#plt.suptitle("Time-averaged Te ("+str(tstep_per_bin)+" tsteps per bin),\nspread, tau=0.01, Te_lim=0.4, dTe_lim=0.2, ndg=50")
#-#plt.suptitle("Time-averaged Te ("+str(tstep_per_bin)+" tsteps per bin) imp4dv, min(T_flux)=0.0,\nspread, tau=0.01, steps 0-4000: te_lim=0.2, dTe_lim=0.1, steps 4001-7000: te_lim=0.1, dTe_lim=0.05, ndg=50")

#-#for i in range(nft):
#-#    plt.errorbar(bin_id, Te_avg[:,i], stdev[:,i], linestyle='--', marker='o', linewidth=2, capsize=10, label=str(round(cpo.coretranspArray[0].values[0].rho_tor_norm[i],3)))

#-#plt.legend(title=r'$||\rho_{tor}||$')
#-#plt.xlim((-1, num_bins))
#-#plt.xlabel('time bin ID ('+str(tstep_per_bin)+' tsteps / bin)')
#-#plt.ylabel('<Te>')
#-#plt.show()

# plot averaged value of Te vs. rho_tor at the last time bin
#fig1 = plt.figure(1,figsize=(12,9),dpi=100)
plt.figure(1)
#ol#plt.suptitle("Electron temperature profile at quasi-steady state")
#o#plt.suptitle("Te Profile, "+str(group_bin)+" tsteps per group, imp4dv, min(T_flux)=0.0, \nWorkflow comparison, spread, tau=0.01, Te_lim=0.2, dTe_lim=0.1, ndg=50", fontsize=16)

#ol#x0=np.empty(nft)
#ol#y0=np.empty(nft)
x =np.empty(nft)
#ol#y =np.empty(nft)

#ol#y0_no = [ 1823.61434079, 1786.56249991, 1609.12858726, 1335.15903567, 1062.48169479, 803.456071501, 509.867869782, 241.261315034]
#ol#std0=[ 231.355706889, 158.838730554, 96.6844086298, 109.363202828, 109.040384483, 81.8866118784, 79.684427123, 63.263487882]
#ol#y_no = [ 1313.62762071, 1262.68368619, 1150.40054813, 964.301993866, 780.348254844, 589.217298427, 371.715639582, 193.755362901]
#ol#std=[ 118.336939995, 85.3450142982, 68.6003247045, 69.1405444654, 62.0167905907, 38.6501497766, 30.6826059118, 18.3403273601]

for j in range(nft):
#ol#    x0[j]= cpo.coretranspArray[0].values[0].rho_tor_norm[j]
#ol#    y0[j]= Te_group_avg[0,j]
    x[j] = cpo.coretranspArray[0].values[0].rho_tor_norm[j]
#ol#    y[j] = Te_group_avg[16,j]#num_group_bins-1,j]


#plt.errorbar(x0, y0, group_stdev[0,:], linestyle='--', marker='o', linewidth=2, capsize=10, label=str(round(t[group_bin-1],3))+" (new)")#str(cpo.coreprofArray[tstep_per_bin-1].time))
#plt.errorbar(x0, y0_no, std0, linestyle='--', marker='o', linewidth=2, capsize=10, label="0.069 (old)")#str(cpo.coreprofArray[tstep_per_bin-1].time))
#ol#plt.errorbar(x, y, group_stdev[16,:], linestyle='--', marker='o', linewidth=2, capsize=10, label=r'$WF_{async}$')#"parallel")#str(round(t[(16+1)*group_bin-1],3))+" (new)")
#str(cpo.coreprofArray[num_bins*tstep_per_bin-1].time))
#ol#plt.errorbar(x, y_no, std, linestyle='--', marker='o', linewidth=2, capsize=10, label=r'$WF_{sync}$')#"serial")#"1.235 (old)")

#plt.errorbar(x0, y0, group_stdev[0,:], linestyle='--', marker='o', linewidth=2, capsize=10, label="initial (parallel)")#"+r'$\tau = 1e-3$'+")")#parallel)")#str(cpo.coreprofArray[tstep_per_bin-1].time))
#plt.errorbar(x0, y0_no, std0, linestyle='--', marker='o', linewidth=2, capsize=10, label="initial (serial)")#adaptive "+r'$\tau$'+")")#serial)")#str(cpo.coreprofArray[tstep_per_bin-1].time))
#plt.errorbar(x, y, group_stdev[16,:], linestyle='--', marker='o', linewidth=2, capsize=10, label="saturation (parallel)")#"+r'$\tau = 1e-3$'+")")#parallel)")#str(cpo.coreprofArray[num_bins*tstep_per_bin-1].time))
#plt.errorbar(x, y_no, std, linestyle='--', marker='o', linewidth=2, capsize=10, label="saturation (serial)")#adaptive "+r'$\tau$'+")")#serial)")

#plt.legend(title='time (s)', fontsize=14)
#plt.legend(title='time bin', fontsize=14)
#plt.legend(title='workflow', fontsize=14)
#ol#plt.legend(fontsize=14)
#ol#plt.xlim((0.0, 1.0))
#ol#plt.xlabel(r'$||\rho_{tor}||$', fontsize=14)
#ol#plt.ylabel(r'$<T_e>$'+' (eV)', fontsize=14)
#ol#plt.xticks(fontsize=14)
#ol#plt.yticks(fontsize=14)
#ol#plt.show()

#ol#print "rho_tor0="+str(x0)
#ol#print "<Te0>="+str(y0)
#ol#print "std0="+str(group_stdev[0,:])
#ol#print "rho_tor="+str(x)
#ol#print "<Te>="+str(y)
#ol#print "std="+str(group_stdev[num_group_bins-1,:])

# plot averaged value of Te at every group bin
#ol#plt.figure(2)
plt.suptitle("Time-averaged electron temperature (<"+r'$T_e$'+">) at "+str(group_bin)+" time steps / group\nAUG#28906/6,  GEM0+IMP4DV, min(T_flux)=0.0, steps 0-20: "+r'$Te_{lim}=0.2, dTe_{lim}=0.2, max(\tau)=1e-2$'+"\n GEM+IMP4DV, min(T_flux)=0.0, ndg=50, steps 21-9999: "+r'$Te_{lim}=0.1, dTe_{lim}=0.05, max(\tau)=1e-2$')
#plt.suptitle("Time-averaged Te ("+str(tstep_per_bin)+" tsteps per bin) imp4dv,\nspread, tau=0.01, Te_lim=0.2, dTe_lim=0.1, ndg=50")
#plt.suptitle("Time-averaged Te ("+str(tstep_per_bin)+" tsteps per bin),\nspread, tau=0.01, Te_lim=0.4, dTe_lim=0.2, ndg=50")
#plt.suptitle("Time-averaged Te ("+str(group_bin)+" tsteps per group) imp4dv, min(T_flux)=0.0,\nspread, tau=0.01, steps 0-4000: te_lim=0.2, dTe_lim=0.1, ndg=50\nsaturation occurs at group #6, when <Te> in the previous 5 groups remain within 1.0 std_dev of current group")
#o#plt.suptitle("Time-averaged Te ("+str(group_bin)+" tsteps per group) imp4dv, min(T_flux)=0.0,\nspread, tau=0.01, te_lim=0.2, dTe_lim=0.1, ndg=50\nsaturation occurs at group #10, when <Te> in the previous 5 groups remain within 1.0 std_dev of current group")

for i in range(nft):
    plt.errorbar(group_id, Te_group_avg[:,i], group_stdev[:,i], linestyle='--', marker='o', linewidth=2, capsize=10, label=str(round(cpo.coretranspArray[0].values[0].rho_tor_norm[i],3)))
#    plt.plot(group_id, Te_group_avg[:,i], linestyle='--', marker='o', linewidth=2, label=str(round(cpo.coretranspArray[0].values[0].rho_tor_norm[i],3)))
plt.vlines(5, 10, 3000, linestyles='dotted')
for i in range(nft):
    plt.hlines(Te_group_avg[5,i], 0, num_group_bins-1, linestyles='dotted')

plt.legend(title=r'$||\rho_{tor}||$')
#plt.xlim((-1, num_bins))
plt.xlabel('time group ID ('+str(group_bin)+' tsteps / group)')
plt.ylabel(r'$<T_e>$'+' (eV)')
plt.show()

y100 = [2245.29842659,  1955.51708748,  1682.31336107,  1386.955624,                    1139.79841444,   876.13782724,   629.84242051,   347.23831579]
y150 = [2230.95044604,  1932.68306862,  1654.68350387,  1358.17944489,                  1108.97583168,   864.02386172,   627.16495684,   345.95865573]
y200 = [2225.15479827,  1902.61001111,  1617.185137,    1315.75016882,                  1062.23136931,   834.65332835,   608.3857304,    336.90771107]
y250 = [2142.33916961,  1861.6666146,   1592.80966745,  1316.99377609,                  1082.62252421,   843.27597606,   608.997234,     337.36953223]
y300 = [2234.93864553,  1895.1506727,   1588.04131764,  1300.05924894,                  1073.33046023,   847.77020036,   617.01275994,   340.06157503]

std100 = [174.66561938,  122.98641396,   97.60257911,  106.78345637,                      100.53565467,   96.30870548,   78.56749228,   23.0528998 ]
std150 = [169.46535613,  120.07588521,  100.31537027,  118.1166859,                       108.60898533,   95.43738995,   74.40810274,   21.35500098]
std200 = [188.61658569,  154.30621208,  124.48457111,  101.13598795,                      112.79976549,  101.26074643,   68.76996221,   22.66809821]
std250 = [218.48399808,  140.39018599,  105.04480971,  106.96741828,                      107.54415528,   95.23198212,   68.98498191,   19.17991277]
std300 = [138.48332232,  125.14009026,  105.69941543,  104.17950846,                      100.7651149,    93.5955501,    73.72465327,   21.83995515]

i100 = [1466.78334467,  1407.115178,    1225.57934643,  1036.39946167,                   889.71063196,   711.65757488,   553.03552337,   350.69443626]
i150 = [1543.73714286,  1384.53857321,  1179.06084695,   997.58645309,                   848.71947097,   689.24925979,   540.68243813,   345.3550045]
i200 = [1593.71314567,  1358.46465853,  1154.33467132,   978.12646981,                   832.59354702,   679.86394428,   527.10113679,   336.10130172]
i250 = [1465.08857804,  1359.14935487,  1156.58407087,   991.06262946,                   840.73582279,   674.45501396,   521.20393887,   333.52017982]
i300 = [1584.29683389,  1369.75897785,  1155.03473312,   977.41722331,                   830.77570478,   676.75611468,   528.52911562,   336.80653165]

istd100 = [261.05589374,  171.38148121,  123.50289924,  119.80312908,                      112.48737857,   99.79796798,   91.51815066,   37.63588706]
istd150 = [267.77849764,  170.5125882,   138.30966288,  130.36406615,                      118.87786297,   98.50143753,   86.05090179,   35.35320852]
istd200 = [292.06030249,  191.6322064,   139.18442249,  123.21309757,                      128.58897775,  114.1755903,    86.15756748,   36.43126036]
istd250 = [309.91497279,  187.21760084,  139.8402982,   135.29053467,                      124.56709246,  105.63273883,   84.80931178,   33.87609256]
istd300 = [227.81950954,  181.69705379,  147.26502879,  135.80343178,                      119.95698523,  103.41141402,   89.59738656,   37.78140303]
plt.figure(2)
#plt.suptitle("AUG28906/6, Comparison between quasi-steady state profiles, from different # time steps/group")
plt.suptitle("T-profiles at different various time groups, with "+str(group_bin)+" time steps / group\nAUG#28906/6,  GEM0+IMP4DV, min(T_flux)=0.0, steps 0-20: "+r'$Te_{lim}=0.2, dTe_{lim}=0.2, max(\tau)=1e-2$'+"\n GEM+IMP4DV, min(T_flux)=0.0, ndg=50, steps 21-9000: "+r'$Te_{lim}=0.1, dTe_{lim}=0.05, max(\tau)=1e-2$')
plt.subplot(211)
plt.errorbar(cpo.coretranspArray[2999].values[0].rho_tor_norm[:], Te_group_avg[14,:], group_stdev[14,:], marker='o', linewidth=2, capsize=10, label="14, "+str(round(cpo.coreprofArray[200*(14+1)-1].time,3)))
plt.errorbar(cpo.coretranspArray[4999].values[0].rho_tor_norm[:], Te_group_avg[24,:], group_stdev[24,:], marker='o', linewidth=2, capsize=10, label="24, "+str(round(cpo.coreprofArray[200*(24+1)-1].time,3)))
plt.errorbar(cpo.coretranspArray[6999].values[0].rho_tor_norm[:], Te_group_avg[34,:], group_stdev[34,:], marker='o', linewidth=2, capsize=10, label="34, "+str(round(cpo.coreprofArray[200*(34+1)-1].time,3)))
plt.errorbar(cpo.coretranspArray[8999].values[0].rho_tor_norm[:], Te_group_avg[44,:], group_stdev[44,:], marker='o', linewidth=2, capsize=10, label="44, "+str(round(cpo.coreprofArray[200*(44+1)-1].time,3)))
#plt.errorbar(x, y100, std100, linestyle='--', marker='o', linewidth=2, capsize=10, label="100, "+str(round(cpo.coreprofArray[100*(9+1)-1].time,3)))
#plt.errorbar(x, y150, std150, linestyle='--', marker='o', linewidth=2, capsize=10, label="150, "+str(round(cpo.coreprofArray[150*(6+1)-1].time,3)))
#plt.errorbar(x, y200, std200, linestyle='--', marker='o', linewidth=2, capsize=10, label="200, "+str(round(cpo.coreprofArray[200*(7+1)-1].time,3)))
#plt.errorbar(x, y250, std250, linestyle='--', marker='o', linewidth=2, capsize=10, label="250, "+str(round(cpo.coreprofArray[250*(7+1)-1].time,3)))
#plt.errorbar(x, y300, std300, linestyle='--', marker='o', linewidth=2, capsize=10, label="300, "+str(round(cpo.coreprofArray[300*(6+1)-1].time,3)))
plt.legend(title='group #, time(s)', fontsize=14)
#plt.legend(title='steps/group, time(s)', fontsize=14)
#plt.xlabel(r'$||\rho_{tor}||$', fontsize=14)
plt.ylabel(r'$<T_e>$'+' (eV)', fontsize=14)

plt.subplot(212)
plt.errorbar(cpo.coretranspArray[2999].values[0].rho_tor_norm[:], Ti_group_avg[14,:], Ti_group_stdev[14,:], marker='o', linewidth=2, capsize=10, label=str(round(cpo.coreprofArray[200*(14+1)-1].time,3)))
plt.errorbar(cpo.coretranspArray[4999].values[0].rho_tor_norm[:], Ti_group_avg[24,:], Ti_group_stdev[24,:], marker='o', linewidth=2, capsize=10, label=str(round(cpo.coreprofArray[200*(24+1)-1].time,3)))
plt.errorbar(cpo.coretranspArray[6999].values[0].rho_tor_norm[:], Ti_group_avg[34,:], Ti_group_stdev[34,:], marker='o', linewidth=2, capsize=10, label=str(round(cpo.coreprofArray[200*(34+1)-1].time,3)))
plt.errorbar(cpo.coretranspArray[8999].values[0].rho_tor_norm[:], Ti_group_avg[44,:], Ti_group_stdev[44,:], marker='o', linewidth=2, capsize=10, label=str(round(cpo.coreprofArray[200*(44+1)-1].time,3)))
#plt.errorbar(x, i100, istd100, linestyle='--', marker='o', linewidth=2, capsize=10, label="100, "+str(round(cpo.coreprofArray[100*(9+1)-1].time,3)))
#plt.errorbar(x, i150, istd150, linestyle='--', marker='o', linewidth=2, capsize=10, label="150, "+str(round(cpo.coreprofArray[150*(6+1)-1].time,3)))
#plt.errorbar(x, i200, istd200, linestyle='--', marker='o', linewidth=2, capsize=10, label="200, "+str(round(cpo.coreprofArray[200*(7+1)-1].time,3)))
#plt.errorbar(x, i250, istd250, linestyle='--', marker='o', linewidth=2, capsize=10, label="250, "+str(round(cpo.coreprofArray[250*(7+1)-1].time,3)))
#plt.errorbar(x, i300, istd300, linestyle='--', marker='o', linewidth=2, capsize=10, label="300, "+str(round(cpo.coreprofArray[300*(6+1)-1].time,3)))
#plt.legend(title='steps/group, time(s)', fontsize=14)
plt.xlabel(r'$||\rho_{tor}||$', fontsize=14)
plt.ylabel(r'$<T_i>$'+' (eV)', fontsize=14)
plt.show()
