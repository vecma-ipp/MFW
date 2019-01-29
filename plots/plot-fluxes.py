### load data ###
equil_code = "chease"
turb_code = "imp4dv"
do_equil = False
do_avg   = False
do_Ti    = False	# plot Ti
do_vmark = True		# plot vertical markers every time simulation run ends
do_limit = False		# plot vertical markers every time acceptance limits change
execfile("/gh/g2olivh/public/pyscripts/plots/slice-bind.py")

import matplotlib.pyplot as plt
from matplotlib import gridspec
from matplotlib import colors as mcolors
import numpy as np
#pylab.ion()
from scipy.interpolate import interp1d

colors = dict(mcolors.BASE_COLORS, **mcolors.CSS4_COLORS)

nftubes = 8

fluxlog = True
tau = 0.01
times=range(coret_len)
#times=range(1000)
#times=range(150)
#times=range(40,395,1)

times_len = len(times)

### time steps in which we end each simulation run ###
tstops = [20, 70, 120, 170, 220, 600]		# define time steps to place vertical markers
num_tstops = len(tstops)
ran_tstops = range(num_tstops)

### time steps in which we change the acceptance limits ###
tlimits = [20, 70, 120, 170, 220, 600]
num_tlimits = len(tlimits)
ran_tlimits = range(num_tlimits)

full_rho_tor = cpo.coreprofArray[0].rho_tor
ft_rho_tor = cpo.coretranspArray[0].values[0].rho_tor

### times ###
#init_time = cpo.coreprofArray[0].time
init_time = 0
t = []
for i in times:
    t.append(cpo.coreprofArray[i].time-init_time)


### fluxes ###
f=[]
for i in times:
    if do_Ti:
        f.append(cpo.coretranspArray[i].values[0].ti_transp.flux)
    else:
        f.append(cpo.coretranspArray[i].values[0].te_transp.flux)
nf = np.array(f)

### Te ###
te=[]
for i in times:
    te.append(interp1d(full_rho_tor,cpo.coreprofArray[i].te.value)(ft_rho_tor))
nte = np.array(te)

### Ti ###
ti=[]
for i in times:
    ti.append(interp1d(full_rho_tor,cpo.coreprofArray[i].ti.value[:,0])(ft_rho_tor))
nti = np.array(ti)

### Ne ###
#n=[]
#for i in range(times):
#    n.append(interp1d(full_rho_tor,cpo.coreprofArray[i].ne.value)(ft_rho_tor))
#nn = np.array(n)


### plot ###
colormap = plt.cm.gist_ncar

plt.figure(1)
plt.suptitle("AUG#28906/4, imp4dv, ndg=50, "+r'$Te_{dev}=0.2, dTe_{dev}=0.1, max(\tau)=$'+str(tau))

gs = gridspec.GridSpec(7,1,wspace=0,hspace=0)


#plt.subplot(311)
ax = plt.subplot(gs[0:-4,:])
plt.gca().set_color_cycle([colormap(i) for i in np.linspace(0.0,0.9,nftubes)])
plt.subplots_adjust(wspace=0, hspace=0)
for i in range(nftubes):
    if fluxlog:
        plt.semilogy(t[:],nf[:,i],'+-',label=str(round(cpo.coretranspArray[0].values[0].rho_tor_norm[i],3)))
    else:
        plt.plot(t[:],nf[:,i],'+-',label=str(round(cpo.coretranspArray[0].values[0].rho_tor_norm[i],3)))
    #plt.semilogy(t[:],nf[:,i],'+-',label=str(round(cpo.coretranspArray[0].values[0].rho_tor_norm[i],3)))
if do_vmark:
    for k in ran_tstops:
        alpha = tstops[k]
        xc = t[alpha]
        plt.axvline(x=xc, color='0.5', linestyle='--')
if do_limit:
    for k in ran_tlimits:
        alpha = tlimits[k]
        xc = t[alpha]
        plt.axvline(x=xc, color=colors['brown'], linestyle=':')
ylab = ax.get_yticklabels()
ylab[0].set_visible(False)
#plt.legend(loc="best")
#plt.xlabel("time")

plt.tick_params(
    axis='x',          # changes apply to the x-axis
    which='both',      # both major and minor ticks are affected
    bottom='off',      # ticks along the bottom edge are off
    top='off',         # ticks along the top edge are off
    labelbottom='off') # labels along the bottom edge are off
plt.ylabel("flux")

l = plt.legend(bbox_to_anchor=(0.5,1), loc='lower center', ncol=nftubes, title=r'$||\rho_{tor}||$')
lt = l.get_title()
lt.set_position((0.0,-5.0))


ax = plt.subplot(gs[3,:])
tstep=[0.]
for i in range(len(times)-1):
    tstep.append(t[i+1]-t[i])
plt.semilogy(t[:],tstep[:],'*')
#plt.plot(t[:],tstep[:],'*')
plt.ylim(0.000001,0.0999)
print tstep
if do_vmark:
    for k in ran_tstops:
        alpha = tstops[k]
        xc = t[alpha]
        plt.axvline(x=xc, color='0.5', linestyle='--')
if do_limit:
    for k in ran_tlimits:
        alpha = tlimits[k]
        xc = t[alpha]
        plt.axvline(x=xc, color=colors['brown'], linestyle=':')
ylab = ax.get_yticklabels()
ylab[0].set_visible(False)
ylab[-1].set_visible(False)

plt.tick_params(
    axis='x',          # changes apply to the x-axis
    which='both',      # both major and minor ticks are affected
    bottom='off',      # ticks along the bottom edge are off
    top='off',         # ticks along the top edge are off
    labelbottom='off') # labels along the bottom edge are off
plt.ylabel("time step")


#plt.subplot(312)
ax = plt.subplot(gs[4:,:])
plt.gca().set_color_cycle([colormap(i) for i in np.linspace(0.0,0.9,nftubes)])
plt.subplots_adjust(wspace=0, hspace=0)
for i in range(nftubes):
    if do_Ti:
        plt.plot(t[:],nti[:,i],'+-',label=str(round(cpo.coretranspArray[0].values[0].rho_tor_norm[i],3)))
    else:
        plt.plot(t[:],nte[:,i],'+-',label=str(round(cpo.coretranspArray[0].values[0].rho_tor_norm[i],3)))
#plt.legend(loc="best")

if do_avg:
    meanwidth = 1./3 # between 0 and 1
    imean = np.empty(times_len)
    for i in range(nftubes):
        if do_Ti:
            imean.fill(nti[int(times_len*(1-meanwidth)):times_len-1,i].mean())
        else:
            imean.fill(nte[int(times_len*(1-meanwidth)):times_len-1,i].mean())
        plt.plot(t[int(times_len*(1-meanwidth)):times_len-1],imean[int(times_len*(1-meanwidth)):times_len-1],'--',lw=3.0)
        plt.text(t[-1]*1.01,imean[-1],str(round(imean[-1],2)))

plt.xlabel("time")

if do_Ti:
    plt.ylabel("Ti")
else:
    plt.ylabel("Te")
if do_vmark:
    for k in ran_tstops:
        alpha = tstops[k]
        xc = t[alpha]
        plt.axvline(x=xc, color='0.5', linestyle='--')
if do_limit:
    for k in ran_tlimits:
        alpha = tlimits[k]
        xc = t[alpha]
        plt.axvline(x=xc, color=colors['brown'], linestyle=':')

ylab = ax.get_yticklabels()
ylab[-1].set_visible(False)

#xpoints = np.arange(0,times,1)
#xlabels = []
#for i in range(len(xpoints)):
#    xlabels.append(t[xpoints[i]])

#plt.xticks(xpoints,xlabels,rotation=90)




#plt.subplot(313)
#for i in range(8):
#    plt.plot(nn[:,i],'+-',label=r'$\rho_{tor}$'+str(round(cpo.coretranspArray[0].values[0].rho_tor[i],3)))
#plt.legend(loc="best")
#plt.xlabel("time")
#plt.ylabel("Ne")

plt.show(block=True)





## plot time history of time step size
#plt.figure(2)
#plt.suptitle("Adaptive Time Steps, imp4dv, min(T_flux)=0.0, \nspread, tau=0.01, Te_lim=0.2, dTe_lim=0.1, ndg=50", fontsize=16)
#
##for i in range(len(times)-1):
#plt.semilogy(t[0:100],tstep[0:100],'*')
##plt.plot(t[:],tstep[:],'*')
#plt.ylim(0.000001,0.0999)
#
#plt.xlabel('time (s)', fontsize=14)
#plt.ylabel(r'$\delta t (s)$', fontsize=14)
#plt.xticks(fontsize=14)
#plt.yticks(fontsize=14)
#plt.show()
