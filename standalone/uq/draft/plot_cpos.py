import pylab
from ascii_cpo import read

time = []
te_1 = []
te_2 = []
te_3 = []
te_4 = []
te_5 = []
te_6 = []

print("reading coreprof and coretransp CPOs")
for i in range(50):
    corep = read('cpos/ets_coreprof_'+str(i+1).zfill(4)+'.cpo', 'coreprof')
    time.append(corep.time)
    te = corep.te.value
    te_1.append(te[0])
    te_2.append(te[19])
    te_3.append(te[39])
    te_4.append(te[59])
    te_5.append(te[79])
    te_6.append(te[99])

r = corep.rho_tor_norm

fig = pylab.figure(figsize=(12,9))

ax1 = pylab.subplot(111)
ax1.set_xlabel('time')
ax1.set_ylabel(r'$t_e \quad [eV]$')

pylab.plot(time, te_1, label=r'$\||\rho_{tor}\||=$'+ '{0:.2f}'.format(r[0]))
pylab.plot(time, te_2, label=r'$\||\rho_{tor}\||=$'+ '{0:.2f}'.format(r[19]))
pylab.plot(time, te_3, label=r'$\||\rho_{tor}\||=$'+ '{0:.2f}'.format(r[39]))
pylab.plot(time, te_4, label=r'$\||\rho_{tor}\||=$'+ '{0:.2f}'.format(r[59]))
pylab.plot(time, te_5, label=r'$\||\rho_{tor}\||=$'+ '{0:.2f}'.format(r[79]))
pylab.plot(time, te_6, label=r'$\||\rho_{tor}\||=$'+ '{0:.2f}'.format(r[99]))
pylab.title('Evolution of Te in (ETS + CHEASE + BOHMGB) 50 loops. Transport timestep
            = 0.008s')
fig.tight_layout()

pylab.legend()
pylab.show()
