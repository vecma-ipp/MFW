import pylab
import ascii_cpo

corep = ascii_cpo.read("ets_coreprof_in.cpo","coreprof")


pylab.figure(1)
pylab.suptitle("Initial profiles for JET#92436/270")

pylab.subplot(321)
pylab.plot(corep.rho_tor_norm,corep.profiles1d.q.value)
#pylab.xlabel("rho_tor_norm")
pylab.ylabel("q")

pylab.subplot(322)
pylab.plot(corep.rho_tor_norm,corep.profiles1d.pr_th.value)
#pylab.xlabel("rho_tor_norm")
pylab.ylabel("pr_th")

pylab.subplot(323)
pylab.plot(corep.rho_tor_norm,corep.te.value)
#pylab.xlabel("rho_tor_norm")
pylab.ylabel("te")

pylab.subplot(324)
pylab.plot(corep.rho_tor_norm,corep.ti.value)
#pylab.xlabel("rho_tor_norm")
pylab.ylabel("ti")

pylab.subplot(325)
pylab.plot(corep.rho_tor_norm,corep.ne.value)
pylab.xlabel("rho_tor_norm")
pylab.ylabel("ne")

pylab.subplot(326)
pylab.plot(corep.rho_tor_norm,corep.ni.value)
pylab.xlabel("rho_tor_norm")
pylab.ylabel("ni")

pylab.tight_layout()

pylab.show(block=True)



