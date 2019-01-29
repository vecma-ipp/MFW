import numpy   as np
import matplotlib.pylab as plt
from ascii_cpo import read
#from wrappers  import update_file

# Plots
corep_file = 'ets_coreprof_in.cpo'
corep      = read(corep_file, "coreprof")
rho = corep.rho_tor_norm
te  = corep.te.value

plt.plot(rho, te, 'b-', label='Initial Te')

plt.legend()
plt.grid()
plt.show()
