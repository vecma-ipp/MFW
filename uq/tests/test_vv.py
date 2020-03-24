import os
import numpy as np
import matplotlib.pylab as plt
from pandas import read_csv
from ascii_cpo import read
from scipy.interpolate import interp1d
from mfw.utils.statistics import Split_Normal, Assymetric_Normal


# Experimental data in csv format containing:
# rho_toroidal,Data,Lower,Upper
exp_filename = os.path.abspath("exp_data/36297_4000_Te.csv")
scale = 1000.

# Read and extract experimental data
df = read_csv(exp_filename)

rho_exp = np.array(df["rho_toroidal"])
mid_exp = np.array(df["Data"])*scale
lo_exp = np.array(df["Lower"])*scale
up_exp = np.array(df["Upper"])*scale

# Get rho_tor_norm from cpo file
cpo_filename = os.path.abspath("../workflows/AUG_28906_6/ets_coreprof_in.cpo")
corep = read(cpo_filename, "coreprof")
rho = corep.rho_tor_norm
ngrid = len(rho) # 100

# Interpolate experimental data
mid = interp1d(rho_exp, mid_exp, kind="cubic")
lo = interp1d(rho_exp, lo_exp, kind="cubic")
up = interp1d(rho_exp, up_exp, kind="cubic")

# Create probability distribution
dist_exp_1 = []
dist_exp_2 = []

for i in range(ngrid):
    m = mid(rho[i])
    l = lo(rho[i])
    u = up(rho[i])
    # using asymetric Gaussian
    dist_exp_1.append(Assymetric_Normal(mu=m, sig1=l, sig2=u))
    # using two-piece Gaussian
    dist_exp_2.append(Split_Normal(mode=m, sig1=l, sig2=u))
