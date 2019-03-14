import os
import time
import numpy    as np
import chaospy  as cp
import easyvvuq as uq
import matplotlib.pylab as plt
from ascii_cpo import read
#from wrappers  import run_ets

# To compute elapsed time
start_time = time.time()

# Inputs
cpo_dir = "../data/SP2FT"
input_json  = "inputs/ets_in.json"

# For the outputs
tmp_dir = "/ptmp/ljala/"
output_json = os.path.join(tmp_dir, "out_pce.json")

# Get uncertain parameters and distrubutions
coret_file = cpo_dir + "/ets_coretransp_in.cpo"
coret      = read(coret_file, "coretransp")
diff_eff = coret.values[0].te_transp.diff_eff
dist1 = cp.Normal(diff_eff[0], 0.2*diff_eff[0])
dist2 = cp.Normal(diff_eff[1], 0.2*diff_eff[1])

# Initialize Campaign object
ets_campaign = uq.Campaign(state_filename=input_json, workdir=tmp_dir,
                           default_campaign_dir_prefix='ETS_Campaign_')

# Define the parameters dictionary
ets_campaign.vary_param("D1", dist=dist1)
ets_campaign.vary_param("D2", dist=dist2)

# Create the sampler
ets_sampler  = uq.elements.sampling.PCESampler(ets_campaign)

# Generate runs
ets_campaign.add_runs(ets_sampler)
ets_campaign.populate_runs_dir()

# Execute runs
ets_campaign.apply_for_each_run_dir(
    uq.actions.ExecuteLocal("../bin/DRACO/ets_run ets_input.nml"))

# Aggregate the results from all runs.
output_filename = ets_campaign.params_info['out_file']['default']
output_columns = ['Te']

aggregate = uq.elements.collate.AggregateSamples( ets_campaign,
                                            output_filename=output_filename,
                                            output_columns=output_columns,
                                            header=0,
                                            )

aggregate.apply()

# Post-processing analysis: computes the 1st two statistical moments and SA
analysis = uq.elements.analysis.PCEAnalysis(ets_campaign, value_cols=output_columns)

stats, sobols, output_file = analysis.apply()

# Plots
mean = stats["mean"].to_numpy()
std  = stats["std"].to_numpy()
var  = stats["var"].to_numpy()

s_d1 = sobols["D1"].to_numpy()
s_d2 = sobols["D2"].to_numpy()

rho = coret.values[0].rho_tor_norm

fig1 = plt.figure()

ax11 = fig1.add_subplot(111)
ax11.plot(rho, mean,     'g-', alpha=0.75, label='Mean')
ax11.plot(rho, mean-std, 'b-', alpha=0.25)
ax11.plot(rho, mean+std, 'b-', alpha=0.25)
ax11.fill_between(t, mean-std, mean+std, alpha=0.25, label= r'Mean $\pm$ deviation')
ax11.set_xlabel(r'$\rho_{tor}$ normalized')
ax11.set_ylabel('Temperature Te', color='b')
ax11.tick_params('y', colors='b')
ax11.legend()

ax12 = ax11.twinx()
ax12.plot(rho, var, 'r-', alpha=0.5)
ax12.set_ylabel('Variance', color='r')
ax12.tick_params('y', colors='r')

ax11.grid()
plt.title('Statistical moments')

fig2 = plt.figure()
ax2 = fig2.add_subplot(111)
ax2.plot(rho, s_kappa, 'b-', label=r'$D_1$')
ax2.plot(rho, s_t_env, 'g-', label=r'$D_2$')
ax2.legend()
ax2.set_xlabel(r'$\rho_{tor}$ normalized')
ax2.set_ylabel('Sobol indices')
ax2.set_title('First order Sobol indices')

ax2.grid()
ax2.legend()

plt.show()
