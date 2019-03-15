import os
import time
import numpy    as np
import chaospy  as cp
import easyvvuq as uq
import matplotlib.pylab as plt
from ascii_cpo import read

start_time = time.time()

# To be read using sys.argv
cpo_dir  = os.path.abspath("../data/SP4FT")
tmp_dir  = "/ptmp/ljala/"
bin_file = "../bin/DRACO/ets_run "

# Run's Input/Output
input_json  = "inputs/ets_in.json"
output_json = os.path.join(tmp_dir, "out_pce.json")

# Initialize Campaign object
ets_campaign = uq.Campaign(state_filename=input_json, workdir=tmp_dir,
                           default_campaign_dir_prefix='ETS_Campaign_')

# Add workflows and copy CPOs files
campaign_dir = ets_campaign.campaign_dir
os.system("mkdir " + campaign_dir +"/workflows")
os.system("cp ../../workflows/ets.x* "+ campaign_dir +"/workflows")
common_dir = campaign_dir +"/common"
os.system("cp " + cpo_dir + "/*.cpo " + common_dir)
print(common_dir)
# Get uncertain parameters and distrubutions
coret_file = common_dir + "/ets_coretransp_in.cpo"
coret      = read(coret_file, "coretransp")
diff_eff = coret.values[0].te_transp.diff_eff
dist1 = cp.Normal(diff_eff[0], 0.2*diff_eff[0])
dist2 = cp.Normal(diff_eff[1], 0.2*diff_eff[1])
dist3 = cp.Normal(diff_eff[2], 0.2*diff_eff[2])
dist4 = cp.Normal(diff_eff[3], 0.2*diff_eff[3])

# Define the parameters dictionary
ets_campaign.vary_param("D1", dist=dist1)
ets_campaign.vary_param("D2", dist=dist2)
ets_campaign.vary_param("D3", dist=dist3)
ets_campaign.vary_param("D4", dist=dist4)

# Create the sampler
ets_sampler  = uq.elements.sampling.PCESampler(ets_campaign)

# Generate runs
ets_campaign.add_runs(ets_sampler)
ets_campaign.populate_runs_dir()

# Execute runs
cmd = bin_file + common_dir + " ets_input.nml"
ets_campaign.apply_for_each_run_dir(uq.actions.ExecuteLocal(cmd))

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

# Elapsed time
end_time = time.time()
print('>>>>> elapsed time = ', end_time - start_time)

# Plots
mean = stats["mean"].to_numpy()
std  = stats["std"].to_numpy()
var  = stats["var"].to_numpy()

s_d1 = sobols["D1"].to_numpy()
s_d2 = sobols["D2"].to_numpy()
s_d3 = sobols["D3"].to_numpy()
s_d4 = sobols["D4"].to_numpy()

corep_file = common_dir + '/ets_coreprof_in.cpo'
corep = read(corep_file, 'coreprof')
rho = corep.rho_tor_norm

fig1 = plt.figure()

ax11 = fig1.add_subplot(111)
ax11.plot(rho, mean,     'g-', alpha=0.75, label='Mean')
ax11.plot(rho, mean-std, 'b-', alpha=0.25)
ax11.plot(rho, mean+std, 'b-', alpha=0.25)
ax11.fill_between(rho, mean-std, mean+std, alpha=0.25, label= r'Mean $\pm$ deviation')
ax11.set_xlabel(r'$\rho_{tor}$ normalized')
ax11.set_ylabel('Temperature Te', color='b')
ax11.tick_params('y', colors='b')
ax11.legend()

ax12 = ax11.twinx()
ax12.plot(rho, var, 'r-', alpha=0.5)
ax12.set_ylabel('Variance', color='r')
ax12.tick_params('y', colors='r')

ax11.grid()
plt.title('Statistical moments (4 params)')

fig2 = plt.figure()
ax2 = fig2.add_subplot(111)
ax2.plot(rho, s_d1,  label=r'$D_1$')
ax2.plot(rho, s_d2, 'r.',label=r'$D_2$')
ax2.plot(rho, s_d3, 'b.',label=r'$D_3$')
ax2.plot(rho, s_d4, label=r'$D_4$')
ax2.legend()
ax2.set_xlabel(r'$\rho_{tor}$ normalized')
ax2.set_ylabel('Sobol indices')
ax2.set_title('First order Sobol indices')

ax2.grid()
ax2.legend()

plt.show()
