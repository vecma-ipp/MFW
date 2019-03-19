import os
import time
import numpy    as np
import chaospy  as cp
import easyvvuq as uq
import matplotlib.pylab as plt
from ascii_cpo import read

start_time = time.time()

# To be read using sys.argv
cpo_dir  = os.path.abspath("../data/AUG_2806_5/BGD_GEM_SPREAD/4FT")
tmp_dir  = "/ptmp/ljala/"
bin_file = "../bin/DRACO/chease_run "

# Run's Input/Output
input_json  = "inputs/eq_in.json"
output_json = os.path.join(tmp_dir, "out_eq.json")

# Initialize Campaign object
eq_campaign = uq.Campaign(state_filename=input_json, workdir=tmp_dir,
                           default_campaign_dir_prefix='EQ_Campaign_')

# Add workflows and copy CPOs files
campaign_dir = eq_campaign.campaign_dir
os.system("mkdir " + campaign_dir +"/workflows")
os.system("cp ../../workflows/chease.x* "+ campaign_dir +"/workflows")
common_dir = campaign_dir +"/common"
os.system("cp " + cpo_dir + "/*.cpo " + common_dir)

# Get uncertain parameters and distrubutions
equil_file = common_dir + "/ets_equilibrium_up.cpo"
equil      = read(equil_file, "equilibrium")
p    = equil.profiles_1d.pressure
j    = equil.profiles_1d.jparallel
dp0 = cp.Normal(p[0], 0.2*p[0])
dp1 = cp.Normal(p[50], 0.2*p[50])
dj0 = cp.Normal(j[0], 0.2*j[0])
dj1 = cp.Normal(j[50], 0.2*j[50])

# Define the parameters dictionary
eq_campaign.vary_param("P0", dist=dp0)
eq_campaign.vary_param("P1", dist=dp1)
eq_campaign.vary_param("J0", dist=dj0)
eq_campaign.vary_param("J1", dist=dj1)

# Create the sampler
eq_sampler  = uq.elements.sampling.PCESampler(eq_campaign)

# Generate runs
eq_campaign.add_runs(eq_sampler)
eq_campaign.populate_runs_dir()

# Execute runs
cmd = bin_file + common_dir + " eq_input.nml"
eq_campaign.apply_for_each_run_dir(uq.actions.ExecuteLocal(cmd))

# Aggregate the results from all runs.
output_filename = eq_campaign.params_info['out_file']['default']
output_columns = ['q']

aggregate = uq.elements.collate.AggregateSamples( eq_campaign,
                                            output_filename=output_filename,
                                            output_columns=output_columns,
                                            header=0,
                                            )

aggregate.apply()

# Post-processing analysis: computes the 1st two statistical moments and SA
analysis = uq.elements.analysis.PCEAnalysis(eq_campaign, value_cols=output_columns)

stats, sobols, corr = analysis.apply()

# Elapsed time
end_time = time.time()
print('>>>>> elapsed time = ', end_time - start_time)

# Plots
mean = stats["mean"].to_numpy()
std  = stats["std"].to_numpy()
var  = stats["var"].to_numpy()

rho = equil.profiles_1d.rho_tor

fig1 = plt.figure()

ax11 = fig1.add_subplot(111)
ax11.plot(rho, mean,     'g-', alpha=0.75, label='Mean')
ax11.plot(rho, mean-std, 'b-', alpha=0.25)
ax11.plot(rho, mean+std, 'b-', alpha=0.25)
ax11.fill_between(rho, mean-std, mean+std, alpha=0.25, label= r'Mean $\pm$ deviation')
ax11.set_xlabel(r'$\rho_{tor}$')
ax11.set_ylabel('q profile', color='b')
ax11.tick_params('y', colors='b')
ax11.legend()

ax12 = ax11.twinx()
ax12.plot(rho, var, 'r-', alpha=0.5)
ax12.set_ylabel('Variance', color='r')
ax12.tick_params('y', colors='r')

ax11.grid()
plt.title('Statistical moments')

#fig2 = plt.figure()
#ax2 = fig2.add_subplot(111)
#ax2.plot(rho, s_d1,  label=r'$D_1$')
#ax2.plot(rho, s_d2, 'r.',label=r'$D_2$')
#ax2.plot(rho, s_d3, 'b.',label=r'$D_3$')
#ax2.plot(rho, s_d4, label=r'$D_4$')
#ax2.legend()
#ax2.set_xlabel(r'$\rho_{tor}$ normalized')
#ax2.set_ylabel('Sobol indices')
#ax2.set_title('First order Sobol indices')
#
#ax2.grid()
#ax2.legend()


#plt.imshow(corr, cmap=plt.cm.jet)
#plt.colorbar()
#plt.clim(-1,1)
#plt.title('Covariance matrix (2FT)')
plt.show()
