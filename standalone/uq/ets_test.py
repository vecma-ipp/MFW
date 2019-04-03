import os
import time
import numpy    as np
import chaospy  as cp
import easyvvuq as uq
import matplotlib.pylab as plt
from ascii_cpo import read

# UQ test of ETS
# inputs:
# outputs:
# related code model:

start_time = time.time()

# CPO files
cpo_dir  = os.path.abspath("../data/AUG_28906_5/BGB_GEM_SPREAD/4FT")

# To store input/ouput files
tmp_dir  = "/ptmp/ljala/"

# The ets_run executable (to run the ets model)
bin_file = "../bin/DRACO/ets_run "

# Input/Output template
input_json  = "inputs/ets_in.json"
output_json = os.path.join(tmp_dir, "out_ets.json")

# Initialize Campaign object
ets_campaign = uq.Campaign(state_filename=input_json, workdir=tmp_dir,
                           default_campaign_dir_prefix='ETS_Campaign_')

campaign_dir = ets_campaign.campaign_dir

# Copy xml files needed in the ets code
os.system("mkdir " + campaign_dir +"/workflows")
os.system("cp ../../workflows/ets.xml "+ campaign_dir +"/workflows")
os.system("cp ../../workflows/ets.xsd "+ campaign_dir +"/workflows")


# Copy CPO files in common directory
common_dir = campaign_dir +"/common"
os.system("cp " + cpo_dir + "/*.cpo " + common_dir)

# Get uncertain parameters and distrubutions
coret_file = common_dir + "/ets_coretransp_in.cpo"
coret      = read(coret_file, "coretransp")
n_params = 4
params = ["D1", "D2", "D3", "D4"]
diff_eff   = coret.values[0].te_transp.diff_eff
list_dist  = [cp.Normal(diff_eff[i], 0.2*diff_eff[i]) for i in range(n_params)]

# Define the parameters dictionary

for i in range(n_params):
    ets_campaign.vary_param(params[i], dist=list_dist[i])

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
output_columns = ['te']

aggregate = uq.elements.collate.AggregateSamples( ets_campaign,
                                            output_filename=output_filename,
                                            output_columns=output_columns,
                                            header=0,
                                            )

aggregate.apply()

# Post-processing analysis: computes the 1st two statistical moments and SA
analysis = uq.elements.analysis.PCEAnalysis(ets_campaign, value_cols=output_columns)

# Analysis results
stats, corr, sobols = analysis.apply()

# Elapsed time
end_time = time.time()
print('>>>>> elapsed time = ', end_time - start_time)

#  Plot satatistical infos and sensitivity analysis
__stats = True
__sobols = False
__corr = False

corep_file = common_dir + '/ets_coreprof_in.cpo'
corep = read(corep_file, 'coreprof')
rho = corep.rho_tor

# Statistical Moments
if __stats:
    mean = stats['te']["mean"].to_numpy()
    var  = stats['te']["var"].to_numpy()

    fig1 = plt.figure(figsize=(12,9))
    ax11 = fig1.add_subplot(111)
    ax11.plot(rho, mean,     'g-', alpha=0.75, label='Mean')
    ax11.plot(rho, mean-std, 'b-', alpha=0.25)
    ax11.plot(rho, mean+std, 'b-', alpha=0.25)
    ax11.fill_between(rho, mean-std, mean+std, alpha=0.25, label= r'Mean $\pm$ deviation')
    ax11.set_xlabel(r'$\rho_{tor} \quad [m]$')
    ax11.set_ylabel('Mean', color='b')
    ax11.tick_params('y', colors='b')
    ax11.legend()

    ax12 = ax11.twinx()
    ax12.plot(rho, var, 'r-', alpha=0.5, label='Variance')
    ax12.set_ylabel('Variance', color='r')
    ax12.tick_params('y', colors='r')
    ax12.legend()
    ax12.grid()
    plt.title('Electron temperature [eV]')

# Sobols indicies
if __sobols:
    s1i  = [sobols[pname].to_numpy() for pname in params]
    fig2 = plt.figure(figsize=(12,9))
    ax21 = fig2.add_subplot(111)
    for i in range(n_params):
        ax21.plot(rho, s1i[i], label=params[i])
    ax21.set_xlabel(r'$\rho_{tor} \quad [m]$')
    ax21.set_ylabel('Sobol indices')
    ax21.legend()
    ax21.grid()
    plt.title('First order Sobol indices')

# Correlation matrix
if __corr:
    fig3 = plt.figure()
    ax3  = fig3.add_subplot(111)
    ax3.imshow(corr, cmap=plt.cm.jet)
    ax3.colorbar()
    ax3.title('Corrolation matrix)')

plt.show()
