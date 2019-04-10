import os
import time
import numpy as np
import chaospy as cp
import easyvvuq as uq
import matplotlib.pylab as plt
from ascii_cpo import read
from utils import plots

# UQ test of ETS + CHESAE
# inputs:
# outputs:
# related code model:

start_time = time.time()

# CPO files
cpo_dir = os.path.abspath("../data/AUG_28906_5/BGB_GEM_SPREAD/4FT")

# To store input/ouput files
tmp_dir = "/ptmp/ljala/"

# To run fus and CHEASE
ets_exec = "../bin/DRACO/ets_run "
chease_exec = "../bin/DRACO/chease_run "

# Input/Output template
input_json = "inputs/fus_in.json"
output_json = os.path.join(tmp_dir, "out_fus.json")

# Initialize Campaign object
fus_campaign = uq.Campaign(state_filename=input_json, workdir=tmp_dir,
                           default_campaign_dir_prefix='FUS_Campaign_')

campaign_dir = fus_campaign.campaign_dir

# Copy xml files needed in the fus code
os.system("mkdir " + campaign_dir +"/workflows")
os.system("cp ../../workflows/ets.xml "+ campaign_dir +"/workflows")
os.system("cp ../../workflows/ets.xsd "+ campaign_dir +"/workflows")
os.system("cp ../../workflows/chease.xml "+ campaign_dir +"/workflows")
os.system("cp ../../workflows/chease.xsd "+ campaign_dir +"/workflows")

# Copy CPO files in common directory
common_dir = campaign_dir +"/common"
os.system("cp " + cpo_dir + "/*.cpo " + common_dir)

# Get uncertain parameters and distrubutions
coret_file = common_dir + "/ets_coretransp_in.cpo"
coret = read(coret_file, "coretransp")
n_params = 4
params = ["D1", "D2", "D3", "D4"]
diff_eff = coret.values[0].te_transp.diff_eff
list_dist = [cp.Normal(diff_eff[i], 0.3*diff_eff[i]) for i in range(n_params)]

# Define the parameters dictionary
for i in range(n_params):
    fus_campaign.vary_param(params[i], dist=list_dist[i])

# Create the sampler
fus_sampler = uq.elements.sampling.PCESampler(fus_campaign)
# Generate runs

fus_campaign.add_runs(fus_sampler)
fus_campaign.populate_runs_dir()

# Execute runs
cmd1 = ets_exec + common_dir + " fus_input.nml"

t_ets = time.time()
fus_campaign.apply_for_each_run_dir(uq.actions.ExecuteLocal(cmd1))
t_ets = time.time() - t_ets

cmd2 = chease_exec
t_chease = time.time()
fus_campaign.apply_for_each_run_dir(uq.actions.ExecuteLocal(cmd2))
t_chease = time.time() - t_chease

# Aggregate the results from all runs.
output_filename = fus_campaign.params_info['out_file']['default']
output_columns = ['gm3']

aggregate = uq.elements.collate.AggregateSamples( fus_campaign,
                                            output_filename=output_filename,
                                            output_columns=output_columns,
                                            header=0,
                                            )

aggregate.apply()

# Analysis
analysis = uq.elements.analysis.PCEAnalysis(fus_campaign, value_cols=output_columns)
out_dist = analysis.apply()

# Results
stat_gm3 = analysis.statistical_moments('gm3')
pctl_gm3 = analysis.percentiles('gm3')

#stat_gm7 = analysis.statistical_moments('gm7')
#pctl_gm7 = analysis.percentiles('gm7')

# Elapsed time
end_time = time.time()
print('=========== elapsed times =============')
print('- ets   : ', t_ets/60.)
print('- chease: ', t_chease/60.)
print('- total : ', (end_time - start_time)/60.)

equil_file = common_dir + '/ets_equilibrium_in.cpo'
equil = read(equil_file, 'equilibrium')
rho = equil.profiles_1d.rho_tor

#  Graphics for descriptive satatistics
plots.plot_stats(rho, stat_gm3, pctl_gm3,
                 title='GM3 profile', xlabel='rho_tor [m]',
                 ylabel='gm3', savefig=True)
