import os
import time
import numpy as np
import chaospy as cp
import easyvvuq as uq
import matplotlib.pylab as plt
from ascii_cpo import read
from utils import plots

'''
UQ test of ETS+CHEASE (UQP1: non intrusive case)
'''

start_time = time.time()

# CPO files
cpo_dir = os.path.abspath("../data/AUG_28906_5/BGB_GEM_SPREAD/4FT/")

# Uncertain parameters
uncert_params = ["D1", "D2", "D3", "D4"]

# To store input/ouput files and Campaign directories
tmp_dir = "/ptmp/ljala/"

# To run ETS and CHEASE
ets_exec = "../bin/DRACO/ets_run "
chease_exec = "../bin/DRACO/chease_run "

# Input/Output template
input_json = "inputs/fus_in.json"
output_json = os.path.join(tmp_dir, "out_fus.json")

# Initialize Campaign object
fus_campaign = uq.Campaign(state_filename=input_json, workdir=tmp_dir,
                           default_campaign_dir_prefix='FUS_Campaign_')

campaign_dir = fus_campaign.campaign_dir

# Copy XML files needed in the ETS and CHEASE codes
os.system("mkdir " + campaign_dir +"/workflows")
os.system("cp ../../workflows/ets.xml "+ campaign_dir +"/workflows")
os.system("cp ../../workflows/ets.xsd "+ campaign_dir +"/workflows")
os.system("cp ../../workflows/chease.xml "+ campaign_dir +"/workflows")
os.system("cp ../../workflows/chease.xsd "+ campaign_dir +"/workflows")

# Copy CPO files in common directory
common_dir = campaign_dir +"/common/"
os.system("cp " + cpo_dir + "/*.cpo " + common_dir)

# Get uncertain parameters distrubutions
coret_file = common_dir + "ets_coretransp_in.cpo"
coret = read(coret_file, "coretransp")
n_params = len(uncert_params)
diff_eff = coret.values[0].te_transp.diff_eff
list_dist = [cp.Normal(diff_eff[i], 0.3*diff_eff[i]) for i in range(n_params)]

# Define the parameters dictionary
for i in range(n_params):
    fus_campaign.vary_param(uncert_params[i], dist=list_dist[i])

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
output_columns = ['gm4', 'gm5', 'gm8']

aggregate = uq.elements.collate.AggregateSamples( fus_campaign,
                                            output_filename=output_filename,
                                            output_columns=output_columns,
                                            header=0,
                                            )

aggregate.apply()

# Analysis
analysis = uq.elements.analysis.PCEAnalysis(fus_campaign, value_cols=output_columns)
oout_distut_dist = analysis.apply()

# Results
stat_gm4 = analysis.statistical_moments('gm4')
pctl_gm4 = analysis.percentiles('gm4')

stat_gm5 = analysis.statistical_moments('gm5')
pctl_gm5 = analysis.percentiles('gm5')

stat_gm8 = analysis.statistical_moments('gm8')
pctl_gm8 = analysis.percentiles('gm8')

# Elapsed time
end_time = time.time()
print('======= Elapsed times')
print('- ETS   : ', t_ets/60.)
print('- CHEASE: ', t_chease/60.)
print('- TOTAL : ', (end_time - start_time)/60.)

#  Graphics for descriptive satatistics
equil_file = common_dir + 'ets_equilibrium_in.cpo'
equil = read(equil_file, 'equilibrium')
rho = equil.profiles_1d.rho_tor

plots.plot_stats(rho, stat_gm4,
                 xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'$\langle \frac{1}{B^2} \rangle$',
                 ftitle='UQP1 - Chease output: GM4 profile', fname='gm4.png')

plots.plot_stats(rho, stat_gm5,
                 xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'$\langle B^{2} \rangle$',
                 ftitle='UQP1 - Chease output: GM5 profile', fname='gm5.png')

plots.plot_stats(rho, stat_gm8,
                 xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'$\langle R \rangle$',
                 ftitle='UQP1 - Chease output: GM8 profile', fname='gm8.png')

