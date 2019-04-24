import os
import time
import numpy as np
import chaospy as cp
import easyvvuq as uq
import matplotlib.pylab as plt
from ascii_cpo import read
from utils import plots

'''
UQ test of ETS
'''


start_time = time.time()

# CPO files
cpo_dir = os.path.abspath("../data/AUG_28906_5/BGB_GEM_SPREAD/4FT/")

# Uncertain parameters
uncert_params = ["D1", "D2", "D3", "D4"]

# To store input/ouput files
tmp_dir = "/ptmp/ljala/"

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
common_dir = campaign_dir +"/common/"
os.system("cp " + cpo_dir + "/*.cpo " + common_dir)

# Get uncertain parameters distrubutions
coret_file = common_dir + "ets_coretransp_in.cpo"
coret      = read(coret_file, "coretransp")
n_params = len(uncert_params)
diff_eff   = coret.values[0].te_transp.diff_eff
list_dist  = [cp.Normal(diff_eff[i], 0.2*diff_eff[i]) for i in range(n_params)]

# Define the parameters dictionary
for i in range(n_params):
    ets_campaign.vary_param(uncert_params[i], dist=list_dist[i])

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
output_columns = ['p']

aggregate = uq.elements.collate.AggregateSamples( ets_campaign,
                                            output_filename=output_filename,
                                            output_columns=output_columns,
                                            header=0,
                                            )

aggregate.apply()

# Analysis
analysis = uq.elements.analysis.PCEAnalysis(ets_campaign, value_cols=output_columns)
out_dist = analysis.apply()

# Results
stat = analysis.statistical_moments('p')
pctl = analysis.percentiles('p')
sobols = analysis.sobol_indices('p', 'first_order')

# Elapsed time
end_time = time.time()
print('>>>>> elapsed time = ', (end_time - start_time)/60.)

#corep_file = common_dir + '/ets_coreprof_in.cpo'
#corep = read(corep_file, 'coreprof')
#rho = corep.rho_tor

equil_file = common_dir + 'ets_equilibrium_in.cpo'
equil = read(equil_file, 'equilibrium')
rho = equil.profiles_1d.rho_tor

#  Graphics for descriptive satatistics
plots.plot_stats(rho, stat, pctl,
                 xlabel=r'$\rho_{tor} ~ [m]$', ylabel='pressure',
                 ftitle='UQP1 - ETS output: pressure profile', fname='pressure.png')

#plots.plot_sobols(rho, sobols, uncert_params)
