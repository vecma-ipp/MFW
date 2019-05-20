import os
import time
import numpy as np
import chaospy as cp
import easyvvuq as uq
import matplotlib.pylab as plt
from ascii_cpo import read
from tools import plots

'''
UQ test of ETS + CHEASE + BOHMGB (UQP1: non intrusive case)
Unvertainties in SOURCES (current)
'''

start_time = time.time()

# CPO files
cpo_dir = os.path.abspath("../data/AUG_28906_5/BGB_GEM_SPREAD/4FT/")

# Uncertain parameters: Initial conditions
uncert_params = ["E_AMP", "E_MEAN", "E_STD"]

# To store input/ouput files and Campaign directories
tmp_dir = "/ptmp/ljala/"

# To run F90 code
src_exec = "../bin/DRACO/src_run "

# Input/Output template
input_json = "inputs/src_in.json"
output_json = os.path.join(tmp_dir, "out_src.json")

# Initialize Campaign object
src_campaign = uq.Campaign(
    name = 'src_campaign',
    state_filename=input_json,
    workdir=tmp_dir,
    default_campaign_dir_prefix='src_campaign_'
)

campaign_dir = src_campaign.campaign_dir

# Copy XML files needed in the ETS, CHEASE and BOHMGB codes
os.system("mkdir " + campaign_dir +"/workflows")
os.system("cp ../../workflows/ets.xml "+ campaign_dir +"/workflows")
os.system("cp ../../workflows/ets.xsd "+ campaign_dir +"/workflows")
os.system("cp ../../workflows/chease.xml "+ campaign_dir +"/workflows")
os.system("cp ../../workflows/chease.xsd "+ campaign_dir +"/workflows")
os.system("cp ../../workflows/bohmgb.xml "+ campaign_dir +"/workflows")
os.system("cp ../../workflows/bohmgb.xsd "+ campaign_dir +"/workflows")
os.system("cp ../../workflows/source_dummy.xml "+ campaign_dir +"/workflows")
os.system("cp ../../workflows/source_dummy.xsd "+ campaign_dir +"/workflows")

# Copy CPO files in common directory
common_dir = campaign_dir +"/common/"
os.system("cp " + cpo_dir + "/*.cpo " + common_dir)

# Get uncertain parameters distrubutions
# Read JNITOT, RCURR, FWCURR from source_dummy.xml file
E_AMP = 1.E6
E_MEAN = 0.5
E_STD = 0.2
dist_1 = cp.Uniform(0.9*E_AMP, 1.1*E_AMP)
dist_2 = cp.Uniform(0.9*E_MEAN, 1.1*E_MEAN)
dist_3 = cp.Uniform(0.9*E_STD, 1.1*E_STD)

# Define the parameters dictionary
src_campaign.vary_param(uncert_params[0], dist=dist_1)
src_campaign.vary_param(uncert_params[1], dist=dist_2)
src_campaign.vary_param(uncert_params[2], dist=dist_3)

# Create the sampler
src_sampler = uq.elements.sampling.PCESampler(src_campaign)

# Generate runs
src_campaign.add_runs(src_sampler)
src_campaign.populate_runs_dir()

# Execute runs
cmd = src_exec + common_dir + " src_input.nml"

t_src = time.time()
src_campaign.apply_for_each_run_dir(uq.actions.ExecuteLocal(cmd))
t_src = time.time() - t_src

# Aggregate the results from all runs.
output_filename = src_campaign.params_info['out_file']['default']
output_columns = ['te', 'ti']

aggregate = uq.elements.collate.AggregateSamples( src_campaign,
                                            output_filename=output_filename,
                                            output_columns=output_columns,
                                            header=0,
                                            )

aggregate.apply()

# Analysis
analysis = uq.elements.analysis.PCEAnalysis(src_campaign, value_cols=output_columns)
analysis.apply()

# Results
stat_te = analysis.statistical_moments('te')
sobol_te = analysis.sobol_indices('te', 'first_order')

stat_ti = analysis.statistical_moments('ti')
sobol_ti = analysis.sobol_indices('ti', 'first_order')

# Elapsed time
end_time = time.time()
print('======= Elapsed times')
print('- RUN_SRC: ', t_src/60.)
print('- TOT_SRC: ', (end_time - start_time)/60.)

#  Graphics for descriptive satatistics
corep_file = common_dir + '/ets_coreprof_in.cpo'
corep = read(corep_file, 'coreprof')
rho = corep.rho_tor

plots.plot_stats(rho, stat_te,
                 xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'$T_e$',
                 ftitle='UQ: Te profile',
                 fname='te_prof.png')

plots.plot_sobols3(rho, sobol_te, uncert_params,
                  ftitle='Te UQ: First-Order Sobol indices',
                  fname='te_sobol.png')

plots.plot_stats(rho, stat_j,
                 xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'$T_i$',
                 ftitle='UQ: Ti profile',
                 fname='ti_prof.png')

plots.plot_sobols3(rho, sobol_j, uncert_params,
                  ftitle='Ti UQ: First-Order Sobol indices',
                  fname='ti_sobol.png')
