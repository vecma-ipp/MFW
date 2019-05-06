import os
import time
import numpy as np
import chaospy as cp
import easyvvuq as uq
import matplotlib.pylab as plt
from ascii_cpo import read
from utils import plots

'''
UQ test of ETS + CHEASE + BOHMGB (UQP1: non intrusive case)
'''

start_time = time.time()

# CPO files
cpo_dir = os.path.abspath("../data/AUG_28906_5/BGB_GEM_SPREAD/4FT/")

# Uncertain parameters: Initial conditions
uncert_params = ["Te_0", "Te_1"]

# To store input/ouput files and Campaign directories
tmp_dir = "/ptmp/ljala/"

# To run F90 code
loop_exec = "../bin/DRACO/loop_run "

# Input/Output template
input_json = "inputs/loop_in.json"
output_json = os.path.join(tmp_dir, "out_loop.json")

# Initialize Campaign object
loop_campaign = uq.Campaign(state_filename=input_json, workdir=tmp_dir,
                           default_campaign_dir_prefix='LOOP_Campaign_')

campaign_dir = loop_campaign.campaign_dir

# Copy XML files needed in the ETS, CHEASE and BOHMGB codes
os.system("mkdir " + campaign_dir +"/workflows")
os.system("cp ../../workflows/ets.xml "+ campaign_dir +"/workflows")
os.system("cp ../../workflows/ets.xsd "+ campaign_dir +"/workflows")
os.system("cp ../../workflows/chease.xml "+ campaign_dir +"/workflows")
os.system("cp ../../workflows/chease.xsd "+ campaign_dir +"/workflows")
os.system("cp ../../workflows/bohmgb.xml "+ campaign_dir +"/workflows")
os.system("cp ../../workflows/bohmgb.xsd "+ campaign_dir +"/workflows")

# Copy CPO files in common directory
common_dir = campaign_dir +"/common/"
os.system("cp " + cpo_dir + "/*.cpo " + common_dir)

# Get uncertain parameters distrubutions
corep_file = common_dir + "ets_coreprof_in.cpo"
corep = read(corep_file, "coreprof")
te_0 = corep.te.value[0]
te_1 = corep.te.value[-1]
dist_0 = cp.Normal(te_0, 0.2*te_0)
dist_1 = cp.Normal(te_1, 0.2*te_1)
print('>>>>> T0 and T1 = ',te_0, te_1)

# Define the parameters dictionary
loop_campaign.vary_param(uncert_params[0], dist=dist_0)
loop_campaign.vary_param(uncert_params[1], dist=dist_1)

# Create the sampler
loop_sampler = uq.elements.sampling.PCESampler(loop_campaign)

# Generate runs
loop_campaign.add_runs(loop_sampler)
loop_campaign.populate_runs_dir()

# Execute runs
cmd = loop_exec + common_dir + " loop_input.nml"

t_loop = time.time()
loop_campaign.apply_for_each_run_dir(uq.actions.ExecuteLocal(cmd))
t_loop = time.time() - t_loop

# Aggregate the results from all runs.
output_filename = loop_campaign.params_info['out_file']['default']
output_columns = ['te']

aggregate = uq.elements.collate.AggregateSamples( loop_campaign,
                                            output_filename=output_filename,
                                            output_columns=output_columns,
                                            header=0,
                                            )

aggregate.apply()

# Analysis
analysis = uq.elements.analysis.PCEAnalysis(loop_campaign, value_cols=output_columns)
analysis.apply()

# Results
stat = analysis.statistical_moments('te')
pctl = analysis.percentiles('te')
sobols = analysis.sobol_indices('te', 'first_order')

# Elapsed time
end_time = time.time()
print('======= Elapsed times')
print('- LOOP  : ', t_loop/60.)
print('- TOTAL : ', (end_time - start_time)/60.)

#  Graphics for descriptive satatistics
rho_nom = corep.rho_tor_norm

plots.plot_stats(rho_nom, stat, pctl,
                 xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'$T_e \rangle$',
                 ftitle='UQP1. ETS-CHEASE-BOHMGB output: Te profile', fname='te_loop.png')

plots.plot_sobols(rho, sobols, uncert_params)
