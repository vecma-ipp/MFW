import os
import time
import numpy as np
import chaospy as cp
import easyvvuq as uq
import matplotlib.pylab as plt
from ascii_cpo import read
from tools import plots


'''
UQ test of ETS + CHEASE + BOHMGB using UQP1 (non intrusive case).
Uncertainties in initial conditions: Te and Ti boundaries.
'''

start_time = time.time()

# OS env
SYS = os.environ['SYS']

# CPO files
cpo_dir = os.path.abspath("../data/TESTS/")

# Uncertain parameters: Initial conditions
uncert_params = ["Te_boundary", "Ti_boundary"]

# To store input/ouput files and Campaign directories
tmp_dir = "/ptmp/ljala/"

# To run F90 code
bc_exec = "../bin/"+SYS+"/bc_run "

# Input/Output template
input_json = "inputs/bc_in.json"
output_json = os.path.join(tmp_dir, "out_bc.json")

# Initialize Campaign object
bc_campaign = uq.Campaign(
    name = 'BC_P3_',
    state_filename=input_json,
    workdir=tmp_dir,
    default_campaign_dir_prefix='BC_P3_'
)

campaign_dir = bc_campaign.campaign_dir

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
Te_boundary = corep.te.boundary.value[0]
Ti_boundary = corep.ti.boundary.value[0][0]
dist_1 = cp.Normal(Te_boundary, 0.2*Te_boundary)
dist_2 = cp.Normal(Ti_boundary, 0.2*Ti_boundary)

# Define the parameters dictionary
bc_campaign.vary_param(uncert_params[0], dist=dist_1)
bc_campaign.vary_param(uncert_params[1], dist=dist_2)

# Create the sampler
bc_sampler = uq.elements.sampling.PCESampler(bc_campaign, polynomial_order=3)

# Generate runs
bc_campaign.add_runs(bc_sampler)
bc_campaign.populate_runs_dir()

# Execute runs
cmd = bc_exec + common_dir + " bc_input.nml"

t_bc = time.time()
bc_campaign.apply_for_each_run_dir(uq.actions.ExecuteLocal(cmd))
t_bc = time.time() - t_bc

# Aggregate the results from all runs.
output_filename = bc_campaign.params_info['out_file']['default']
output_columns = ['ti', 'te']

aggregate = uq.elements.collate.AggregateSamples( bc_campaign,
                                            output_filename=output_filename,
                                            output_columns=output_columns,
                                            header=0,
                                            )

aggregate.apply()

# Analysis
analysis = uq.elements.analysis.PCEAnalysis(bc_campaign, value_cols=output_columns)
analysis.apply()

# Results
stats_te = analysis.statistical_moments('te')
stats_ti = analysis.statistical_moments('ti')
sobols_te = analysis.sobol_indices('te', 'first_order')
sobols_ti = analysis.sobol_indices('ti', 'first_order')

# Elapsed time
end_time = time.time()
print('======= Elapsed times')
print('- LOOP  : ', t_bc/60.)
print('- TOTAL : ', (end_time - start_time)/60.)

#  Graphics for descriptive satatistics
rho = corep.rho_tor

plots.plot_stats(rho, stats_te,
                 xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'$T_e [eV]$',
                 ftitle='Te profile',
                 fname='plots/te_stats_bc_3.png')

plots.plot_stats(rho, stats_ti,
                 xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'$T_i [eV]$',
                 ftitle='Ti profile',
                 fname='plots/ti_stats_bc_3.png')

plots.plot_sobols(rho, sobols_te, uncert_params,
                  ftitle=' First-Order Sobol indices - QoI: Te.',
                  fname='plots/te_sobols_bc_3.png')

plots.plot_sobols(rho, sobols_ti, uncert_params,
                  ftitle=' First-Order Sobol indices - QoI: Ti.',
                  fname='plots/ti_sobols_bc_3.png')

