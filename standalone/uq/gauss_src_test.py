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
Unvertainties in SOURCES (electrons)
'''

start_time = time.time()

# OS env
SYS = os.environ['SYS']

# CPO files
CPO_DIR = os.path.abspath("../data/TESTS/")

# To store input/ouput files and Campaign directories
TMP_DIR = "/ptmp/ljala/"

# Uncertain parameters: Gaussian Sources
# For electrons: S1=WTOT_el (amplitude), S2=RHEAT_el (mean), S3=FWHEAT_el (std)
uncert_params =["S1", "S2", "S3"]

# To run F90 code
src_exec = "../bin/"+SYS+"/gauss_src_run "

# Input/Output template
input_json = "inputs/src_in.json"
output_json = os.path.join(TMP_DIR, "out_src.json")

# Initialize Campaign object
src_campaign = uq.Campaign(
    name = 'src_campaign',
    state_filename=input_json,
    workdir=TMP_DIR,
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
os.system("cp ../../workflows/source_dummy_new.xml "+ campaign_dir +"/workflows/source_dummy.xml")
os.system("cp ../../workflows/source_dummy.xsd "+ campaign_dir +"/workflows")

# Copy CPO files in common directory
common_dir = campaign_dir +"/common/"
os.system("cp " + CPO_DIR + "/*.cpo " + common_dir)

# Get uncertain parameters distrubutions
# Read WTOT_el, RHEAT_el, FWCURR_el from source_dummy.xml file
S1 = 1.5E6
S2 = 0.0
S3 = 0.2
dist_1 = cp.Uniform(0.9*S1, 1.1*S1)
dist_2 = cp.Uniform(0.0, 0.2)
dist_3 = cp.Uniform(0.9*S3, 1.1*S3)

# Define the parameters dictionary
src_campaign.vary_param(uncert_params[0], dist=dist_1)
src_campaign.vary_param(uncert_params[1], dist=dist_2)
src_campaign.vary_param(uncert_params[2], dist=dist_3)

# Create the sampler
src_sampler = uq.elements.sampling.PCESampler(src_campaign, polynomial_order=3)

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
dist_out, cov_out = analysis.apply()

# Results
stats_te = analysis.statistical_moments('te')
sobols_te = analysis.sobol_indices('te', 'first_order')

stats_ti = analysis.statistical_moments('ti')
sobols_ti = analysis.sobol_indices('ti', 'first_order')

# Elapsed time
end_time = time.time()
print('======= Elapsed times')
print('- RUN_SRC: ', t_src/60.)
print('- TOT_SRC: ', (end_time - start_time)/60.)

#  Graphics for descriptive satatistics
corep_file = common_dir + '/ets_coreprof_in.cpo'
corep = read(corep_file, 'coreprof')
rho = corep.rho_tor

plots.plot_stats(rho, stats_te,
                 xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'$T_e ~ [eV]$',
                 ftitle='UQ: Te profile',
                 fname='plots/te_stats_src.png')

plots.plot_stats(rho, stats_ti,
                 xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'$T_i ~ [eV]$',
                 ftitle='UQ: Ti profile',
                 fname='plots/ti_stats_src.png')

plots.plot_sobols_3(rho, sobols_te, uncert_params,
              ftitle='First-Order Sobol indices - QoI: Te',
              fname='plots/te_sobol_src.png')

plots.plot_sobols_3(rho, sobols_ti, uncert_params,
              ftitle='First-Order Sobol indices - QoI: Ti',
              fname='plots/ti_sobol_src.png')

