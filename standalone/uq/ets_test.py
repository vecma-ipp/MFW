import os
import time
import numpy as np
import chaospy as cp
import easyvvuq as uq
import matplotlib.pylab as plt
from ascii_cpo import read
from tools import plots, spl


'''
UQ test of ETS. Uncertainties in 4 flux tubes.
Parameters: D1, D2, D3, D4.
QoI: Control points of the Spline approximation of Te
'''

start_time = time.time()

# Os env
SYS = os.environ['SYS']

# CPO files
cpo_dir = os.path.abspath("../data/TESTS/")

# Uncertain parameters
uncert_params = ["D1", "D2", "D3", "D4"]

# To store input/ouput files
tmp_dir = "/ptmp/ljala/"

# The ets_run executable (to run the ets model)
bin_file = "../bin/"+SYS+"/ets_run "

# Input/Output template
input_json  = "inputs/ets_in.json"
output_json = os.path.join(tmp_dir, "out_ets.json")

# Initialize Campaign object
ets_campaign = uq.Campaign(
    name='ETS_Campaign',
    state_filename=input_json,
    workdir=tmp_dir,
    default_campaign_dir_prefix='ETS_Campaign_'
)

campaign_dir = ets_campaign.campaign_dir

# Copy xml files needed in the ETS code
os.system("mkdir " + campaign_dir +"/workflows")
os.system("cp ../../workflows/ets.xml "+ campaign_dir +"/workflows")
os.system("cp ../../workflows/ets.xsd "+ campaign_dir +"/workflows")

# Copy CPO files in common directory
common_dir = campaign_dir +"/common/"
os.system("cp " + cpo_dir + "/*.cpo " + common_dir)

# Get uncertain parameters distrubutions
coret_file = common_dir + "ets_coretransp_in.cpo"
coret = read(coret_file, "coretransp")
diff_eff = coret.values[0].te_transp.diff_eff
list_dist = [cp.Normal(diff_eff[i], 0.2*diff_eff[i]) for i in range(4)]

# Define the parameters dictionary
for i in range(4):
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
output_columns = ['c']

aggregate = uq.elements.collate.AggregateSamples(
    ets_campaign,
    output_filename=output_filename,
    output_columns=output_columns,
    header=0,
)

aggregate.apply()

# Analysis
analysis = uq.elements.analysis.PCEAnalysis(
    ets_campaign, value_cols=output_columns)

dist, cov = analysis.apply()

# Results
print(">>> Covariance Matrix:")
print(cov["c"])
stat = analysis.statistical_moments('c')

# Elapsed time
end_time = time.time()

# For plots
corep_file = common_dir + '/ets_coreprof_in.cpo'
corep = read(corep_file, 'coreprof')
rho = corep.rho_tor

n=6
p=3
knot, crho = spl.spl_fit(rho, n, p)

#  Graphics for descriptive satatistics
plots.plot_stats(crho, stat,
                 xlabel=r'$\rho_{tor} app ~ [m]$', ylabel=r'$CP$',
                 ftitle='Approximation of Te profile',
                 fname='figs/cp_te_prof.png')

plots.plot_dist(dist["c"], stat)
