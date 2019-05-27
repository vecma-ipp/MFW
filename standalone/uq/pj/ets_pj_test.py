import os
import time
import numpy as np
import chaospy as cp
import easyvvuq as uq

from ascii_cpo import read
from easyvvuq.execution.qcgpj.pj_utils.pj_configurator import PJConfigurator
from qcg.appscheduler.api.manager import Manager
from qcg.appscheduler.api.job import Jobs
from qcg.appscheduler.api.manager import LocalManager


'''
UQ test of ETS using QCG Pilot Job.
Uncertainties in 4 flux tubes. Parameters: D1, D2, D3, D4.
Quantity of Interest: electron temperature (Te).
'''

cwd = os.getcwd()
tmp_dir = "/ptmp/ljala/"
cpo_dir = os.path.abspath("../../data/TESTS/")

# Uncertain parameters
uncert_params = ["D1", "D2", "D3", "D4"]

print("Running in directory: " + cwd)
print("Temporary directory: " + tmpdir)

# Os env
SYS = os.environ['SYS']

# set location of log file
client_conf = {'log_file': os.path.join(tmpdir, "api.log")}



# The ets_run executable (to run the ets model)
bin_file = "../../bin/"+SYS+"/ets_pj_run "

# Input/Output template
input_json  = "inputs/ets_pj_in.json"
output_json = os.path.join(tmp_dir, "out_ets_pj.json")

# Initialize Campaign object
ets_campaign = uq.Campaign(
    name='ETS_PJ_Campaign',
    state_filename=input_json,
    workdir=tmp_dir,
    default_campaign_dir_prefix='ETS_PJ_Campaign_'
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
cmd = bin_file + common_dir + " ets_pj_input.nml"
ets_campaign.apply_for_each_run_dir(uq.actions.ExecuteLocal(cmd))

# Aggregate the results from all runs.
output_filename = ets_campaign.params_info['out_file']['default']
output_columns = ['te']

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
stats  = analysis.statistical_moments('te')
sobols = analysis.sobol_indices('te', 'first_order')

# Elapsed time
end_time = time.time()

# For plots
corep_file = common_dir + '/ets_coreprof_in.cpo'
corep = read(corep_file, 'coreprof')
rho = corep.rho_tor

#  Graphics for descriptive satatistics
plots.plot_stats(rho, stats,
                 xlabel=r'$\rho_{tor} app ~ [m]$', ylabel=r'$T_e [eV]$',
                 ftitle='Te profile',
                 fname='../plots/te_stats_pj.png')

plots.plot_sobols(rho, sobols, uncert_params,
                  ftitle=' First-Order Sobol indices - QoI: Te.',
                  fname='../plots/ti_sobols_pj.png')
