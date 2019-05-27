import os
import time
import numpy as np
import chaospy as cp
import easyvvuq as uq

from ascii_cpo import read
#from plots import plot_stats, plot_sobols

from easyvvuq.execution.qcgpj.pj_utils.pj_configurator import PJConfigurator
from qcg.appscheduler.api.manager import Manager
from qcg.appscheduler.api.job import Jobs
from qcg.appscheduler.api.manager import LocalManager


'''
UQ test of ETS code using QCG Pilot Job.
Uncertainties in 4 flux tubes. Parameters: D1, D2, D3, D4.
Quantity of Interest: electron temperature (Te).
'''

# Environment infos
#===================

# Working directories 
cwd = os.getcwd()

# TODO to be adapted to other machines like sys in config
tmp_dir = os.environ['CINECA_SCRATCH']

# Machine name (cf. MFW/config)
SYS = os.environ['SYS']

# Set location of log file
client_conf = {'log_file': os.path.join(tmp_dir, "api.log")}

# Create QCG Pilot Job Manager
m = LocalManager([], client_conf)

print("available resources:\n%s\n" % str(m.resources())) 


# Application infos
#===================

# CPO files location
cpo_dir = os.path.abspath("../../data/TESTS/")

# The executable application
app = "../../bin/"+SYS+"/ets_pj_run "

# Uncertain parameters (TODO check name from cpo files)
uncert_params = ["D1", "D2", "D3", "D4"]

coret_file = cpo_dir + "/ets_coretransp_in.cpo"
coret = read(coret_file, "coretransp")
Ds = coret.values[0].te_transp.diff_eff

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

# Uncertain parameters distrubutions
list_dist = [cp.Normal(Ds[i], 0.2*Ds[i]) for i in range(4)]

# Define the parameters dictionary
for i in range(4):
    ets_campaign.vary_param(uncert_params[i], dist=list_dist[i])

# Create the sampler
ets_sampler  = uq.elements.sampling.PCESampler(ets_campaign)

# Generate runs
ets_campaign.add_runs(ets_sampler)

# Create PJ configurator
pjc = PJConfigurator(ets_campaign)
pjc.init_runs_dir()

# Save PJ configuration
pjc.save()

# Execute encode -> execute for each run using QCG-PJ
for key, data in ets_campaign.runs.items():

    encode_job = {
        "name": 'encode_' + key,
        "execution": {
            "exec": "./pj_scripts/easyvvuq_encode",
            "args": [ets_campaign.campaign_dir,
                     key],
            "wd": cwd,
            "stdout": ets_campaign.campaign_dir + '/encode_' + key + '.stdout',
            "stderr": ets_campaign.campaign_dir + '/encode_' + key + '.stderr'
        },
        "resources": {
            "numCores": {
                "exact": 1
            }
        }
    }

    execute_job = {
        "name": 'execute_' + key,
        "execution": {
            "exec": "./pj_scripts/easyvvuq_execute",
            "args": [ets_campaign.campaign_dir,
                     key,
                     cwd + "/pj_scripts/easyvvuq_app",
                     app, common_dir + " ets_pj_input.nml"],
            "wd": cwd,
            "stdout": ets_campaign.campaign_dir + '/execute_' + key + '.stdout',
            "stderr": ets_campaign.campaign_dir + '/execute_' + key + '.stderr'
        },
        "resources": {
            "numCores": {
                "exact": 1
            }
        },
        "dependencies": {
            "after": ["encode_" + key]
        }
    }

    m.submit(Jobs().addStd(encode_job))
    m.submit(Jobs().addStd(execute_job))

# wait for completion of all PJ tasks and terminate the PJ manager
m.wait4all()
m.finish()
m.stopManager()
m.cleanup()

# Aggregate the results from all runs.
print("Aggregating the results")
output_filename = ets_campaign.params_info['out_file']['default']
output_columns = ['te']

aggregate = uq.elements.collate.AggregateSamples(
    ets_campaign,
    output_filename=output_filename,
    output_columns=output_columns,
    header=0,
)

aggregate.apply()

print("aggregated data:")
print(open(ets_campaign.data['files'][0], 'r').read())

# Analysis
print("Making the analysis")
analysis = uq.elements.analysis.PCEAnalysis(
    ets_campaign, value_cols=output_columns)

dist, cov = analysis.apply()

# Results
stats  = analysis.statistical_moments('te')
sobols = analysis.sobol_indices('te', 'first_order')

# Elapsed time
end_time = time.time()

# For plots
corep_file = cpo_dir + '/ets_coreprof_in.cpo'
corep = read(corep_file, 'coreprof')
rho = corep.rho_tor

print(stats)
print(rho)
#  Graphics for descriptive satatistics
#plots.plot_stats(rho, stats,
#                 xlabel=r'$\rho_{tor} app ~ [m]$', ylabel=r'$T_e [eV]$',
#                 ftitle='Te profile',
#                 fname='../figs/te_stats_pj.png')

#plots.plot_sobols(rho, sobols, uncert_params,
#                  ftitle=' First-Order Sobol indices - QoI: Te.',
#                  fname='../figs/ti_sobols_pj.png')
