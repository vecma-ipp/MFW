import os
import time
import numpy as np
import chaospy as cp
import easyvvuq as uq

from ascii_cpo import read
#from plots import plot_stats, plot_sobols
import matplotlib.pylab as plt

#from easyvvuq.execution.qcgpj.pj_utils.pj_configurator import PJConfigurator
from easypj.pj_configurator import PJConfigurator
from qcg.appscheduler.api.manager import Manager
from qcg.appscheduler.api.job import Jobs
from qcg.appscheduler.api.manager import LocalManager


'''
UQ test of ETS code using QCG Pilot Job.
Uncertainties in 4 flux tubes. Parameters: D1, D2, D3, D4.
Quantity of Interest: electron temperature (Te).
'''

# Util for scaling coordianates
def format_exponent(ax, axis='y'):
    # Change the ticklabel format to scientific format
    ax.ticklabel_format(axis=axis, style='sci', scilimits=(-2, 2))

    # Get the appropriate axis
    if axis == 'y':
        ax_axis = ax.yaxis
        x_pos = 0.0
        y_pos = 1.0
        horizontalalignment='left'
        verticalalignment='bottom'
    else:
        ax_axis = ax.xaxis
        x_pos = 1.0
        y_pos = -0.05
        horizontalalignment='right'
        verticalalignment='top'


# Statistical Moments (mean +- deviation and variance)
def plot_stats(x, stat, xlabel, ylabel, ftitle, fname):
    mean = np.array(stat["mean"])
    var  = stat["var"]
    std = np.array(stat['std'])

    plt.switch_backend('agg')
    fig = plt.figure(figsize=(12,9))

    ax1 = fig.add_subplot(111)
    ax1.plot(x, mean, 'g-', alpha=0.75, label='Mean')
    ax1.plot(x, mean-std, 'b-', alpha=0.25)
    ax1.plot(x, mean+std, 'b-', alpha=0.25)
    ax1.fill_between(x, mean-std, mean+std, alpha=0.25, label=r'Mean $\pm$ deviation')
    ax1.set_xlabel(xlabel)
    ax1.set_ylabel(ylabel, color='b')
    ax1.tick_params('y', colors='b')
    ax1.grid()
    ax1.legend()

    ax2 = ax1.twinx()
    ax2.plot(x, var, 'r-', alpha=0.5)
    ax2.set_ylabel('Variance', color='r')
    ax2.tick_params('y', colors='r')
    ax2 = format_exponent(ax2, axis='y')

    plt.title(ftitle)
    fig.savefig(fname)
    plt.close(fig)


# First Sobol indicies
def plot_sobols(x, sobols, params, ftitle, fname):
    plt.switch_backend('agg')
    npar = len(params)

    plt.switch_backend('agg')
    fig = plt.figure(figsize=(12,9))
    ax = fig.add_subplot(111)

    for i in range(npar):
        s = sobols[params[i]]
        ax.plot(x, s, label=params[i])

    ax.set_xlabel(r'$\rho_{tor} ~ [m]$')
    ax.set_ylabel(r'$1^{st} ~ Sobol$')

    ax.set_title(ftitle)
    plt.legend()
    fig.savefig(fname)
    plt.close(fig)


# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>> UQ CODE + PJ

start_time = time.time()

# Environment infos
#===================

# Working directories
cwd = os.getcwd()

# TODO to be adapted to other machines like sys in config
tmp_dir = os.environ['SCRATCH']

# Machine name (cf. MFW/config)
SYS = os.environ['SYS']

# Set location of log file
client_conf = {'log_file': os.path.join(tmp_dir, "api.log")}

# Create QCG Pilot Job Manager
m = LocalManager(['--log', 'warning'], client_conf)

print("available resources:\n%s\n" % str(m.resources()))


# Application infos
#===================

# CPO files location
cpo_dir = os.path.abspath("../../data/TESTS/")

# The executable application
app = os.path.abspath("../../bin/"+SYS+"/ets_pj_run ")
print("app =" % str(app))	##

# Uncertain parameters
uncert_params = ["D1", "D2", "D3", "D4"]
nparams = 4

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
    default_campaign_dir_prefix='ETS_PJ_'
)

campaign_dir = ets_campaign.campaign_dir

# Copy xml files needed in the ETS code
os.system("mkdir " + campaign_dir +"/workflows")
os.system("cp ../../../workflows/ets.xml "+ campaign_dir +"/workflows")
os.system("cp ../../../workflows/ets.xsd "+ campaign_dir +"/workflows")

# Copy CPO files in common directory
common_dir = campaign_dir +"/common/"
os.system("cp " + cpo_dir + "/*.cpo " + common_dir)

# Uncertain parameters distrubutions
list_dist = [cp.Normal(Ds[i], 0.2*Ds[i]) for i in range(nparams)]

# Define the parameters dictionary
for i in range(nparams):
    ets_campaign.vary_param(uncert_params[i], dist=list_dist[i])

# Create the sampler
##ets_sampler  = uq.elements.sampling.PCESampler(ets_campaign)
ets_sampler  = uq.sampling.PCESampler(ets_campaign)	##

# Generate runs
ets_campaign.add_runs(ets_sampler)

# Create PJ configurator
pjc = PJConfigurator(ets_campaign)
pjc.init_runs_dir()

# Save PJ configuration
pjc.save()

# Execute encode -> execute for each run using QCG-PJ
os.system("mkdir " + campaign_dir +"/logs")
for key, data in ets_campaign.runs.items():

    encode_job = {
        "name": 'encode_' + key,
        "execution": {
            "exec": "./pj_scripts/easyvvuq_encode",
            "args": [ets_campaign.campaign_dir,
                     key],
            "wd": cwd,
            "stdout": ets_campaign.campaign_dir + '/logs/encode_' + key + '.stdout',
            "stderr": ets_campaign.campaign_dir + '/logs/encode_' + key + '.stderr'
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
            "stdout": ets_campaign.campaign_dir + '/logs/execute_' + key + '.stdout',
            "stderr": ets_campaign.campaign_dir + '/logs/execute_' + key + '.stderr'
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

##aggregate = uq.elements.collate.AggregateSamples(
aggregate = uq.collate.AggregateSamples(
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
##analysis = uq.elements.analysis.PCEAnalysis(
analysis = uq.analysis.PCEAnalysis(
    ets_campaign, value_cols=output_columns)

analysis.apply()

# Elapsed time
end_time = time.time()
print(" Elapsed time = ", end_time - start_time )


# For plots
corep_file = cpo_dir + '/ets_coreprof_in.cpo'
corep = read(corep_file, 'coreprof')
rho = corep.rho_tor

# Graphics for descriptive satatistics
plot_stats(rho, stats["te"],
                 xlabel=r'$\rho_{tor} app ~ [m]$', ylabel=r'$T_e [eV]$',
                 ftitle='Te profile',
                 fname='te_stats_pj.png')

plot_sobols(rho, sobols["te"], uncert_params,
                  ftitle=' First-Order Sobol indices - QoI: Te.',
                  fname='ti_sobols_pj.png')

