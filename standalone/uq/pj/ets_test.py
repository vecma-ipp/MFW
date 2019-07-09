import os
import time
import numpy as np
import chaospy as cp
import easyvvuq as uq

from ascii_cpo import read
import matplotlib.pylab as plt

from qcg.appscheduler.api.job import Jobs
from qcg.appscheduler.api.manager import LocalManager
from easypj.pj_configurator import PJConfigurator


'''
UQ test of ETS code using QCG Pilot Job (slurkm version).
Uncertainties in 4 flux tubes. Parameters: D1, D2, D3, D4.
Quantity of Interest: electron temperature (Te).
'''


# =========================================================
#               Tools for results plot
# =========================================================

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
def plot_stats(x, stats, xlabel, ylabel, ftitle, fname):
    mean = stats["mean"]
    var  = stats["var"]
    std =  stats['std']

    plt.switch_backend('agg')
    fig = plt.figure(figsize=(12,9))

    ax1 = fig.add_subplot(111)
    ax1.plot(x, mean, 'g-', alpha=0.75, label='Mean')
    ax1.plot(x, mean-std, 'b-', alpha=0.25)
    ax1.plot(x, mean+std, 'b-', alpha=0.25)
    ax1.fill_between(x, mean-std, mean+std,
                     alpha=0.25, label=r'Mean $\pm$ deviation')
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


# =========================================================
#               The UQ CODE + PJ for ETS
# =========================================================

# For the elapsed time
start_time = time.time()

# OS env
SYS = os.environ['SYS']

tmp_dir = os.environ['SCRATCH']

# Working directory
cwd = os.getcwd()
print("Running in directory: " + cwd)

# PJ Manager (switch on debugging, by default in api.log file)
client_conf = {'log_level': 'DEBUG'}
# ======= SLURM =======
m = LocalManager(['--log', 'warning'], client_conf)

# ======= LOCAL =======
#cores = 40
#m = LocalManager(['--nodes', str(cores)], client_conf)

print("Available resources:\n%s\n" % str(m.resources()))

print("Initializing Campaign")

# CPO and XML files
cpo_dir = os.path.abspath("../../data/TESTS/")
workflows_dir = os.path.abspath("../../../workflows/")

# The exec code (ETS, CHEASE and BOHMGB wrappers)
run_exec = os.path.abspath("../../bin/"+SYS+"/ets_run")

# Uncertain parameters: 4 flux tubes positions
uparams = ["D1", "D2", "D3", "D4"]

# Define parameter space
params = {
    uparams[0]: {
        "type": "real",
        "default": "0."},
    uparams[1]: {
        "type": "real",
        "default": "0."},
    uparams[2]: {
        "type": "real",
        "default": "0."},
    uparams[3]: {
        "type": "real",
        "default": "0."},
    "out_file": {
        "type": "str",
        "default": "output.csv"}}

output_filename = params["out_file"]["default"]
output_columns = ["te"]

# Initialize Campaign object
my_campaign = uq.Campaign(name='uq_ets', work_dir=tmp_dir)

# Copy xml and xsd files needed in the ETS wrappers
campaign_dir = my_campaign.campaign_dir
os.system("mkdir " + campaign_dir +"/workflows")
os.system("cp " + workflows_dir + "/ets.x* "+ campaign_dir +"/workflows")

# Copy CPO files in common directory
common_dir = campaign_dir +"/common"
os.system("mkdir " + common_dir)
os.system("cp " + cpo_dir + "/*.cpo " + common_dir)

# Create an encoder, decoder and collation element
encoder = uq.encoders.GenericEncoder(
    template_fname='ets_test.template',
    delimiter='#',
    target_filename='input.nml')

decoder = uq.decoders.SimpleCSV(target_filename=output_filename,
                                output_columns=output_columns,
                                header=0)
collation = uq.collate.AggregateSamples(average=False)

# Add the ETS app (automatically set as current app)
my_campaign.add_app(name="uq_ets",
                    params=params,
                    encoder=encoder,
                    decoder=decoder
                    )

# Create a collation element for this campaign
collater = uq.collate.AggregateSamples(average=False)
my_campaign.set_collater(collater)

# Get uncertain parameters values
coret_file = common_dir + "/ets_coretransp_in.cpo"
coret = read(coret_file, "coretransp")
diff_eff = coret.values[0].te_transp.diff_eff

# Create the sampler
vary = { uparams[k]: cp.Normal(diff_eff[k], 0.2*diff_eff[k]) for k in range(4)}
my_sampler = uq.sampling.PCESampler(vary=vary, polynomial_order=2)

# Associate the sampler with the campaign
my_campaign.set_sampler(my_sampler)

# Will draw all (of the finite set of samples)
my_campaign.draw_samples()

# Create & save PJ configurator
print("Creating configuration for QCG Pilot Job Manager")
pjc = PJConfigurator(my_campaign)
pjc.save()

logs_dir = campaign_dir +"/logs"
os.system("mkdir " + logs_dir)

# Execute encode -> execute for each run using QCG-PJ
print("Starting submission of tasks to QCG Pilot Job Manager")
pj_start_time = time.time()
for run in my_campaign.list_runs():
    key = run[0]
    encode_job = {
        "name": 'encode_' + key,
        "execution": {
            "exec": 'easyvvuq_encode',
            "args": [my_campaign.campaign_dir,
                     key],
            "wd": cwd,
            "stdout": my_campaign.campaign_dir + '/logs/encode_' + key + '.stdout',
            "stderr": my_campaign.campaign_dir + '/logs/encode_' + key + '.stderr'
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
            "exec": 'easyvvuq_execute',
            "args": [my_campaign.campaign_dir,
                     key,
                     'easyvvuq_app',
                     run_exec, "input.nml"],
            "wd": cwd,
            "stdout": my_campaign.campaign_dir + '/logs/execute_' + key + '.stdout',
            "stderr": my_campaign.campaign_dir + '/logs/execute_' + key + '.stderr'
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

print("Waiting for completion of all QCG PJ tasks")
# wait for completion of all PJ tasks and terminate the PJ manager
m.wait4all()
m.finish()
m.stopManager()
m.cleanup()

# Sync state of a campaign with PJConfigurator
print("Syncing state of campaign after execution of PJ")
pjc.finalize()

pj_end_time = time.time()

# Collating results
my_campaign.collate()

# Post-processing analysis
analysis = uq.analysis.PCEAnalysis(sampler=my_sampler, qoi_cols=output_columns)

my_campaign.apply_analysis(analysis)

results = my_campaign.get_last_analysis()

# Get Descriptive Statistics
stats = results['statistical_moments']['te']

# Elapsed time
end_time = time.time()
print('PJ elapsed times: ', (end_time - start_time)/60., ' mins.')
print('Total elapsed times: ', (pj_end_time - pj_start_time)/60., ' mins.')

#  Graphics for descriptive satatistics
corep_file = common_dir + '/ets_coreprof_in.cpo'
corep = read(corep_file, 'coreprof')
rho = corep.rho_tor

plot_stats(rho, stats,
           xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'$T_e [eV]$',
           ftitle='Te profile',
           fname='te_ets_stats')
