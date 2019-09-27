# -*- coding: UTF-8 -*-
import os, sys, time
import chaospy as cp
import easyvvuq as uq
import matplotlib.pylab as plt
from utils import plots
from ascii_cpo import read
from templates.xml_encoder import XMLEncoder
from templates.cpo_decoder import CPODecoder
from qcg.appscheduler.api.job import Jobs
from qcg.appscheduler.api.manager import LocalManager


# Gaussian Sources test:
# UQ for a given model(s) using Non intrisive method.
# Uncertainties in Sources of Electons and Ions.

# For Ellapsed time
time0 = time.time()

# establish available resources
#cores = 32

# set location of log file
# client_conf = {'log_file': tmpdir.join('api.log'), 'log_level': 'DEBUG'}

# switch on debugging (by default in api.log file)
client_conf = {'log_level': 'DEBUG'}

# switch on debugging (by default in api.log file)
#m = LocalManager(['--nodes', str(cores)], client_conf)

# This can be used for execution of the test using a separate (non-local) instance of PJManager
#
# get available resources
res = m.resources()
# remove all jobs if they are already in PJM
# (required when executed using the same QCG-Pilot Job Manager)
# m.remove(m.list().keys())

print(">>> PJ: Available resources:\n%s\n" % str(m.resources()))

# OS env
SYS = os.environ['SYS']

# Current directory
cwd = os.getcwd()

# Working directory
tmp_dir = os.environ['SCRATCH']

# CPO files
cpo_dir = os.path.abspath("../data/AUG_28906_6/")

# XML and XSD files
xml_dir = os.path.abspath("../../workflows")

# execuatbles
obj_dir = os.path.abspath("../bin/"+SYS)

# the executable code to run
exec_code = "loop_gem0"
bbox = os.path.join(obj_dir, exec_code)

# Define a specific parameter space
uncertain_params = {
    # Electons heating Gaussian
    "amplitude_el":{
        "type": "float",
        "distribution": "Uniform",
        "margin_error": 0.2,
    },
    "position_el":{
        "type": "float",
        "distribution": "Uniform",
        "margin_error": 0.25,
    },
    "width_el":{
        "type": "float",
        "distribution": "Uniform",
        "margin_error": 0.2,
    },
    # Ions heatings Gaussian
    "amplitude_ion":{
        "type": "float",
        "distribution": "Uniform",
        "margin_error": 0.25
    },
    "position_ion":{
        "type": "float",
        "distribution": "Uniform",
        "margin_error": 0.25
    },
    "width_ion":{
        "type": "float",
        "distribution": "Uniform",
        "margin_error": 0.2
    }
}

# For the output: quantities of intersts
output_columns = ["Te", "Ti"]

# Initialize Campaign object
print('>>> Initialize Campaign object')
campaign_name = "uq_src"
my_campaign = uq.Campaign(name='uq_src', work_dir=tmp_dir)

# Create new directory for commons inputs
campaign_dir = my_campaign.campaign_dir
common_dir = campaign_dir +"/common/"
os.system("mkdir " + common_dir)

# Copy XML and XSD files
os.system("cp " + xml_dir + "/ets.x* "    + common_dir)
os.system("cp " + xml_dir + "/chease.x* " + common_dir)
os.system("cp " + xml_dir + "/gem0.x* "   + common_dir)
os.system("cp " + xml_dir + "/source_dummy.x* " + common_dir)

# Copy input CPO files in common directory
os.system("cp " + cpo_dir + "/ets_coreprof_in.cpo "    + common_dir)
os.system("cp " + cpo_dir + "/ets_equilibrium_in.cpo " + common_dir)
os.system("cp " + cpo_dir + "/ets_coreimpur_in.cpo "   + common_dir)
os.system("cp " + cpo_dir + "/ets_coretransp_in.cpo "  + common_dir)
os.system("cp " + cpo_dir + "/ets_toroidfield_in.cpo " + common_dir)

# Create the encoder and get the app parameters
print('>>> Create the encoder')
input_filename = "source_dummy.xml"
encoder = XMLEncoder(template_filename=input_filename,
                     target_filename="source_dummy.xml",
                     common_dir=common_dir,
                     uncertain_params=uncertain_params)

params, vary = encoder.draw_app_params()

# Create the encoder
print('>>> Create the decoder')
output_filename = "ets_coreprof_out.cpo"
decoder = CPODecoder(target_filename=output_filename,
                     cpo_name="coreprof",
                     output_columns=output_columns)


# Create a collation element for this campaign
print('>>> Create Collater')
collater = uq.collate.AggregateSamples(average=False)

# Add the ETS app (automatically set as current app)
print('>>> Add app to campagn object')
my_campaign.add_app(name=campaign_name,
                    params=params,
                    encoder=encoder,
                    decoder=decoder,
                    collater=collater)

# Create the sampler
print('>>> Create the sampler')
my_sampler = uq.sampling.PCESampler(vary=vary,
                                    polynomial_order=3,
                                    quadrature_rule='G',
                                    sparse=False)
my_campaign.set_sampler(my_sampler)

# Will draw all (of the finite set of samples)
print('>>> Draw Samples')
my_campaign.draw_samples()

#my_campaign.populate_runs_dir()
#my_campaign.apply_for_each_run_dir(uq.actions.ExecuteLocal(bbox))

# Execute encode -> execute for each run using QCG-PJ
print(">>> Starting submission of tasks to QCG Pilot Job Manager")
encoder_path = os.path.realpath(os.path.expanduser("easypj/easyvvuq_encode"))
execute_path = os.path.realpath(os.path.expanduser("easypj/easyvvuq_execute"))
for run in my_campaign.list_runs():

    key = run[0]
    run_dir = run[1]['run_dir']

    enc_args = [
        my_campaign.db_type,
        my_campaign.db_location,
        'FALSE',
        campaign_name,
        campaign_name,
        key
    ]

    exec_args = [
        run_dir,
        'easyvvuq_app',
        bbox
    ]

    encode_task = {
        "name": 'encode_' + key,
        "execution": {
            "exec": encoder_path,
            "args": enc_args,
            "wd": cwd,
            "stdout": my_campaign.campaign_dir + '/encode_' + key + '.stdout',
            "stderr": my_campaign.campaign_dir + '/encode_' + key + '.stderr'
        },
        "resources": {
            "numCores": {
                "exact": 1
            }
        }
    }

    execute_task = {
        "name": 'execute_' + key,
        "execution": {
            "exec": execute_path,
            "args": exec_args,
            "wd": cwd,
            "stdout": my_campaign.campaign_dir + '/execute_' + key + '.stdout',
            "stderr": my_campaign.campaign_dir + '/execute_' + key + '.stderr'
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

    m.submit(Jobs().addStd(encode_task))
    m.submit(Jobs().addStd(execute_task))

# Wait for completion of all PJ tasks and terminate the PJ manager
print(">>> Wait for completion of all PJ tasks")
m.wait4all()
m.finish()
m.stopManager()
m.cleanup()

print(">>> Syncing state of campaign after execution of PJ")
def update_status(run_id, run_data):
    my_campaign.campaign_db.set_run_statuses([run_id], uq.constants.Status.ENCODED)

my_campaign.call_for_each_run(update_status, status=uq.constants.Status.NEW)

print('>>> Collate')
my_campaign.collate()

# Post-processing analysis
print('>>> Post-processing analysis')
analysis = uq.analysis.PCEAnalysis(sampler=my_sampler, qoi_cols=output_columns)
my_campaign.apply_analysis(analysis)

print('>>> Get results')
results = my_campaign.get_last_analysis()

# Get Descriptive Statistics
print('>>> Get Descriptive Statistics')
stats_te = results['statistical_moments']['Te']
pctl_te = results['percentiles']['Te']
stot_te = results['sobols_total']['Te']

stats_ti = results['statistical_moments']['Ti']
pctl_ti = results['percentiles']['Ti']
stot_ti = results['sobols_total']['Ti']

print('>>> Ellapsed time: ', time.time() - time0)

#  Graphics for Sescriptive satatistics
print('>>> Statictics and SA plots')
corep = read(os.path.join(cpo_dir,  "ets_coreprof_in.cpo"), "coreprof")
rho = corep.rho_tor
uparams_names = list(uncertain_params.keys())
plots.plot_stats_pctl(rho, stats_te, pctl_te,
                 xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'$Te$',
                 ftitle='Te profile',
                 fname='outputs/figs/te_src_stat')

plots.plot_sobols(rho, stot_te, uparams_names,
                  ftitle=' Total-Order Sobol indices - QoI: Te',
                  fname='outputs/figs/te_src_sob')

plots.plot_stats_pctl(rho, stats_ti, pctl_ti,
                 xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'$T_i [eV]$',
                 ftitle='Te profile',
                 fname='outputs/figs/ti_src_stat')

plots.plot_sobols(rho, stot_ti, uparams_names,
                  ftitle=' Total-Order Sobol indices - QoI: Ti',
                  fname='outputs/figs/ti_src_sob')

print('>>> End of test_sources_PJ')
