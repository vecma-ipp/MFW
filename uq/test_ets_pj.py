# -*- coding: UTF-8 -*-
import os
import sys
import time

import numpy as np
import pandas as pd
import chaospy as cp
import easyvvuq as uq

from ascii_cpo import read

from templates.cpo_encoder import CPOEncoder
from templates.cpo_decoder import CPODecoder

import easypj
from easypj import TaskRequirements, Resources
from easypj import Task, TaskType, SubmitOrder


# ets_uqpj.py:
# Perform UQ for ETS
# QCG-PJ is used here to run samples
# Uncertainties are driven by:
# - Boudary conditions (Edge) of electrons and ions tempurature.
# QoI: ion and electon temperatures


print('>>> ets_uqpj : START')

# For Ellapsed time
t0 = time.time()

# OS env
SYS = os.environ['SYS']

# Working directory
tmp_dir = os.environ['SCRATCH']

# CPO files
cpo_dir = os.path.abspath("../workflows/AUG_28906_6")

# XML and XSD files
xml_dir = os.path.abspath("../workflows")

# The executable code to run
obj_dir = os.path.abspath("../standalone/bin/"+SYS)
exec_code = "ets_test"

# Define a specific parameter space
uncertain_params = {
    "Te_boundary": {
        "type": "float",
        "distribution": "Normal",
        "margin_error": 0.4,
    },
    "Ti_boundary": {
        "type": "float",
        "distribution": "Normal",
           "margin_error": 0.4,
      }
}

# For the output: quantities of intersts
output_columns = ["Te", "Ti"]

# Initialize Campaign object
print('>>> Initialize Campaign object')
campaign_name = "UQPJ_ETS_"
my_campaign = uq.Campaign(name=campaign_name, work_dir=tmp_dir)

# Create new directory for inputs (to be ended with /)
campaign_dir = my_campaign.campaign_dir
common_dir = campaign_dir +"/common/"
os.system("mkdir " + common_dir)

# Copy input CPO files (cf test_ets.f90)
os.system("cp " + cpo_dir + "/*.cpo " + common_dir)

# Copy XML and XSD files
os.system("cp " + xml_dir + "/ets.xml " + common_dir)
os.system("cp " + xml_dir + "/ets.xsd " + common_dir)

# Create the encoder and get the app parameters
print('>>> Create the encoder')
input_filename = "ets_coreprof_in.cpo"
encoder = CPOEncoder(template_filename=input_filename,
                     target_filename=input_filename,
                     common_dir=common_dir,
                     uncertain_params=uncertain_params,
                     cpo_name="coreprof",
                     link_xmlfiles=True)

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

# Create the sampler (500*2 = 1000 total samples using qMC)
print('>>> Create the sampler')
my_sampler = uq.sampling.QMCSampler(vary=vary, n_samples=10)
my_campaign.set_sampler(my_sampler)

# Will draw all (of the finite set of samples)
print('>>> Draw Samples')
my_campaign.draw_samples()

print(">>> PJ: Starting execution")
exec_path = os.path.join(obj_dir, exec_code)

qcgpjexec = easypj.Executor()
qcgpjexec.create_manager(dir=my_campaign.campaign_dir, resources='4')

qcgpjexec.add_task(Task(TaskType.ENCODING,
                        TaskRequirements(cores=Resources(exact=1))))

qcgpjexec.add_task(Task(TaskType.EXECUTION,
                        TaskRequirements(cores=Resources(exact=1)),
                        application=exec_path))

qcgpjexec.run(campaign=my_campaign,
              submit_order=SubmitOrder.RUN_ORIENTED)

qcgpjexec.terminate_manager()

print('>>> Collate')
my_campaign.collate()

# Post-processing analysis
print('>>> Post-processing analysis')
analysis = uq.analysis.QMCAnalysis(sampler=my_sampler, qoi_cols=output_columns)
my_campaign.apply_analysis(analysis)

print('>>> Get results')
results = my_campaign.get_last_analysis()
t3 = time.time()

# print Ellapsed time
#print('>>> PJ time = ', t2 - t1)
print('>>> Total time = ', t3 - t0)

# Get Descriptive Statistics
print('>>> Get Descriptive Statistics')
stat_te = results['statistical_moments']['Te']
sob1_te = results['sobols_first']['Te']
stat_ti = results['statistical_moments']['Ti']
sob1_ti = results['sobols_first']['Ti']

corep = read(os.path.join(cpo_dir,  "ets_coreprof_in.cpo"), "coreprof")
rho = corep.rho_tor_norm

#print statistics
__PLOTS = True # If True create plots subfolder under outputs folder

if __PLOTS:
    from utils import plots
    from utils import plots

    uparams_names = list(params.keys())

    plots.plot_stats(rho, stat_te,
                     xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'$Te$',
                     ftitle='Te profile',
                     fname='outputs/plots/'+campaign_name+'Te_STAT')

    plots.plot_sobols_all(rho, sob1_te, uparams_names,
                      ftitle=' First-Order Sobol indices - QoI: Te',
                      fname='outputs/plots/'+campaign_name+'Te_S1')

    plots.plot_stats(rho, stat_ti,
                     xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'$T_i [eV]$',
                     ftitle='Ti profile',
                     fname='outputs/plots/'+campaign_name+'Ti_STAT')

    plots.plot_sobols_all(rho, sob1_ti, uparams_names,
                      ftitle=' First-Order Sobol indices - QoI: Ti',
                      fname='outputs/plots/'+campaign_name+'Ti_S1')

print('>>> ets_uqpj : END')
