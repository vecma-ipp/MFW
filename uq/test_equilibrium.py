# -*- coding: UTF-8 -*-
import os
import sys
import time
import numpy as np
import pandas as pd
import chaospy as cp
import easyvvuq as uq
from ascii_cpo import read
from utils import cpo_tools
from templates.cpo_encoder import CPOEncoder
from templates.cpo_decoder import CPODecoder


# gem0_uq.py:
# Perform UQ for GEM
# Uncertainties are driven by: 1 Flux tubes.
# IMPORTANT CHECK: in gem.xml, nrho_transp = 1


print('>>> equilibrium_uq : START')

# For Ellapsed time
time0 = time.time()

# OS env
SYS = os.environ['SYS']

# Working directory
tmp_dir = os.environ['SCRATCH']

# CPO files
#cpo_dir = os.path.abspath("../workflows/AUG_28906_6")
cpo_dir = os.path.abspath("../workflows/JET_92436_23066")

# XML and XSD files
xml_dir = os.path.abspath("../workflows")

# The executable code to run
obj_dir = os.path.abspath("../standalone/bin/"+SYS)
exec_code = "equilibrium_test"
mpi_instance = None#os.environ['MPICMD']
exec_path = os.path.join(obj_dir, exec_code)

# Define a specific parameter space
uncertain_params = {
    "Te_grad_1": {
        "type": "float",
        "distribution": "Uniform",
        "margin_error": 0.2,
    },
    "Ti_grad_1": {
        "type": "float",
        "distribution": "Uniform",
        "margin_error": 0.2,
    },
    "Te_1": {
        "type": "float",
        "distribution": "Uniform",
        "margin_error": 0.2,
    },
    "Ti_1": {
        "type": "float",
        "distribution": "Uniform",
        "margin_error": 0.2,
    }
}

# For the output: quantities of intersts
output_columns = ["gm1", "gm2", "gm3", "gm4", "gm5", "gm6", "gm7", "gm8", "gm9"]

# Initialize Campaign object
print('>>> Initialize Campaign object')
campaign_name = "UQ_EQL_"+cpo_dir.split('/')[-1]+"_"
my_campaign = uq.Campaign(name=campaign_name, work_dir=tmp_dir)

# Create new directory for inputs (to be ended with /)
campaign_dir = my_campaign.campaign_dir
common_dir = campaign_dir +"/common/"
os.system("mkdir " + common_dir)
print('>>> common_dir = ', common_dir)

# Copy input CPO files (cf test_gem0.f90)
os.system("cp " + cpo_dir + "/ets_equilibrium_in.cpo "
                + common_dir + "eq_equilibrium_in.cpo")
os.system("cp " + cpo_dir + "/ets_coreprof_in.cpo "
                + common_dir + "/eq_coreprof_in.cpo")
os.system("cp " + cpo_dir + "/ets_toroidfield_in.cpo "
                + common_dir + "/eq_toroidfield_in.cpo")

# Copy XML and XSD files
os.system("cp " + xml_dir + "/chease.xml " + common_dir)
os.system("cp " + xml_dir + "/chease.xsd " + common_dir)

# We test 1 flux tube. VERIFY in gem0.xml: nrho_transp = 1
flux_indices = [61]

# Create the encoder and get the app parameters
print('>>> Create the encoder')
input_filename = "eq_coreprof_in.cpo"
encoder = CPOEncoder(template_filename=input_filename,
                     target_filename=input_filename,
                     common_dir=common_dir,
                     uncertain_params=uncertain_params,
                     cpo_name="coreprof",
                     flux_indices = flux_indices,
                     link_xmlfiles=True)

params, vary = encoder.draw_app_params()

# Create the decoder
print('>>> Create the decoder')
output_filename = "eq_equilibrium_out.cpo"
decoder = CPODecoder(target_filename=output_filename,
                     cpo_name="equilibrium",
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
my_sampler = uq.sampling.PCESampler(vary=vary, polynomial_order=3)
my_campaign.set_sampler(my_sampler)

# Will draw all (of the finite set of samples)
print('>>> Draw Samples')
my_campaign.draw_samples()

print('>>> Populate runs_dir')
my_campaign.populate_runs_dir()

print('>>> Execute The code runs')
my_campaign.apply_for_each_run_dir(uq.actions.ExecuteLocal(run_cmd=exec_path,
                                                           interpret=mpi_instance))

print('>>> Collate')
my_campaign.collate()

# Post-processing analysis
print('>>> Post-processing analysis')
analysis = uq.analysis.PCEAnalysis(sampler=my_sampler, qoi_cols=output_columns)
my_campaign.apply_analysis(analysis)

print('>>> Get results')
results = my_campaign.get_last_analysis()

print('>>> Ellapsed time: ', time.time() - time0)

# Get Descriptive Statistics
print('>>> Get Descriptive Statistics: \n')

# Get Descriptive Statistics
print('>>> Get Descriptive Statistics')

stat = []
sob1 = []

for qoi in  output_columns:
    stat.append(results['statistical_moments'][qoi])
    sob1.append(results['sobols_first'][qoi])

# Plots STAT and SA
__PLOTS = True
if __PLOTS:
    from utils import plots
    equil = read(os.path.join(cpo_dir,  "ets_equilibrium_in.cpo"), "equilibrium")
    rho = equil.profiles_1d.rho_tor
    uparams_names = list(params.keys())

    for i in range(10):

        plots.plot_stats(rho, stat[i],
                         xlabel=r'$\rho_{tor} ~ [m]$', ylabel='GM'+str(i+1),
                         ftitle='GM'+str(i+1)+' profile',
                         fname='outputs/plots/GM'+str(i+1)+'_STAT')

        plots.plot_sobols(rho, sob1[i], uparams_names,
                          ftitle=' First-Order Sobol indices - QoI: GM'+str(i+1),
                          fname='outputs/plots/GM'+str(i+1)+'_SOB1')

print('>>> equilibrium_uq : END')
