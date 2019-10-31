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
# Perform UQ for GEM0.
# Uncertainties are driven by:


print('>>> gem0_uq : START')

# For Ellapsed time
time0 = time.time()

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
exec_code = "gem0_test"
exec_path = os.path.join(obj_dir, exec_code)

# Define a specific parameter space
#uncertain_params = {
#    "Te_grad": {
#        "type": "float",
#        "distribution": "Normal",
#        "margin_error": 0.1,
#    },
#    "Ti_grad": {
#        "type": "float",
#        "distribution": "Normal",
#       "margin_error": 0.1,
#    }
#}

uncertain_params = {
    "Te": {
        "type": "float",
        "distribution": "Normal",
        "margin_error": 0.2,
    },
    "Ti": {
        "type": "float",
        "distribution": "Normal",
        "margin_error": 0.2,
    }
}

# For the output: quantities of intersts
output_columns = ["Te_transp_flux", "Ti_transp_flux"]

# Initialize Campaign object
print('>>> Initialize Campaign object')
campaign_name = "UQ_AUG_GEM0_"
my_campaign = uq.Campaign(name=campaign_name, work_dir=tmp_dir)

# Create new directory for inputs (to be ended with /)
campaign_dir = my_campaign.campaign_dir
common_dir = campaign_dir +"/common/"
os.system("mkdir " + common_dir)

# Copy input CPO files (cf test_gem0.f90)
os.system("cp " + cpo_dir + "/ets_equilibrium_in.cpo " + common_dir)
os.system("cp " + cpo_dir + "/ets_coreprof_in.cpo " + common_dir)

# Copy XML and XSD files
os.system("cp " + xml_dir + "/gem0.xml " + common_dir)
os.system("cp " + xml_dir + "/gem0.xsd " + common_dir)

# Run test_gem0 to get flux tube indices
full_cmd = f'cd {common_dir}\n{exec_path}\n'
os.system(full_cmd)
corep_file= os.path.join(common_dir, "ets_coreprof_in.cpo")
coret_file= os.path.join(common_dir, "gem0_coretransp_out.cpo")
# TODO We test just one flux tube. Verify in gem0.xml: nrho_transp = 1
flux_index = cpo_tools.get_flux_index(corep_file, coret_file)[0]
print('>>>> flux_index = ', flux_index)

# Create the encoder and get the app parameters
print('>>> Create the encoder')
input_filename = "ets_coreprof_in.cpo"
encoder = CPOEncoder(template_filename=input_filename,
                     target_filename="ets_coreprof_in.cpo",
                     common_dir=common_dir,
                     uncertain_params=uncertain_params,
                     cpo_name="coreprof",
                     flux_index = flux_index,
                     link_xmlfiles=True)

params, vary = encoder.draw_app_params()

# Create the encoder
print('>>> Create the decoder')
output_filename = "gem0_coretransp_out.cpo"
decoder = CPODecoder(target_filename=output_filename,
                     cpo_name="coretransp",
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
                                    polynomial_order=4,
                                    quadrature_rule='G',
                                    sparse=False)
my_campaign.set_sampler(my_sampler)

# Will draw all (of the finite set of samples)
print('>>> Draw Samples')
my_campaign.draw_samples()

print('>>> Populate runs_dir')
my_campaign.populate_runs_dir()

print('>>> Execute BlackBox code')
my_campaign.apply_for_each_run_dir(uq.actions.ExecuteLocal(exec_path))

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
stat_te = results['statistical_moments']['Te_transp_flux']
sob1_te = results['sobols_first']['Te_transp_flux']
stat_ti = results['statistical_moments']['Ti_transp_flux']
sob1_ti = results['sobols_first']['Ti_transp_flux']

# Save results in CSV file
#mean_te = list(stat_te['mean'])
#std_te  = list(stat_te['std'])
#mean_ti = list(stat_ti['mean'])

for qoi in output_columns:
    print('===========================================')
    print(qoi)
    print('STAT = ', results['statistical_moments'][qoi])
    print('Sobol 1st = ', results['sobols_first'][qoi])
    #print('Sobol tot = ', results['sobols_total'][qoi])

print('>>> gem0_uq : END')
