# -*- coding: UTF-8 -*-
import os
import sys
import time
import numpy as np
import pandas as pd
import chaospy as cp
import easyvvuq as uq
import matplotlib.pylab as plt
from ascii_cpo import read
from templates.xml_encoder import XMLEncoder
from templates.cpo_encoder import CPOEncoder
from templates.cpo_decoder import CPODecoder
from easypj import qcgpj_wrapper


# test_combined.py:
# Perform UQ for a given model using Non intrusive method.
# Uncertainties are driven by:
# - External sources of Electons heating.
# - Electrons boudary condition (Edge).

print('>>> test_combined_el PJ: START')

# For Ellapsed time
time0 = time.time()

# OS env
SYS = os.environ['SYS']

# Working directory
tmp_dir = os.environ['SCRATCH']

# CPO files
cpo_dir = os.path.abspath("../workflows/AUG_28906_6")
#cpo_dir = os.path.abspath("../workflows/JET_92436_23066")

# XML and XSD files
xml_dir = os.path.abspath("../workflows")

# The execuatble model code
obj_dir = os.path.abspath("../standalone/bin/"+SYS)
exec_code = "loop_gem0"

# Define a specific parameter space
uncertain_params_bc = {
    # Electrons boudary condition
    "Te_boundary": {
        "type": "float",
        "distribution": "Normal",
        "margin_error": 0.2,
    }
}
uncertain_params_src = {
    # Gaussian Sources: Electrons heating
    "amplitude_el":{
        "type": "float",
        "distribution": "Uniform",
        "margin_error": 0.15,
    },
    "position_el":{
        "type": "float",
        "distribution": "Uniform",
        "margin_error": 0.15,
    },
    "width_el":{
        "type": "float",
        "distribution": "Uniform",
        "margin_error": 0.15,
    }
}

# The Quantitie of intersts
output_columns = ["Te", "Ti"]

# Initialize Campaign object
print('>>> Initialize Campaign object')
campaign_name = "UQW_QMC_PJ_"
my_campaign = uq.Campaign(name=campaign_name, work_dir=tmp_dir)

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
print('>>> Create the encoders')
input_cpo_filename = "ets_coreprof_in.cpo"
encoder_cpo = CPOEncoder(template_filename=input_cpo_filename,
                     target_filename="ets_coreprof_in.cpo",
                     cpo_name="coreprof",
                     common_dir=common_dir,
                     uncertain_params=uncertain_params_bc)

params_cpo, vary_cpo = encoder_cpo.draw_app_params()

input_xml_filename = "source_dummy.xml"
encoder_xml = XMLEncoder(template_filename=input_xml_filename,
                     target_filename="source_dummy.xml",
                     common_dir=common_dir,
                     uncertain_params=uncertain_params_src)

params, vary = encoder_xml.draw_app_params()

# Combine both encoders into a single encoder
encoder = uq.encoders.MultiEncoder(encoder_cpo, encoder_xml)
params.update(params_cpo)
vary.update(vary_cpo)

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

# Create the sampler (10002 total samples using sobol sequences)
print('>>> Create the sampler')
my_sampler = uq.sampling.QMCSampler(vary=vary, n_samples=3334)
my_campaign.set_sampler(my_sampler)

# Will draw all (of the finite set of samples)
print('>>> Draw Samples')
my_campaign.draw_samples()

# Encode and Execute samples
exec_path = os.path.join(obj_dir, exec_code)
print('>>> Call QCG-Pilot Job')
t1 = time.time()
qcgpj_wrapper.run(my_campaign, exec_path)
t2 = time.time()

print('>>> PJ Ellapsed time = ', t2 - t1)

print('>>> Collate')
my_campaign.collate()

# Post-processing analysis
print('>>> Post-processing analysis')
analysis = uq.analysis.QMCAnalysis(sampler=my_sampler, qoi_cols=output_columns)
my_campaign.apply_analysis(analysis)

print('>>> Get results')
results = my_campaign.get_last_analysis()

# Get Descriptive Statistics
print('>>> Get Descriptive Statistics')
stat_te = results['statistical_moments']['Te']
sob1_te = results['sobols_first']['Te']

stat_ti = results['statistical_moments']['Ti']
sob1_ti = results['sobols_first']['Ti']

# Plots STAT and SA
__PLOTS = True # If True create plots subfolder under outputs folder
if __PLOTS:
    from utils import plots

    print('>>> Plot Statictics and SA')
    corep = read(os.path.join(cpo_dir,  "ets_coreprof_in.cpo"), "coreprof")
    rho = corep.rho_tor_norm

    uparams_names = list(params.keys())

    plots.plot_stats(rho, stat_te,
                     xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'$Te$',
                     ftitle='Te profile',
                     fname='outputs/plots/Te_STAT_'+campaign_name)

    plots.plot_sobols(rho, sob1_te, uparams_names,
                      ftitle=' First-Order Sobol indices - QoI: Te',
                      fname='outputs/plots/Te_SA_'+campaign_name)

    plots.plot_stats(rho, stat_ti,
                     xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'$T_i [eV]$',
                     ftitle='Te profile',
                     fname='outputs/plots/Ti_STAT_'+campaign_name)

    plots.plot_sobols(rho, sob1_ti, uparams_names,
                      ftitle=' First-Order Sobol indices - QoI: Ti',
                  fname='outputs/plots/Ti_SA_'+campaign_name)

print('>>> test_combined_el PJ: END')
