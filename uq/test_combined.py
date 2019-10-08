# -*- coding: UTF-8 -*-
import os, sys, time
import chaospy as cp
import easyvvuq as uq
import matplotlib.pylab as plt
from ascii_cpo import read
from utils import plots
from templates.xml_encoder import XMLEncoder
from templates.cpo_encoder import CPOEncoder
from templates.cpo_decoder import CPODecoder


# Combined test:
# UQ for a given model(s) using Non intrusive method.
# External Uncertainties in:
# - Electons heating sources.
# - Boundary conditions of Electrons Temperature in the edge.

# For Ellapsed time
time0 = time.time()

# OS env
SYS = os.environ['SYS']

# Working directory
tmp_dir = os.environ['SCRATCH']

# CPO files
cpo_dir = os.path.abspath("../workflows/AUG_28906_6/")
#cpo_dir = os.path.abspath("../../workflows/JET_92436_23066/")

# XML and XSD files
xml_dir = os.path.abspath("../workflows")

# The execuatble model code
obj_dir = os.path.abspath("../workflow/bin/"+SYS)
exec_code = "loop_gem0"
bbox = os.path.join(obj_dir, exec_code)

# Define a specific parameter space
uncertain_params_bc = {
    # Electron tempearture in the Edge
    "Te_boundary": {
        "type": "float",
        "distribution": "Normal",
        "margin_error": 0.2,
    }
}
uncertain_params_src = {
    # Gaussian Sources: Ions heating
    "amplitude_el":{
        "type": "float",
        "distribution": "Uniform",
        "margin_error": 0.2,
    },
    "position_el":{
        "type": "float",
        "distribution": "Uniform",
        "margin_error": 0.2,
    },
    "width_el":{
        "type": "float",
        "distribution": "Uniform",
        "margin_error": 0.2,
    }
}

# The Quantitie of intersts
output_columns = ["Te"]

# Initialize Campaign object
print('>>> Initialize Campaign object')
campaign_name = "uq_combined"
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

# Create the sampler
print('>>> Create the sampler')
my_sampler = uq.sampling.PCESampler(vary=vary,
                                    polynomial_order=4,
                                    quadrature_rule='P',
                                    sparse=True)
my_campaign.set_sampler(my_sampler)

# Will draw all (of the finite set of samples)
print('>>> Draw Samples')
my_campaign.draw_samples()

print('>>> Populate runs_dir')
my_campaign.populate_runs_dir()

print('>>> Execute BlackBox code')
my_campaign.apply_for_each_run_dir(uq.actions.ExecuteLocal(bbox))

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
stats = results['statistical_moments']['Te']
pctl = results['percentiles']['Te']
stot = results['sobols_total']['Te']

print('>>> Ellapsed time: ', time.time() - time0)

#  Graphics for Sescriptive satatistics
print('>>> Statictics and SA plots')
corep = read(os.path.join(cpo_dir,  "ets_coreprof_in.cpo"), "coreprof")
rho = corep.rho_tor
params_names = list(params.keys())
test_case=cpo_dir.split('/')[-1]

plots.plot_stats_pctl(rho, stats, pctl,
                 xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'$Te$',
                 ftitle='Te profile ('+test_case+')',
                 fname='plots/Te_STAT_'+test_case)

plots.plot_sobols(rho, stot, params_names,
                  ftitle=' Total-Order Sobol indices - QoI: Te',
                  fname='plots/Te_SA_'+test_case)

print('>>> End of test_combined')
