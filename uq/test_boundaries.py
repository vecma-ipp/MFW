# -*- coding: UTF-8 -*-
import os, sys, time
import numpy as np
import chaospy as cp
import pandas as pd
import easyvvuq as uq
import matplotlib.pylab as plt
from ascii_cpo import read
from utils import plots
from templates.cpo_encoder import CPOEncoder
from templates.cpo_decoder import CPODecoder


# test_boundaries.py:
# Perform UQ for a given model using Non intrusive method.
# Uncertainties are driven by electrons and ions boudary condition (Edge).

print('>>> Strat of test_boundaries')

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

# The executable code to run
obj_dir = os.path.abspath("../standalone/bin/"+SYS)
exec_code = "ets_test"

# Define a specific parameter space
uncertain_params = {
    "Te_boundary": {
        "type": "float",
        "distribution": "Normal",
        "margin_error": 0.2,
    },
    "Ti_boundary": {
        "type": "float",
        "distribution": "Normal",
           "margin_error": 0.2,
      }
}

# For the output: quantities of intersts
output_columns = ["Te", "Ti"]

# Initialize Campaign object
print('>>> Initialize Campaign object')
campaign_name = "uq_boundaries_"
my_campaign = uq.Campaign(name=campaign_name, work_dir=tmp_dir)

# Create new directory for inputs (to be ended with /)
campaign_dir = my_campaign.campaign_dir
common_dir = campaign_dir +"/common/"
os.system("mkdir " + common_dir)

# Copy input CPO files
os.system("cp " + cpo_dir + "/*.cpo " + common_dir)

# Copy XML and XSD files
os.system("cp " + xml_dir + "/ets.x* "    + common_dir)
os.system("cp " + xml_dir + "/chease.x* " + common_dir)
os.system("cp " + xml_dir + "/bohmgb.x* " + common_dir)
os.system("cp " + xml_dir + "/gem0.x* "   + common_dir)

# Create the encoder and get the app parameters
print('>>> Create the encoder')
input_filename = "ets_coreprof_in.cpo"
encoder = CPOEncoder(template_filename=input_filename,
                     target_filename="ets_coreprof_in.cpo",
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
exec_path = os.path.join(obj_dir, exec_code)
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
print('>>> Get Descriptive Statistics')
stat_te = results['statistical_moments']['Te']
pctl_te = results['percentiles']['Te']
sob1_te = results['sobols_first']['Te']
sobt_te = results['sobols_total']['Te']

stat_ti = results['statistical_moments']['Ti']
pctl_ti = results['percentiles']['Ti']
sob1_ti = results['sobols_first']['Ti']
sobt_ti = results['sobols_total']['Ti']

#  Graphics for Descriptive satatistics
print('>>> Statictics and SA plots')
corep = read(os.path.join(cpo_dir,  "ets_coreprof_in.cpo"), "coreprof")
rho = corep.rho_tor_norm

test_case = cpo_dir.split('/')[-1]

# Save statistics
mean_te = list(stat_te['mean'])
std_te  = list(stat_te['std'])
mean_ti = list(stat_ti['mean'])
std_ti  = list(stat_ti['std'])

header = 'RHO_TOR_NORM \t MEAN_TE \t STD_TE \t MEAN_TI \t STD_TI'
np.savetxt('outputs/'+test_case+'_UQ_STATS.dat',
           np.c_[rho, mean_te, std_te, mean_ti, std_ti], delimiter='\t', header=header)

# Save into database
engine = my_campaign.campaign_db.engine

# Create new tables for results and store them in the data base
stat_te_df = pd.DataFrame.from_dict(stat_te)
stat_te_df.to_sql('STAT_TE', engine, if_exists='append')
sob1_te_df = pd.DataFrame.from_dict(sobt_te)
sob1_te_df.to_sql('SOB1_TE', engine, if_exists='append')
sobt_te_df = pd.DataFrame.from_dict(sobt_te)
sobt_te_df.to_sql('SOBT_TE', engine, if_exists='append')

stat_ti_df = pd.DataFrame.from_dict(stat_ti)
stat_ti_df.to_sql('STAT_TI', engine, if_exists='append')
sob1_ti_df = pd.DataFrame.from_dict(sobt_ti)
sob1_ti_df.to_sql('SOB1_TI', engine, if_exists='append')
sobt_ti_df = pd.DataFrame.from_dict(sobt_ti)
sobt_ti_df.to_sql('SOBT_TI', engine, if_exists='append')

os.system('cp '+ engine.url.database +' outputs')

# PLOTS
uparams_names = list(uncertain_params.keys())

plots.plot_stats_pctl(rho, stat_te, pctl_te,
                 xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'$Te$',
                 ftitle='Te profile ('+test_case+')',
                 fname='outputs/plots/Te_STAT_'+test_case)

plots.plot_sobols(rho, sobt_te, uparams_names,
                  ftitle='Total-Order Sobol indices - QoI: Te',
                  fname='outputs/plots/Te_SA_'+test_case)

plots.plot_stats_pctl(rho, stat_ti, pctl_ti,
                 xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'$T_i [eV]$',
                 ftitle='Te profile ('+test_case+')',
                 fname='outputs/plots/Ti_STAT_'+test_case)

plots.plot_sobols(rho, sobt_ti, uparams_names,
                  ftitle='Total-Order Sobol indices - QoI: Ti',
                  fname='outputs/plots/Ti_SA_'+test_case)

print('>>> End of test_boundaries')
