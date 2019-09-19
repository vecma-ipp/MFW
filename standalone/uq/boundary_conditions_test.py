import os, sys
import time
import chaospy as cp
import easyvvuq as uq
import matplotlib.pylab as plt
from ascii_cpo import read
from tools import plots, cpo_template

# Boundary conditions test:
# UQ for a given model(s) using Non intrisive method.
# Uncertainties in Te and Ti boudaries (Edge)

# For Ellapsed time
time0 = time.time()

# OS env
SYS = os.environ['SYS']

# Working directory
tmp_dir = os.environ['SCRATCH']

# CPO files
cpo_dir = os.path.abspath("../data/AUG_28906_6/")

# The path to the executable code to run
bbox_exe = os.path.abspath("../bin/"+SYS+"/loop_test2 ")

# Define a specific parameter space
uncertain_params = {
    "Te_boundary": {
        "type": "float",
        "distribution": "Normal",
        "margin_error": 0.25,
    },
    "Ti_boundary": {
        "type": "float",
        "distribution": "Normal",
        "margin_error": 0.25,
    }
}

# For the output: quantities of intersts
output_columns = ["Te", "Ti"]

# Initialize Campaign object
print('>>> Initialize Campaign object')
my_campaign = uq.Campaign(name = 'uq_test', work_dir=tmp_dir)

# Copy XML files needed in the wrappers
campaign_dir = my_campaign.campaign_dir
os.system("mkdir " + campaign_dir +"/workflows")
os.system("cp ../../workflows/ets.x* "+ campaign_dir +"/workflows")
os.system("cp ../../workflows/chease.x* "+ campaign_dir +"/workflows")
os.system("cp ../../workflows/bohmgb.x* "+ campaign_dir +"/workflows")
os.system("cp ../../workflows/gem0.x* "+ campaign_dir +"/workflows")

# Copy input CPO files in common directory
common_dir = campaign_dir +"/common/"
os.system("mkdir " + common_dir)
os.system("cp " + cpo_dir + "/*.cpo " + common_dir)

# Create the encoder and get the app parameters
print('>>> Create the encoder')
input_filename = "ets_coreprof_in.cpo"
encoder = cpo_template.CPOEncoder(template_filename=input_filename,
                                  template_cponame="coreprof",
                                  cpos_directory=common_dir,
                                  target_filename=input_filename,
                                  uncertain_params=uncertain_params
                                 )

params, vary = encoder.draw_app_params()

# Create the encoder
print('Create the decoder')
output_filename = "ets_coreprof_out.cpo"
decoder = cpo_template.CPODecoder(target_filename=output_filename,
                                  target_cponame="coreprof",
                                  output_columns=output_columns)

# Add the ETS app (automatically set as current app)
print('>>> Add app to campagn object')
my_campaign.add_app(name="uq_test",
                    params=params,
                    encoder=encoder,
                    decoder=decoder
                    )

# Create a collation element for this campaign
print('>>> Create Collater')
collater = uq.collate.AggregateSamples(average=False)
my_campaign.set_collater(collater)

# Create the sampler
print('>>> Create the sampler')
my_sampler = uq.sampling.PCESampler(vary=vary,
                                    polynomial_order=4,
                                    quadrature_rule='G',
                                    sparse=False
                                    )
my_campaign.set_sampler(my_sampler)

# Will draw all (of the finite set of samples)
print('>>> Draw Samples')
my_campaign.draw_samples()

print('>>> Populate runs_dir')
my_campaign.populate_runs_dir()

print('>>> Execute BlackBox code')
my_campaign.apply_for_each_run_dir(uq.actions.ExecuteLocal(bbox_exe))

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

#  Graphics for Descriptive satatistics
print('>>> Statictics and SA plots')
corep = read(os.path.join(cpo_dir,  "ets_coreprof_in.cpo"), "coreprof")
rho = corep.rho_tor
uncertain_params = ["Te_boundary", "Ti_boundary"]
plots.plot_stats_pctl(rho, stats_te, pctl_te,
                 xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'$Te$',
                 ftitle='Te profile',
                 fname='outputs/figs/te_ets_stats2')

plots.plot_sobols(rho, stot_te, uncertain_params,
                  ftitle=' Total-Order Sobol indices - QoI: Te',
                  fname='outputs/figs/te_ets_stot2')

plots.plot_stats_pctl(rho, stats_ti, pctl_ti,
                 xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'$T_i [eV]$',
                 ftitle='Te profile',
                 fname='outputs/figs/ti_ets_stats2')

plots.plot_sobols(rho, stot_ti, uncertain_params,
                  ftitle=' Total-Order Sobol indices - QoI: Ti',
                  fname='outputs/figs/ti_ets_stot2')

print('>>> End of boundary_conditions_test.')
