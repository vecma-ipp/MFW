import os, sys
import time
import pandas as pd
import chaospy as cp
import easyvvuq as uq
import matplotlib.pylab as plt
from ascii_cpo import read
from tools import plots, cpo_template

# to compute CPU time
time0 = time.time()

# OS env
SYS = os.environ['SYS']

# Working directory
tmp_dir = os.environ['SCRATCH']

# CPO files
cpo_dir = os.path.abspath("../data/TESTS/")

# The path to the executable of ETS wrapper
ets_run = os.path.abspath("../bin/"+SYS+"/ets_test ")

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
print('Initialize Campaign object')
my_campaign = uq.Campaign(name = 'uq_ets', work_dir=tmp_dir)

# Copy XML files needed in the ETS wrappers
campaign_dir = my_campaign.campaign_dir
os.system("mkdir " + campaign_dir +"/workflows")
os.system("cp ../../workflows/ets.xml "+ campaign_dir +"/workflows")
os.system("cp ../../workflows/ets.xsd "+ campaign_dir +"/workflows")

# Copy input CPO files in common directory
common_dir = campaign_dir +"/common/"
os.system("mkdir " + common_dir)
os.system("cp " + cpo_dir + "/*.cpo " + common_dir)

# Create the encoder and get the app parameters
print('Create the encoder')
input_filename = "ets_coreprof_in.cpo"
encoder = cpo_template.CPOEncoder(template_filename=input_filename,
                                  template_cponame="coreprof",
                                  cpos_directory=common_dir,
                                  target_filename=input_filename,
                                  uncertain_params=uncertain_params
                                 )

params, vary = encoder.draw_app_params()

# Create the encoder
print('Create the encoder')
output_filename = "ets_coreprof.cpo"
decoder = cpo_template.CPODecoder(target_filename=output_filename,
                                  target_cponame="coreprof",
                                  output_columns=output_columns)

# Add the ETS app (automatically set as current app)
print('Add app to campagn object')
my_campaign.add_app(name="uq_ets",
                    params=params,
                    encoder=encoder,
                    decoder=decoder
                    )

# Create a collation element for this campaign
print('Create Collater')
collater = uq.collate.AggregateSamples(average=False)
my_campaign.set_collater(collater)

# Create the sampler
print('Create the sampler')
my_sampler = uq.sampling.PCESampler(vary=vary,
                                    polynomial_order=4,
                                    quadrature_rule='G',
                                    sparse=False
                                    )
my_campaign.set_sampler(my_sampler)

# Will draw all (of the finite set of samples)
print('Draw Samples')
my_campaign.draw_samples()

print('Populate runs_dir')
my_campaign.populate_runs_dir()

print('Executes ETS')
my_campaign.apply_for_each_run_dir(uq.actions.ExecuteLocal(ets_run))

print('Collate')
my_campaign.collate()

# Post-processing analysis
print('Post-processing analysis')
analysis = uq.analysis.PCEAnalysis(sampler=my_sampler, qoi_cols=output_columns)

my_campaign.apply_analysis(analysis)

results = my_campaign.get_last_analysis()

# Get Descriptive Statistics
print('Get Descriptive Statistics')
stats = results['statistical_moments']['Te']
pctl = results['percentiles']['Te']
sob_tot = results['sobols_total']['Te']

stats_ti = results['statistical_moments']['Ti']
pctl_ti = results['percentiles']['Ti']
s1st_ti = results['sobols_first']['Ti']
stot_ti = results['sobols_total']['Ti']


print('Ellapsed time: ', time.time() - time0)

#  Graphics for descriptive satatistics
print('PLOTS')
rho = corep.rho_tor
uncertain_params = ["Te_boundary", "Ti_boundary"]
plots.plot_stats_pctl(rho, stats, pctl,
                 xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'$Te$',
                 ftitle='Te profile',
                 fname='figs/te_ets_stats_sparse')

plots.plot_sobols(rho, sob_tot, uncertain_params,
                  ftitle=' Total-Order Sobol indices - QoI: Te',
                  fname='figs/te_ets_stot_sparse')

plots.plot_stats_pctl(rho, stats_ti, pctl_ti,
                 xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'$T_i [eV]$',
                 ftitle='Te profile',
                 fname='figs/ti_ets_stats')


plots.plot_sobols(rho, stot_ti, uncertain_params,
                  ftitle=' Total-Order Sobol indices - QoI: Ti',
                  fname='figs/ti_ets_st')

#print('TO database')
## To create new table for results and store them in the data base
#engine = my_campaign.campaign_db.engine
#stats_df = pd.DataFrame.from_dict(stats)
#stats_df.to_sql('STATSP', engine, if_exists='append')
#sob_1st_df = pd.dataframe.from_dict(sob_1st)
#sob_1st_df.to_sql('SOBOLS_1_P', engine, if_exists='append')
#sob_tot_df = pd.DataFrame.from_dict(sob_tot)
#sob_tot_df.to_sql('SOBOLS_TOT_P', engine, if_exists='append')

print('=== End of test_uq1_ets ===')
