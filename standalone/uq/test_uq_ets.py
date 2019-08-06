import os
import time
import numpy as npp
import pandas as pd
import chaospy as cp
import easyvvuq as uq
import matplotlib.pylab as plt
from ascii_cpo import read
from tools import plots

'''
UQ test of ETS.
Uncertainties in initial conditions: Te and Ti boundaries (Edge).
'''


# OS env
SYS = os.environ['SYS']

# Working directory
tmp_dir = os.environ['SCRATCH']

# CPO files
cpo_dir = os.path.abspath("../data/TESTS/")

# The path to the executable of ETS wrapper
ets_run = os.path.abspath("../bin/"+SYS+"/ets_run ")

uncertain_params = ["Te_boundary", "Ti_boundary"]

# Define parameter space
params = {
    uncertain_params[0]: {
        "type": "float",
        "default": "113."
    },
    uncertain_params[1]: {
        "type": "float",
        "default": "180."
    },
    "out_file": {
        "type": "string",
        "default": "output.csv"
    }
}
output_filename = params["out_file"]["default"]
output_columns = ["Te", "Ti"]

# Initialize Campaign object
my_campaign = uq.Campaign(name = 'uq_ets', work_dir=tmp_dir)

# Copy XML files needed in the ETS wrappers
campaign_dir = my_campaign.campaign_dir
os.system("mkdir " + campaign_dir +"/workflows")
os.system("cp ../../workflows/ets.xml "+ campaign_dir +"/workflows")
os.system("cp ../../workflows/ets.xsd "+ campaign_dir +"/workflows")

# Copy CPO files in common directory
common_dir = campaign_dir +"/common/"
os.system("mkdir " + common_dir)
os.system("cp " + cpo_dir + "/*.cpo " + common_dir)

# Create an encoder and decoder
encoder = uq.encoders.GenericEncoder(
    template_fname='inputs/boundaries.template',
    delimiter='#',
    target_filename='input.nml')
decoder = uq.decoders.SimpleCSV(target_filename=output_filename,
                                output_columns=output_columns,
                                header=0)

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
corep_file = common_dir + "ets_coreprof_in.cpo"
corep = read(corep_file, "coreprof")
Te_boundary = corep.te.boundary.value[0]
Ti_boundary = corep.ti.boundary.value[0][0]

# Create the sampler
vary = {
    uncertain_params[0]: cp.Normal(Te_boundary, 0.2*Te_boundary),
    uncertain_params[1]: cp.Normal(Ti_boundary, 0.2*Ti_boundary)
}
my_sampler = uq.sampling.PCESampler(vary=vary, polynomial_order=4)

# Associate the sampler with the campaign
my_campaign.set_sampler(my_sampler)

# Will draw all (of the finite set of samples)
my_campaign.draw_samples()
my_campaign.populate_runs_dir()
my_campaign.apply_for_each_run_dir(uq.actions.ExecuteLocal(ets_run + " input.nml"))
my_campaign.collate()

# Post-processing analysis
analysis = uq.analysis.PCEAnalysis(sampler=my_sampler, qoi_cols=output_columns)

my_campaign.apply_analysis(analysis)

results = my_campaign.get_last_analysis()

# Get Descriptive Statistics
stats_te = results['statistical_moments']['Te']
pctl_te = results['percentiles']['Te']
sobols_te = results['sobols_first']['Te']

# To create new table for results and store them in the data base
#engine = my_campaign.campaign_db.engine
#stat_df = pd.DataFrame.from_dict(stats)
#stat_df.to_sql('STATS', engine, if_exists='append')
#sob_df = pd.DataFrame.from_dict(sob1)
#sob_df.to_sql('SOBOLS', engine, if_exists='append')

#  Graphics for descriptive satatistics
rho = corep.rho_tor
plots.plot_stats_pctl(rho, stats_te, pctl_te,
                 xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'$T_e [eV]$',
                 ftitle='Te profile',
                 fname='figs/te_ets_stats')

plots.plot_sobols_all(rho, sobols_te, uncertain_params,
                  ftitle=' First-Order Sobol indices - QoI: Te',
                  fname='figs/te_ets_sobols')
