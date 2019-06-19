import os
import time
import numpy as np
import chaospy as cp
import easyvvuq as uq
import matplotlib.pylab as plt
from ascii_cpo import read
from tools import plots

'''
UQ test of ETS + CHEASE + BOHMGB using UQP1 (non intrusive case).
Uncertainties in initial conditions: Te and Ti boundaries (Eadge).
'''


# For the elapsed time
start_time = time.time()

# OS env
SYS = os.environ['SYS']

# Working directory
tmp_dir = os.environ['SCRATCH']

# CPO files
cpo_dir = os.path.abspath("../data/TESTS/")

# The exec code (ETS, CHEASE and BOHMGB wrappers)
run_exec = os.path.abspath("../bin/"+SYS+"/boundaries_run ")

# Uncertain parameters: Initial conditions
uparams = ["Te_boundary", "Ti_boundary"]

# Define parameter space
params = {
    uparams[0]: {
        "type": "real",
        "default": "113."},
    uparams[1]: {
        "type": "real",
        "default": "180."},
    "out_file": {
        "type": "str",
        "default": "output.csv"}}

output_filename = params["out_file"]["default"]
output_columns = ["te", "ti"]

# Initialize Campaign object
my_campaign = uq.Campaign(name = 'uq_boundaries', work_dir=tmp_dir)

# Copy XML files needed in the ETS, CHEASE and BOHMGB wrappers
campaign_dir = my_campaign.campaign_dir
os.system("mkdir " + campaign_dir +"/workflows")
os.system("cp ../../workflows/ets.xml "+ campaign_dir +"/workflows")
os.system("cp ../../workflows/ets.xsd "+ campaign_dir +"/workflows")
os.system("cp ../../workflows/chease.xml "+ campaign_dir +"/workflows")
os.system("cp ../../workflows/chease.xsd "+ campaign_dir +"/workflows")
os.system("cp ../../workflows/bohmgb.xml "+ campaign_dir +"/workflows")
os.system("cp ../../workflows/bohmgb.xsd "+ campaign_dir +"/workflows")

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

# Add the BC app (automatically set as current app)
my_campaign.add_app(name="boundaries",
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
    uparams[0]: cp.Normal(Te_boundary, 0.2*Te_boundary),
    uparams[1]: cp.Normal(Ti_boundary, 0.2*Ti_boundary)
}
my_sampler = uq.sampling.PCESampler(vary=vary, polynomial_order=3)

# Associate the sampler with the campaign
my_campaign.set_sampler(my_sampler)

# Will draw all (of the finite set of samples)
my_campaign.draw_samples()

my_campaign.populate_runs_dir()
cmd = run_exec + common_dir + " input.nml"

exec_time = time.time()
my_campaign.apply_for_each_run_dir(uq.actions.ExecuteLocal(cmd))
exec_time = time.time() - exec_time

my_campaign.collate()

# Post-processing analysis
analysis = uq.analysis.PCEAnalysis(sampler=my_sampler, qoi_cols=output_columns)

my_campaign.apply_analysis(analysis)

results = my_campaign.get_last_analysis()

# Get Descriptive Statistics
stats_te = results['statistical_moments']['te']
sobols_te = results['sobol_first_order']['te']

stats_ti = results['statistical_moments']['ti']
sobols_ti = results['sobol_first_order']['ti']

# Elapsed time
end_time = time.time()
print('======= Elapsed times')
print('- EXEC  : ', exec_time/60.)
print('- TOTAL : ', (end_time - start_time)/60.)

#  Graphics for descriptive satatistics
rho = corep.rho_tor

plots.plot_stats(rho, stats_te,
                 xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'$T_e [eV]$',
                 ftitle='Te profile',
                 fname='figs/te_stats_bc.png')

plots.plot_stats(rho, stats_ti,
                 xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'$T_i [eV]$',
                 ftitle='Ti profile',
                 fname='figs/ti_stats_bc.png')

plots.plot_sobols(rho, sobols_te, uparams,
                  ftitle=' First-Order Sobol indices - QoI: Te.',
                  fname='figs/te_sobols_bc.png')

plots.plot_sobols(rho, sobols_ti, uparams,
                  ftitle=' First-Order Sobol indices - QoI: Ti.',
                  fname='figs/ti_sobols_bc.png')

