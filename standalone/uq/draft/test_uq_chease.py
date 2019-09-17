import os
import time
import numpy as np
import chaospy as cp
import easyvvuq as uq
import matplotlib.pylab as plt
from ascii_cpo import read
from tools import plots

'''
UQ test of ETS + CHEASE using the following uncertain parameters:
    - 4 flux tubes: D1, D2, D3, D4.
    - Initial conditions: Te_boundary and Ti_boundary.
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
ets_chease_run = os.path.abspath("../bin/"+SYS+"/ets_chease_run ")

# Uncertain parameters:
#uparams = ["D1", "D2", "D3", "D4"]
uparams = ["Te_boundary", "Ti_boundary"]

# Define parameter space
params = {
    uparams[0]: {
        "type": "float",
        "default": "0."},
    uparams[1]: {
        "type": "float",
        "default": "0."},
    uparams[2]: {
        "type": "float",
        "default": "0."},
    uparams[3]: {
        "type": "float",
        "default": "0."},
    "out_file": {
        "type": "string",
        "default": "output.csv"}}

output_filename = params["out_file"]["default"]
output_columns = ["te", "ne", "q"]

# Initialize Campaign object
my_campaign = uq.Campaign(name = 'uq_chease', work_dir=tmp_dir)

# Copy XML files needed in the ETS wrappers
campaign_dir = my_campaign.campaign_dir
os.system("mkdir " + campaign_dir +"/workflows")
os.system("cp ../../workflows/ets.xml "+ campaign_dir +"/workflows")
os.system("cp ../../workflows/ets.xsd "+ campaign_dir +"/workflows")
os.system("cp ../../workflows/chease.xml "+ campaign_dir +"/workflows")
os.system("cp ../../workflows/chease.xsd "+ campaign_dir +"/workflows")

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
my_campaign.add_app(name="uq_ets_chease",
                    params=params,
                    encoder=encoder,
                    decoder=decoder
                    )

# Create a collation element for this campaign
collater = uq.collate.AggregateSamples(average=False)
my_campaign.set_collater(collater)

# Get uncertain parameters values
#coret_file = common_dir + "ets_coretransp_in.cpo"
#coret = read(coret_file, "coretransp")
#diff_eff = coret.values[0].te_transp.diff_eff
corep_file = common_dir + "ets_coreprof_in.cpo"
corep = read(corep_file, "coreprof")
Te_boundary = corep.te.boundary.value[0]
Ti_boundary = corep.ti.boundary.value[0][0]

# Create the sampler
#vary = { uparams[k]: cp.Normal(diff_eff[k], 0.2*diff_eff[k]) for k in range(4)}
vary = {
    uparams[0]: cp.Normal(Te_boundary, 0.2*Te_boundary),
    uparams[1]: cp.Normal(Ti_boundary, 0.2*Ti_boundary)
}

my_sampler = uq.sampling.PCESampler(vary=vary, polynomial_order=4)

# Associate the sampler with the campaign
my_campaign.set_sampler(my_sampler)

# Will draw all (of the finite set of samples)
my_campaign.draw_samples()

my_campaign.populate_runs_dir()

exec_time = time.time()
my_campaign.apply_for_each_run_dir(uq.actions.ExecuteLocal(ets_chease_run + " input.nml"))
exec_time = time.time() - exec_time

my_campaign.collate()

# Post-processing analysis
analysis = uq.analysis.PCEAnalysis(sampler=my_sampler, qoi_cols=output_columns)
my_campaign.apply_analysis(analysis)

results = my_campaign.get_last_analysis()

# Elapsed time
end_time = time.time()
print('======= Elapsed times')
print('- EXEC  : ', exec_time/60.)
print('- TOTAL : ', (end_time - start_time)/60.)

# Get Descriptive Statistics
stats_q1 = results['statistical_moments']['te']
pctl_q1 = results['percentiles']['te']
s1_q1 = results['sobols_first']['te']

stats_q2 = results['statistical_moments']['ne']
pctl_q2 = results['percentiles']['ne']
s1_q2 = results['sobols_first']['ne']

stats_q3 = results['statistical_moments']['q']
pctl_q3 = results['percentiles']['q']
s1_q3 = results['sobols_first']['q']


#  Graphics for descriptive satatistics
#eq_file = cpo_dir + "/ets_equilibrium_in.cpo"
#eq = read(eq_file, "equilibrium")
#rho = eq.profiles_1d.rho_tor

rho = corep.rho_tor

plots.plot_stats_pctl(rho, stats_q1, pctl_q1,
                 xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'te',
                 ftitle='Te Profile (IC)',
                 fname='figs/te_stats_ic.png')
plots.plot_sobols_all(rho, s1_q1, uparams,
                  ftitle=' First-Order Sobol indices - QoI: Te',
                  fname='figs/te_sobols_ic.png')

plots.plot_stats_pctl(rho, stats_q2, pctl_q2,
                 xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'ne',
                 ftitle='Ne Profile (IC)',
                 fname='figs/ne_stats_ic.png')
plots.plot_sobols_all(rho, s1_q2, uparams,
                  ftitle=' First-Order Sobol indices - QoI: Ne',
                  fname='figs/ne_sobols_ic.png')

plots.plot_stats_pctl(rho, stats_q3, pctl_q3,
                 xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'q',
                 ftitle='Q Profile (ic)',
                 fname='figs/q_stats_ic.png')
plots.plot_sobols_all(rho, s1_q3, uparams,
                  ftitle=' First-Order Sobol indices - QoI: Q',
                  fname='figs/q_sobols_ic.png')
