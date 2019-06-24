import os
import time
import numpy as np
import chaospy as cp
import easyvvuq as uq
import matplotlib.pylab as plt
from ascii_cpo import read
from tools import plots

'''
UQ test of ETS using 4 flux tubes. Parameters: D1, D2, D3, D4.
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
run_exec = os.path.abspath("../bin/"+SYS+"/ets_run ")

# Uncertain parameters: 4 flux tubes positions
uparams = ["D1", "D2", "D3", "D4"]

# Define parameter space
params = {
    uparams[0]: {
        "type": "real",
        "default": "0."},
    uparams[1]: {
        "type": "real",
        "default": "0."},
    uparams[2]: {
        "type": "real",
        "default": "0."},
    uparams[3]: {
        "type": "real",
        "default": "0."},
    "out_file": {
        "type": "str",
        "default": "output.csv"}}

output_filename = params["out_file"]["default"]
output_columns = ["te"]

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
    template_fname='inputs/ets.template',
    delimiter='#',
    target_filename='input.nml')
decoder = uq.decoders.SimpleCSV(target_filename=output_filename,
                                output_columns=output_columns,
                                header=0)

# Add the BC app (automatically set as current app)
my_campaign.add_app(name="ets",
                    params=params,
                    encoder=encoder,
                    decoder=decoder
                    )

# Create a collation element for this campaign
collater = uq.collate.AggregateSamples(average=False)
my_campaign.set_collater(collater)

# Get uncertain parameters values
coret_file = common_dir + "ets_coretransp_in.cpo"
coret = read(coret_file, "coretransp")
diff_eff = coret.values[0].te_transp.diff_eff

# Create the sampler
vary = { uparams[k]: cp.Normal(diff_eff[k], 0.2*diff_eff[k]) for k in range(4)}
my_sampler = uq.sampling.QMCSampler(vary=vary, number_of_samples=1000)

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
analysis = uq.analysis.QMCAnalysis(sampler=my_sampler, qoi_cols=output_columns)

my_campaign.apply_analysis(analysis)

results = my_campaign.get_last_analysis()

# Get Descriptive Statistics
stats = results['statistical_moments']['te']
pctl = results['percentiles']['te']

# Elapsed time
end_time = time.time()
print('======= Elapsed times')
print('- EXEC  : ', exec_time/60.)
print('- TOTAL : ', (end_time - start_time)/60.)

#  Graphics for descriptive satatistics
corep_file = common_dir + '/ets_coreprof_in.cpo'
corep = read(corep_file, 'coreprof')
rho = corep.rho_tor

plots.plot_stats_pctl(rho, stats, pctl,
                 xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'$T_e [eV]$',
                 ftitle='Te profile (QMC)',
                 fname='figs/te_ets_stats-QMC')


