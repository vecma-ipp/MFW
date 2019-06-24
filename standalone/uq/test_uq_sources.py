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
Uncertainties in sources.
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
run_exec = os.path.abspath("../bin/"+SYS+"/sources_run ")

# Uncertain parameters: Gaussian Sources
uparams =['amp_e', 'pos_e', 'width_e', 'amp_i', 'pos_i', 'width_i']

# Define parameter space
params = {
    uparams[0]: {
        "type": "real",
        "default": "1.5e6"},
    uparams[1]: {
        "type": "real",
        "default": "0."},
    uparams[2]: {
        "type": "real",
        "default": "0.2"},
    uparams[3]: {
        "type": "real",
        "default": "1.5e6."},
    uparams[4]: {
        "type": "real",
        "default": "0."},
    uparams[5]: {
        "type": "real",
        "default": "0.2"},
    "out_file": {
        "type": "str",
        "default": "output.csv"}}

output_filename = params["out_file"]["default"]
output_columns = ["te", "ti"]

# Initialize Campaign object
my_campaign = uq.Campaign(name = 'uq_sources', work_dir=tmp_dir)

# Copy XML files needed in the ETS, CHEASE and BOHMGB codes
campaign_dir = my_campaign.campaign_dir
os.system("mkdir " + campaign_dir +"/workflows")
os.system("cp ../../workflows/ets.xml "+ campaign_dir +"/workflows")
os.system("cp ../../workflows/ets.xsd "+ campaign_dir +"/workflows")
os.system("cp ../../workflows/chease.xml "+ campaign_dir +"/workflows")
os.system("cp ../../workflows/chease.xsd "+ campaign_dir +"/workflows")
os.system("cp ../../workflows/bohmgb.xml "+ campaign_dir +"/workflows")
os.system("cp ../../workflows/bohmgb.xsd "+ campaign_dir +"/workflows")
os.system("cp ../../workflows/source_dummy_new.xml "+ campaign_dir +"/workflows/source_dummy.xml")
os.system("cp ../../workflows/source_dummy.xsd "+ campaign_dir +"/workflows")

# Copy CPO files in common directory
common_dir = campaign_dir +"/common/"
os.system("mkdir " + common_dir)
os.system("cp " + cpo_dir + "/*.cpo " + common_dir)

# Create an encoder and decoder
encoder = uq.encoders.GenericEncoder(
    template_fname='inputs/sources.template',
    delimiter='#',
    target_filename='input.nml')
decoder = uq.decoders.SimpleCSV(target_filename=output_filename,
                                output_columns=output_columns,
                                header=0)

# Add the BC app (automatically set as current app)
my_campaign.add_app(name="sources",
                    params=params,
                    encoder=encoder,
                    decoder=decoder
                    )

# Create a collation element for this campaign
collater = uq.collate.AggregateSamples(average=False)
my_campaign.set_collater(collater)

# Create the sampler
vary = {
    uparams[0]: cp.Uniform(0.9*1.5e6, 1.1*1.5e6),
    uparams[1]: cp.Uniform(0.0,  0.2),
    uparams[2]: cp.Uniform(0.18, 0.22),
    uparams[3]: cp.Uniform(0.9*1.5e6, 1.1*1.5e6),
    uparams[4]: cp.Uniform(0.0,  0.2),
    uparams[5]: cp.Uniform(0.18, 0.22)
}

#my_sampler = uq.sampling.PCESampler(vary=vary, polynomial_order=3)
my_sampler = uq.sampling.QMCSampler(vary=vary, number_of_samples=10000)

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
#analysis = uq.analysis.PCEAnalysis(sampler=my_sampler, qoi_cols=output_columns)
analysis = uq.analysis.QMCAnalysis(sampler=my_sampler, qoi_cols=output_columns)

my_campaign.apply_analysis(analysis)

results = my_campaign.get_last_analysis()

# Get Descriptive Statistics
stats_te = results['statistical_moments']['te']
pctl_te = results['percentiles']['te']
#s1_te = results['sobol_first_order']['te']
#st_te = results['sobol_total_order']['te']

stats_ti = results['statistical_moments']['ti']
pctl_ti = results['percentiles']['ti']
#s1_ti = results['sobol_first_order']['ti']
#st_ti = results['sobol_total_order']['ti']


# Elapsed time
end_time = time.time()
print('======= Elapsed times')
print('- EXEC  : ', exec_time/60.)
print('- TOTAL : ', (end_time - start_time)/60.)

#  Graphics for descriptive satatistics
corep_file = common_dir + '/ets_coreprof_in.cpo'
corep = read(corep_file, 'coreprof')
rho = corep.rho_tor

plots.plot_stats_pctl(rho, stats_te, pctl_te,
                 xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'$T_e ~ [eV]$',
                 ftitle='UQ: Te profile (Uncertenties in Ion sources)',
                 fname='figs/te_sr_ions-stats.png')

plots.plot_stats_pctl(rho, stats_ti, pctl_ti,
                 xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'$T_i ~ [eV]$',
                 ftitle='UQ: Ti profile (Uncertenties in Ion sources)',
                 fname='figs/ti_sr_ions-stats.png')

#plots.plot_sobols(rho, s1_te, uparams,
#              ftitle='1st Sobol indices. QoI: Te, Uncertenties in ion sources',
#              fname='figs/te_src-first_sobols.png')
#
#plots.plot_sobols(rho, s1_ti, uparams,
#              ftitle='1st Sobol indices. QoI: Ti, Uncertenties in ion sources',
#              fname='figs/ti_srs-first_sobols.png')
#
#plots.plot_sobols(rho, st_te, uparams,
#              ftitle='Total Sobol indices. QoI: Te, Uncertenties in ion sources',
#              fname='figs/te_src-total_sobols.png')
#
#plots.plot_sobols(rho, st_ti, uparams,
#              ftitle='Total Sobol indices. QoI: Ti, Uncertenties in ion sources',
#              fname='figs/ti_src-total_sobols.png')
