import os
#import time
#import pandas as pd
import chaospy as cp
import easyvvuq as uq
import matplotlib.pylab as plt
from ascii_cpo import read
from tools import plots

'''
UQ test of ETS code using UQP1.
Uncertainties in:
- Initial conditions: Te and Ti edge boundaries (COREPROF)
- 4 flux tubes: D1, D2, D3, D4 (CORETRANSP).
'''


# OS env
SYS = os.environ['SYS']

# Working directory
tmp_dir = os.environ['SCRATCH']

# CPO files
cpo_dir = os.path.abspath("../data/TESTS/")

# The path to the executable of ETS wrapper
ets_run = os.path.abspath("../bin/"+SYS+"/ets_run ")

uncertain_params = ["D1", "D2", "D3", "D4", "Te_boundary", "Ti_boundary"]

# Define parameter space
print('Define parameter space')
params = {k: {"type": "float", "default": "0."} for k in uncertain_params}
params.update({"out_file": {"type": "string", "default": "output.csv"}})

output_filename = params["out_file"]["default"]
output_columns = ["Te", "Ti", 'Ne']

# Initialize Campaign object
print('Initialize Campaign object')
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
coret_file = common_dir + "ets_coretransp_in.cpo"
coret = read(coret_file, "coretransp")
diff_eff = coret.values[0].te_transp.diff_eff

corep_file = common_dir + "ets_coreprof_in.cpo"
corep = read(corep_file, "coreprof")
Te_boundary = corep.te.boundary.value[0]
Ti_boundary = corep.ti.boundary.value[0][0]

# Create the sampler
print('Create the sampler')
vary = {uncertain_params[k]: cp.Normal(diff_eff[k], 0.2*diff_eff[k]) for k in range(4)}
vary.update({
    uncertain_params[4]: cp.Normal(Te_boundary, 0.2*Te_boundary),
    uncertain_params[5]: cp.Normal(Ti_boundary, 0.2*Ti_boundary)
})

my_sampler = uq.sampling.PCESampler(vary=vary, polynomial_order=2)

# Associate the sampler with the campaign
my_campaign.set_sampler(my_sampler)

# Will draw all (of the finite set of samples)
print('Draw Samples and run the code')
my_campaign.draw_samples()
my_campaign.populate_runs_dir()
my_campaign.apply_for_each_run_dir(uq.actions.ExecuteLocal(ets_run + " input.nml"))
my_campaign.collate()

# Post-processing analysis
print('Post-processing analysis')
analysis = uq.analysis.PCEAnalysis(sampler=my_sampler, qoi_cols=output_columns)

my_campaign.apply_analysis(analysis)

results = my_campaign.get_last_analysis()

# Get Descriptive Statistics
print('Get Descriptive Statistics')
stats_te = results['statistical_moments']['Te']
pctl_te = results['percentiles']['Te']
s1st_te = results['sobols_first']['Te']
stot_te = results['sobols_total']['Te']

stats_ti = results['statistical_moments']['Ti']
pctl_ti = results['percentiles']['Ti']
s1st_ti = results['sobols_first']['Ti']
stot_ti = results['sobols_total']['Ti']

stats_ne = results['statistical_moments']['Ne']
pctl_ne = results['percentiles']['Ne']
s1st_ne = results['sobols_first']['Ne']
stot_ne = results['sobols_total']['Ne']

# To create new table for results and store them in the data base
#engine = my_campaign.campaign_db.engine
#statte_df = pd.DataFrame.from_dict(stats)
#statte_df.to_sql('STATS_TE', engine, if_exists='append')
#sob1_df = pd.DataFrame.from_dict(s1st_te)
#sob_df.to_sql('SOBOLS_TE', engine, if_exists='append')

#  Graphics for descriptive satatistics
print('PLOTS')
rho = corep.rho_tor

# Te
plots.plot_stats_pctl(rho, stats_te, pctl_te,
                 xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'$T_e [eV]$',
                 ftitle='Te profile',
                 fname='figs/te_ets_stats')

plots.plot_sobols(rho, s1st_te, uncertain_params,
                  ftitle=' First-Order Sobol indices - QoI: Te',
                  fname='figs/te_ets_s1')

plots.plot_sobols(rho, stot_te, uncertain_params,
                  ftitle=' Total-Order Sobol indices - QoI: Te',
                  fname='figs/te_ets_st')

# Ne
plots.plot_stats_pctl(rho, stats_ne, pctl_ne,
                 xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'$Ne$',
                 ftitle='Ne profile',
                 fname='figs/ne_ets_stats')

plots.plot_sobols(rho, s1st_ne, uncertain_params,
                  ftitle=' First-Order Sobol indices - QoI: Ne',
                  fname='figs/ne_ets_s1')

plots.plot_sobols(rho, stot_ne, uncertain_params,
                  ftitle=' Total-Order Sobol indices - QoI: Ne',
                  fname='figs/ne_ets_st')

# Ti
plots.plot_stats_pctl(rho, stats_ti, pctl_ti,
                 xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'$T_i [eV]$',
                 ftitle='Te profile',
                 fname='figs/ti_ets_stats')
plots.plot_sobols(rho, s1st_ti, uncertain_params,
                  ftitle=' First-Order Sobol indices - QoI: Ti',
                  fname='figs/ti_ets_s1')

plots.plot_sobols(rho, stot_ti, uncertain_params,
                  ftitle=' Total-Order Sobol indices - QoI: Ti',
                  fname='figs/ti_ets_st')

print('=== End of test_uq1_ets ===')
