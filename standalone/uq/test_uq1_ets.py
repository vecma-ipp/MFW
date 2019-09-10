import os, sys
import time
import pandas as pd
import chaospy as cp
import easyvvuq as uq
import matplotlib.pylab as plt
from ascii_cpo import read
from tools import plots, cpo_template

'''
UQ test of ETS code using UQP1.
Uncertainties in:
- Initial conditions: Te and Ti edge boundaries (COREPROF)
- 4 flux tubes: D1, D2, D3, D4 (CORETRANSP).
'''

time0 = time.time()

# OS env
SYS = os.environ['SYS']

# Working directory
tmp_dir = os.environ['SCRATCH']

# CPO files
cpo_dir = os.path.abspath("../data/TESTS/")

# The path to the executable of ETS wrapper
ets_run = os.path.abspath("../bin/"+SYS+"/ets_test ")

uncertain_params = ["Te_boundary", "Ti_boundary"]

# Define parameter space
print('Define parameter space')
params = {
 "Te_boundary": {"type": "float", "default": 100.},
 "Ti_boundary": {"type": "float", "default": 100.}
}

#output_filename = params["out_file"]["default"]
output_columns = ["Te", 'Ti']

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

# Get uncertain parameters values
coret_file = common_dir + "ets_coretransp_in.cpo"
coret = read(coret_file, "coretransp")
diff_eff = coret.values[0].te_transp.diff_eff

corep_file = common_dir + "ets_coreprof_in.cpo"
corep = read(corep_file, "coreprof")
Te_boundary = corep.te.boundary.value[0]
Ti_boundary = corep.ti.boundary.value[0][0]

# Create an encoder and decoder
encoder = cpo_template.CPOEncoder(
    template_filename=corep_file,
    target_filename="ets_coreprof_in.cpo",
    cpo_type="coreprof")

decoder = uq.decoders.SimpleCSV(target_filename='out.csv',
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


# Create the sampler
print('Create the sampler')
vary={
    'Te_boundary': cp.Normal(Te_boundary, 0.2*Te_boundary),
    'Ti_boundary': cp.Normal(Ti_boundary, 0.2*Ti_boundary)
}


print('>>> call PCESampler')
time1 = time.time()
my_sampler = uq.sampling.PCESampler(vary=vary, polynomial_order=4,
                                    quadrature_rule='G', sparse=False)
print('>>> Samlper time =', time.time() - time1)

print('Number of samples: ', my_sampler._number_of_samples)

# Associate the sampler with the campaign
my_campaign.set_sampler(my_sampler)

# Will draw all (of the finite set of samples)
print('Draw Samples')
my_campaign.draw_samples()
print('populate_runs_dir ')
my_campaign.populate_runs_dir()
sys.exit()
my_campaign.apply_for_each_run_dir(uq.actions.ExecuteLocal(ets_run))
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
