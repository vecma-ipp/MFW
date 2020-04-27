import os
import easyvvuq as uq
from easymfw.templates.cpo_encoder import CPOEncoder
from easymfw.templates.cpo_decoder import CPODecoder
from easymfw.utils.io_tools import get_cpo_inputs


'''
Perform UQ for the workflow Transport-Equilibrium-Turblence.
Uncertainties are driven by:
    Boundary conditions (Plasma Edge) of electrons and ions tempurature.
Method: Non intrusive (UQP1) with PCE.
'''

print('>>> UQ-Workflow BC: START')

# OS env
SYS = os.environ['SYS']

# Working directory
tmp_dir = os.environ['SCRATCH']

# CPO files (and restart data)
cpo_dir = os.path.abspath("../workflows/AUG_28906_6")
#cpo_dir = os.path.abspath("../workflows/JET_92436_23066")

test_case = cpo_dir.split('/')[-1]

# XML and XSD files
xml_dir = os.path.abspath("../workflows")

# The executable code to run
obj_dir = os.path.abspath("../standalone/bin/"+SYS)
exec_code = "loop_gem0"

# Define the uncertain parameters
uncertain_params = {
    "te.boundary": {
        "dist": "Normal",
        "err":  0.25,
    },
    "ti.boundary": {
        "dist": "Normal",
        "err": 0.25,
    }
}

# CPO file containg initial values of uncertain params
input_filename = "ets_coreprof_in.cpo"
input_cponame = "coreprof"

# The quantities of intersts and the cpo file to set them
output_columns = ["te", "ti"]
output_filename = "ets_coreprof_out.cpo"
output_cponame = "coreprof"

# params: the parameter space for campaign object
# vary: adistributions list for the sampler
print('>>> Get input parmeters')
input_cpo_file = os.path.join(cpo_dir, input_filename)
params, vary = get_cpo_inputs(cpo_file = input_cpo_file,
                              cpo_name = input_cponame,
                              input_params = uncertain_params)

# Initialize Campaign object
print('>>> Initialize Campaign object')
campaign_name = "UQBC_"+test_case
my_campaign = uq.Campaign(name=campaign_name, work_dir=tmp_dir)

# Create new directory for inputs
campaign_dir = my_campaign.campaign_dir
common_dir = campaign_dir +"/common/"
os.mkdir(common_dir)

# Copy input CPO files
os.system("cp " + cpo_dir + "/*.cpo " + common_dir)

# Copy XML and XSD files
os.system("cp " + xml_dir + "/ets.xsd "    + common_dir)
os.system("cp " + xml_dir + "/ets.xsd "    + common_dir)
os.system("cp " + xml_dir + "/chease.xml " + common_dir)
os.system("cp " + xml_dir + "/chease.xsd " + common_dir)
os.system("cp " + xml_dir + "/gem0.xml "   + common_dir)
os.system("cp " + xml_dir + "/gem0.xsd "   + common_dir)

# Copy exec file
os.system("cp " + exec_code + " " + common_dir)

# Create the encoder and get the app parameters
print('>>> Create the encoder')
decoder = CPODecoder(target_filename = output_filename,
                     output_columns = output_columns,
                     output_cponame = output_cponame)

# Create the encoder
print('>>> Create the decoder')
decoder = CPODecoder(target_filename=output_filename,
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
my_sampler = uq.sampling.PCESampler(vary=vary, polynomial_order=3)
my_campaign.set_sampler(my_sampler)

# Will draw all (of the finite set of samples)
print('>>> Draw Samples')
my_campaign.draw_samples()

print('>>> Populate runs_dir')
my_campaign.populate_runs_dir()

print('>>> Execute the workflow code')
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

# Get Descriptive Statistics
print('>>> Get Descriptive Statistics')
stat = {}
sob1 = {}
dist = {}
for qoi in output_columns:
    stat[qoi] = results['statistical_moments'][qoi]
    sob1[qoi] = results['sobols_first'][qoi]
    dist[qoi] = results['output_distributions'][qoi]

# Graphics Descriptive satatistics
from easymfw.utils import plots
from ascii_cpo import read

corep = read(os.path.join(cpo_dir,  "ets_coreprof_in.cpo"), "coreprof")
rho = corep.rho_tor_norm


# Save STAST into the csv file
__SAVE_CSV = True
if __SAVE_CSV:
    import numpy as np

    mean_te = list(stat_te['mean'])
    std_te  = list(stat_te['std'])
    mean_ti = list(stat_ti['mean'])
    std_ti  = list(stat_ti['std'])

    header = 'RHO_TOR_NORM\tMEAN_TE\tSTD_TE\tMEAN_TI\tSTD_TI'
    np.savetxt('outputs/'+test_case+test_case+'STATS.csv',
               np.c_[rho, mean_te, std_te, mean_ti, std_ti],
               delimiter='\t', comments='', header=header)

# Save STAST and SA into the database
__SAVE_DB = False
if __SAVE_DB:
    import pandas as pd

    # Save statistics and Sobol indices into the database
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

# Plots STAT and SA
__PLOTS = True # If True create plots subfolder under outputs folder
if __PLOTS:
    from mfw.utils import plots
    uparams_names = list(params.keys())

    plots.plot_stats_pctl(rho, stat_te, pctl_te,
                     xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'$Te$',
                     ftitle='Te profile',
                     fname='outputs/plots/Te_STAT_'+test_case+test_case)

    plots.plot_sobols(rho, sobt_te, uparams_names,
                      ftitle='Total-Order Sobol indices - QoI: Te',
                      fname='outputs/plots/Te_SA_'+test_case+test_case)

    plots.plot_stats_pctl(rho, stat_ti, pctl_ti,
                     xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'$T_i [eV]$',
                     ftitle='Te profile',
                     fname='outputs/plots/Ti_STAT_'+test_case+test_case)

    plots.plot_sobols(rho, sobt_ti, uparams_names,
                      ftitle='Total-Order Sobol indices - QoI: Ti',
                      fname='outputs/plots/Ti_SA_'+test_case+test_case)

print('>>> UQ-Workflow BC: END')
