import os
import sys
import easyvvuq as uq
from ascii_cpo import read
from utils import cpo_io
from templates.cpo_encoder import CPOEncoder
from templates.cpo_decoder import CPODecoder


'''
Perform UQ for the workflow Transport-Equilibrium-Turblence.
Uncertainties are driven by:
    Boundary conditions (Plasma Edge) of electrons and ions tempurature.
Method: Non intrusive with PCE.
'''

print('>>> test ETS: START')

# OS env
SYS = os.environ['SYS']

# Working directory
tmp_dir = os.environ['SCRATCH']

# CPO files
cpo_dir = os.path.abspath("../workflows/AUG_28906_6")

# XML and XSD files
xml_dir = os.path.abspath("../workflows")

# The executable code to run
obj_dir = os.path.abspath("../standalone/bin/"+SYS)
exec_code = "ets_test"

# Define the uncertain parameters
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
# CPO file containg initial values of uncertain params
input_filename = "ets_coreprof_in.cpo"

# The quantities of intersts and the cpo file to set them
output_columns = ["Te", "Ti"]
output_filename = "ets_coreprof_out.cpo"

# Parameter space for campaign and the distributions list for the Sampler
params, vary = cpo_io.get_inputs(dirname=cpo_dir, filename=input_filename,
                                 config_dict=uncertain_params)

# Initialize Campaign object
print('>>> Initialize Campaign object')
campaign_name = "UQ_ETS_"
my_campaign = uq.Campaign(name=campaign_name, work_dir=tmp_dir)

# Create new directory for inputs (to be ended with /)
campaign_dir = my_campaign.campaign_dir
common_dir = campaign_dir +"/common/"
os.system("mkdir " + common_dir)

# Copy input CPO files (cf test_ets.f90)
os.system("cp " + cpo_dir + "/*.cpo " + common_dir)

# Copy XML and XSD files
os.system("cp " + xml_dir + "/ets.xml " + common_dir)
os.system("cp " + xml_dir + "/ets.xsd " + common_dir)

# Create the encoder and get the app parameters
print('>>> Create the encoder')
encoder = CPOEncoder(template_filename=input_filename,
                     target_filename=input_filename,
                     common_dir=common_dir)

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
my_sampler = uq.sampling.PCESampler(vary=vary,
                                    polynomial_order=3,
                                    quadrature_rule='G',
                                    sparse=False)
my_campaign.set_sampler(my_sampler)

# Will draw all (of the finite set of samples)
print('>>> Draw Samples')
my_campaign.draw_samples()

print('>>> Populate runs_dir')
my_campaign.populate_runs_dir()

print('>>> Execute ETS code')
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

print('>>> Ellapsed time: ', time.time() - time0)

# Get Descriptive Statistics
print('>>> Get Descriptive Statistics')
stat_te = results['statistical_moments']['Te']
sob1_te = results['sobols_first']['Te']
stat_ti = results['statistical_moments']['Ti']
sob1_ti = results['sobols_first']['Ti']

corep = read(os.path.join(cpo_dir,  "ets_coreprof_in.cpo"), "coreprof")
rho = corep.rho_tor_norm

#  Graphics for Descriptive satatistics
__PLOTS = True # If True create plots subfolder under outputs folder

if __PLOTS:
    from utils import plots

    uparams_names = list(params.keys())

    plots.plot_stats(rho, stat_te,
                     xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'$Te$',
                     ftitle='Te profile',
                     fname='outputs/plots/Te_STAT_'+test_case+test_case)

    plots.plot_sobols_all(rho, sob1_te, uparams_names,
                      ftitle=' First-Order Sobol indices - QoI: Te',
                      fname='outputs/plots/Te_SA_'+test_case+test_case)

    plots.plot_stats(rho, stat_ti,
                     xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'$T_i [eV]$',
                     ftitle='Ti profile',
                     fname='outputs/plots/Ti_STAT_'+test_case+test_case)

    plots.plot_sobols_all(rho, sob1_ti, uparams_names,
                      ftitle=' First-Order Sobol indices - QoI: Ti',
                      fname='outputs/plots/Ti_SA_'+test_case+test_case)

print('>>> test ETS : END')
