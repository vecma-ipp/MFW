import os
import time
import easyvvuq as uq
from ascii_cpo import read
from mfw.templates.cpo_element import get_inputs
from mfw.templates.cpo_encoder import CPOEncoder
from mfw.templates.cpo_decoder import CPODecoder


'''
Perform UQ for the workflow Transport-Equilibrium-Turblence.
Uncertainties are driven by:
    Boundary conditions (Plasma Edge) of electrons and ions tempurature.
Method: Non intrusive with PCE.
'''

print('>>> test ETS: START')

time0 =  time.time()
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
    "te.boundary": {
        "type": "float",
        "dist": "Normal",
        "err":  0.25,
    }
    ,
    "ti.boundary": {
        "type": "float",
        "dist": "Normal",
        "err": 0.25,
    }
}
# CPO file containg initial values of uncertain params
input_filename = "ets_coreprof_in.cpo"
input_cponame = "coreprof"

# The quantities of intersts and the cpo file to set them
output_columns = ["Te", "Ti"]
output_filename = "ets_coreprof_out.cpo"
output_cponame = "coreprof"

# Parameter space for campaign and the distributions list for the Sampler
input_cpo_file = os.path.join(cpo_dir, input_filename)
params, vary = get_inputs(cpo_file = input_cpo_file,
                          cpo_name = input_cponame,
                          input_params = uncertain_params)

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
                     cpo_name = input_cponame,
                     common_dir=common_dir)

# Create the encoder
print('>>> Create the decoder')
decoder = CPODecoder(target_filename = output_filename,
                     output_columns = output_columns,
                     cpo_name = output_cponame)

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

test_case = cpo_dir.split('/')[-1]

#  Graphics for Descriptive satatistics
__PLOTS = True # If True create plots subfolder under outputs folder

if __PLOTS:
    from mfw.utils import plots

    uparams_names = list(params.keys())

    plots.plot_stats(rho, stat_te,
                     xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'$Te$',
                     ftitle='Te profile',
                     fname='outputs/Te_STAT_'+campaign_name)

    plots.plot_sobols_all(rho, sob1_te, uparams_names,
                      ftitle=' First-Order Sobol indices - QoI: Te',
                      fname='outputs/Te_SA_'+campaign_name)

    plots.plot_stats(rho, stat_ti,
                     xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'$T_i [eV]$',
                     ftitle='Ti profile',
                     fname='outputs/Ti_STAT_'+campaign_name)

    plots.plot_sobols_all(rho, sob1_ti, uparams_names,
                      ftitle=' First-Order Sobol indices - QoI: Ti',
                      fname='outputs/Ti_SA_'+campaign_name)

print('>>> test ETS : END')
