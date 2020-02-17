import os
import time
import easyvvuq as uq
from ascii_cpo import read
from mfw.utils import xml_io, cpo_io
from mfw.templates.xml_encoder import XMLEncoder
from mfw.templates.cpo_encoder import CPOEncoder
from mfw.templates.cpo_decoder import CPODecoder
# GCG-PJ wrapper
import easypj
from easypj import TaskRequirements, Resources
from easypj import Task, TaskType, SubmitOrder


'''
Perform UQ for the workflow Transport-Equilibrium-Turblence.
Uncertainties are driven by:
    External sources of Electrons heating.
    Boundary conditions (Plasma Edge) of electrons tempurature.
(The same thing can be done for ions)
Method: Non intrusive (UQP1) with PCE.
Sampling and execution with QCG-Pilot Job
'''

print('>>> UQ-Workflow Combined (w/ PJ): START')

# OS env
SYS = os.environ['SYS']

# Working directory
tmp_dir = os.environ['SCRATCH']

# CPO files
cpo_dir = os.path.abspath("../workflows/AUG_28906_6")
#cpo_dir = os.path.abspath("../workflows/JET_92436_23066")

# XML and XSD files
xml_dir = os.path.abspath("../workflows")

# The execuatble model code
obj_dir = os.path.abspath("../standalone/bin/"+SYS)
exec_code = "loop_gem0"
exec_path = os.path.join(obj_dir, exec_code)

# Define the uncertain parameters
uncertain_params_bc = {
    # Electrons boudary condition
    "Te_boundary": {
        "type": "float",
        "distribution": "Normal",
        "margin_error": 0.2,
    }
}
uncertain_params_src = {
    # Gaussian Sources: Electrons heating
    "amplitude_el":{
        "type": "float",
        "distribution": "Uniform",
        "margin_error": 0.2,
    },
    "position_el":{
        "type": "float",
        "distribution": "Uniform",
        "margin_error": 0.2,
    },
    "width_el":{
        "type": "float",
        "distribution": "Uniform",
        "margin_error": 0.2,
    }
}

# CPO and XML file containg initial values of uncertain params
input_cpo_filename = "ets_coreprof_in.cpo"
input_xml_filename = "source_dummy.xml"

# The quantities of intersts and the cpo file to set them
output_columns = ["Te", "Ti"]
output_filename = "ets_coreprof_out.cpo"

# Parameter space for campaign and the distributions list for the Sampler
params_cpo, vary_cpo = cpo_io.get_inputs(dirname=cpo_dir, filename=input_cpo_filename,
                                 config_dict=uncertain_params_bc)
params_xml, vary_xml = xml_io.get_inputs(dirname=xml_dir, filename=input_xml_filename,
                                 config_dict=uncertain_params_src)

# Merge the dicts
params = {**params_cpo, **params_xml}
vary = {**vary_cpo, **vary_xml}

# Initialize Campaign object
print('>>> Initialize Campaign object')
campaign_name = "UQ_COMBINED_"
my_campaign = uq.Campaign(name=campaign_name, work_dir=tmp_dir)

# Create new directory for commons inputs
campaign_dir = my_campaign.campaign_dir
common_dir = campaign_dir +"/common/"
os.system("mkdir " + common_dir)

# Copy XML and XSD files
os.system("cp " + xml_dir + "/ets.x* "    + common_dir)
os.system("cp " + xml_dir + "/chease.x* " + common_dir)
os.system("cp " + xml_dir + "/gem0.x* "   + common_dir)
os.system("cp " + xml_dir + "/source_dummy.x* " + common_dir)

# Copy input CPO files in common directory
os.system("cp " + cpo_dir + "/ets_coreprof_in.cpo "    + common_dir)
os.system("cp " + cpo_dir + "/ets_equilibrium_in.cpo " + common_dir)
os.system("cp " + cpo_dir + "/ets_coreimpur_in.cpo "   + common_dir)
os.system("cp " + cpo_dir + "/ets_coretransp_in.cpo "  + common_dir)
os.system("cp " + cpo_dir + "/ets_toroidfield_in.cpo " + common_dir)

# Create the encoder and get the app parameters
print('>>> Create the encoders')
# params_names dict is given here because we will use MultiEncoder
encoder_cpo = CPOEncoder(template_filename=input_cpo_filename,
                         target_filename=input_cpo_filename,
                         common_dir=common_dir,
                         params_names=list(vary_cpo))

encoder_xml = XMLEncoder(template_filename=input_xml_filename,
                         target_filename=input_xml_filename,
                         common_dir=common_dir,
                         params_names=list(vary_xml))

# Combine both encoders into a single encoder
encoder = uq.encoders.MultiEncoder(encoder_cpo, encoder_xml)

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
                                    polynomial_order=4,
                                    regression=True)
my_campaign.set_sampler(my_sampler)

# Will draw all (of the finite set of samples)
print('>>> Draw Samples')
my_campaign.draw_samples()

print(">>> Starting PJ execution")
time0 = time.time()

qcgpjexec = easypj.Executor()
qcgpjexec.create_manager(dir=my_campaign.campaign_dir)

qcgpjexec.add_task(Task(
    TaskType.ENCODING,
    TaskRequirements(cores=Resources(exact=1))
))

qcgpjexec.add_task(Task(
    TaskType.EXECUTION,
    TaskRequirements(cores=Resources(exact=1)),
    application=exec_path
))

qcgpjexec.run(
    campaign=my_campaign,
    submit_order=SubmitOrder.RUN_ORIENTED
)

qcgpjexec.terminate_manager()

time1 = time.time()
print('>>> PJ Ellapsed time: ', time1 - time0)

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
stat_te = results['statistical_moments']['Te']
pctl_te = results['percentiles']['Te']
sob1_te = results['sobols_first']['Te']
sobt_te = results['sobols_total']['Te']

stat_ti = results['statistical_moments']['Ti']
pctl_ti = results['percentiles']['Ti']
sob1_ti = results['sobols_first']['Ti']
sobt_ti = results['sobols_total']['Ti']

#  Graphics for Descriptive satatistics
print('>>> Save Statictics and SA')
corep = read(os.path.join(cpo_dir,  "ets_coreprof_in.cpo"), "coreprof")
rho = corep.rho_tor_norm

test_case = cpo_dir.split('/')[-1]

# Plots STAT and SA
__PLOTS = True # If True create plots subfolder under outputs folder
if __PLOTS:
    from mfw.utils import plots
    uparams_names = list(params.keys())

    plots.plot_stats_pctl(rho, stat_te, pctl_te,
                     xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'$Te$',
                     ftitle='Te profile',
                     fname='outputs/Te_STAT_'+test_case+test_case)

    plots.plot_sobols(rho, sobt_te, uparams_names,
                      ftitle=' Total-Order Sobol indices - QoI: Te',
                      fname='outputs/Te_SA_'+test_case+test_case)

    plots.plot_stats_pctl(rho, stat_ti, pctl_ti,
                     xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'$T_i [eV]$',
                     ftitle='Te profile',
                     fname='outputs/Ti_STAT_'+test_case+test_case)

    plots.plot_sobols(rho, sobt_ti, uparams_names,
                      ftitle=' Total-Order Sobol indices - QoI: Ti',
                      fname='outputs/Ti_SA_'+test_case+test_case)

print('>>> UQ-Workflow Combined (w/ PJ): END')
