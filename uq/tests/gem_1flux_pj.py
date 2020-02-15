import os
import time

import easyvvuq as uq

from ascii_cpo import read

from mfw.utils import cpo_io
from mfw.templates.cpo_encoder import CPOEncoder
from mfw.templates.cpo_decoder import CPODecoder

import easypj
from easypj import TaskRequirements, Resources
from easypj import Task, TaskType, SubmitOrder


'''
Perform UQ for the Turblence code GEM (using QCG-Pilot Job)
Uncertainties are driven by:
    The electon and ion temperatur and their gradient localisd by a Flux tube position
IMPORTANT CHECK: in gem.xml, nrho_transp = 1
'''


print('TEST GEM UQ with PilotJob: START')

# OS env
SYS = os.environ['SYS']
mpi_instance =  os.environ['MPICMD']

# Working directory
tmp_dir = os.environ['SCRATCH']


# CPO files
cpo_dir = os.path.abspath("../workflows/JET_92436_23066")

# XML and XSD files
xml_dir = os.path.abspath("../workflows")

# The executable code to run
obj_dir = os.path.abspath("../standalone/bin/"+SYS)
exec_code = "gem_test"
exec_path = os.path.join(obj_dir, exec_code)

# Define the uncertain parameters
uncertain_params = {
    "Te": {
        "type": "float",
        "distribution": "Normal",
        "margin_error": 0.2,
    },
    "Ti": {
        "type": "float",
        "distribution": "Normal",
        "margin_error": 0.2,
    },
    "Te_grad": {
        "type": "float",
        "distribution": "Normal",
        "margin_error": 0.2,
    },
    "Ti_grad": {
        "type": "float",
        "distribution": "Normal",
        "margin_error": 0.2,
    }
}
# CPO file containg initial values of uncertain params
input_filename = "gem_coreprof_in.cpo"

# We test 1 flux tube.
flux_index = 69

# The quantities of intersts and the cpo file to set them
output_columns = ["Te_transp_flux", "Ti_transp_flux"]
output_filename = "gem_coretransp_out.cpo"

# Initialize Campaign object
print('>>> Initialize Campaign object')
campaign_name = "UQ_GEM_"+cpo_dir.split('/')[-1]+"_"
my_campaign = uq.Campaign(name=campaign_name, work_dir=tmp_dir)

# Create new directory for inputs (to be ended with /)
campaign_dir = my_campaign.campaign_dir
common_dir = campaign_dir +"/common/"
os.system("mkdir " + common_dir)
print('>>> common_dir = ', common_dir)

# Copy input CPO files (cf test_gem0.f90)
os.system("cp " + cpo_dir + "/ets_equilibrium_in.cpo "
                + common_dir + "gem_equilibrium_in.cpo")
os.system("cp " + cpo_dir + "/ets_coreprof_in.cpo "
                + common_dir + "/gem_coreprof_in.cpo")

# Copy XML and XSD files
os.system("cp " + xml_dir + "/gem.xml " + common_dir)
os.system("cp " + xml_dir + "/gem.xsd " + common_dir)

# Parameter space for campaign and the distributions list for the Sampler
params, vary = cpo_io.get_inputs(dirname=common_dir, filename=input_filename,
                                 config_dict=uncertain_params,
                                 flux_index=flux_index)

# Create the encoder and get the app parameters
print('>>> Create the encoder')
encoder = CPOEncoder(template_filename=input_filename,
                     target_filename=input_filename,
                     common_dir=common_dir,
                     flux_index=flux_index)

# Create the decoder
print('>>> Create the decoder')
decoder = CPODecoder(target_filename=output_filename,
                     output_columns=output_columns)

# Create a collation element for this campaign
print('>>> Create Collater')
collater = uq.collate.AggregateSamples(average=False)

# Add the ETS app (automatically set as current app)
print('>>> Add app to campaign object')
my_campaign.add_app(name=campaign_name,
                    params=params,
                    encoder=encoder,
                    decoder=decoder,
                    collater=collater)

# Create the sampler
print('>>> Create the sampler')
my_sampler = uq.sampling.PCESampler(vary=vary, polynomial_order=3,
                                    regression=True)
my_campaign.set_sampler(my_sampler)

# Will draw all (of the finite set of samples)
print('>>> Draw Samples - Ns = ', my_sampler._number_of_samples)
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
    TaskRequirements(cores=Resources(exact=16)),
    application=mpi_instance + " " + exec_path
))

qcgpjexec.run(
    campaign=my_campaign,
    submit_order=SubmitOrder.RUN_ORIENTED
)

qcgpjexec.terminate_manager()

print('>>> PJ Ellapsed time: ', time.time() - time0)

print('>>> Collate')
my_campaign.collate()

# Post-processing analysis
print('>>> Post-processing analysis')
analysis = uq.analysis.PCEAnalysis(sampler=my_sampler, qoi_cols=output_columns)
my_campaign.apply_analysis(analysis)

print('>>> Get results')
results = my_campaign.get_last_analysis()

# Get Descriptive Statistics
print('>>> Get Descriptive Statistics: \n')

for qoi in output_columns:
    print('===========================================')
    print(qoi)
    print('STAT = \n', results['statistical_moments'][qoi])
    print('Sobol 1st = \n', results['sobols_first'][qoi])

print('>>> TEST GEM UQ PJ: END')
