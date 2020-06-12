import os
import easyvvuq as uq
from ascii_cpo import read
from mfw.utils import cpo_io
from mfw.templates.cpo_encoder import CPOEncoder
from mfw.templates.cpo_decoder import CPODecoder
# GCG-PJ wrapper
import easypj
from easypj import TaskRequirements, Resources
from easypj import Task, TaskType, SubmitOrder


'''
Perform UQ for the workflow Transport-Equilibrium-Turblence.
Uncertainties are driven by:
    Boundary conditions (Plasma Edge) of electrons and ions tempurature.
Method: Non intrusive with PCE.
'''

print('>>> test ETS PJ: START')

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
exec_code = "ets_test3"

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
output_columns = ["Te"]
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

# Copy the exec file
os.system("cp " + obj_dir + "/"+ exec_code + " " + common_dir)

# Copy input CPO files (cf test_ets.f90)
os.system("cp " + cpo_dir + "/*.cpo " + common_dir)

# Copy XML and XSD files
os.system("cp " + xml_dir + "/ets.xml " + common_dir)
os.system("cp " + xml_dir + "/ets.xsd " + common_dir)

# Create the encoder and get the app parameters
print('>>> Create the encoder')
encoder = CPOEncoder(template_filename=input_filename,
                     target_filename=input_filename,
                     common_dir=common_dir,
                     t=2)

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
my_sampler = uq.sampling.QMCSampler(vary=vary, n_samples=192)
my_campaign.set_sampler(my_sampler)

# Will draw all (of the finite set of samples)
my_campaign.draw_samples()

print(">>> Starting PJ execution")
exec_path = os.path.join(common_dir, exec_code)

qcgpjexec = easypj.Executor()
qcgpjexec.create_manager(dir=my_campaign.campaign_dir, log_level='info')

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

print('>>> Collate')
my_campaign.collate()

# Post-processing analysis
print('>>> Post-processing analysis')
analysis = uq.analysis.QMCAnalysis(sampler=my_sampler, qoi_cols=output_columns)
my_campaign.apply_analysis(analysis)

print('>>> Get results')
results = my_campaign.get_last_analysis()

print('>>> test ETS PJ : END')
