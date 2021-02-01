import os
import easyvvuq as uq
# EasyVVUQ/QCG-PJ
import eqi
# from ual
from ascii_cpo import read
# from current package
from base.cpo_encoder import CPOEncoder
from base.cpo_decoder import CPODecoder
from base.utils import cpo_inputs
import time
'''
Perform UQ for the Turblence code GEM0.
Uncertainties are driven by:
The electon and ion temperatur and their gradient localisd on the Flux tube position.
IMPORTANT CHECK: in gem0.xml, nrho_transp = 1.
'''


print('TEST GEM0-UQ: START')

# We test 1 flux tube
# run gem0_test in strandalone and use:
# base.utils.ftube_indices('gem0_coreprof_in.cpo','gem0_coretransp_out.cpo') to get the index
ftube_index = 61

# Machine name
SYS = os.environ['SYS']

# Working directory
tmp_dir = os.environ['SCRATCH']

# CPO files location
cpo_dir = os.path.abspath("../workflows/AUG_28906_6")

# XML and XSD files location
xml_dir = os.path.abspath("../workflows")

# The executable code to run
obj_dir = os.path.abspath("../standalone/bin/"+SYS)
exec_code = "gem0_test"

# Define the uncertain parameters
# Electron temperature and its gradient

dist ={ "dist": "Uniform", "err":  0.2}
input_params = {
    "te.value": dist,
    "te.ddrho": dist,
    "ti.value": dist,
    "ti.ddrho": dist
}

# CPO file containg initial values of uncertain params
input_filename = "ets_coreprof_in.cpo"
input_cponame = "coreprof"

# The quantities of intersts and the cpo file to set them
output_columns = ["te_transp.flux", "ti_transp.flux"]
output_filename = "gem0_coretransp_out.cpo"
output_cponame = "coretransp"

# parameter space for campaign and the distributions list for the sampler
params, vary = cpo_inputs(cpo_filename=input_filename,
                          cpo_name=input_cponame,
                          input_dir=cpo_dir,
                          input_params=input_params,
                          ftube_index=ftube_index)

# Initialize Campaign object
campaign_name = "UQ_GEM0_"
my_campaign = uq.Campaign(name=campaign_name, work_dir=tmp_dir)

# Create new directory for inputs
campaign_dir = my_campaign.campaign_dir
common_dir = campaign_dir +"/common/"
os.mkdir(common_dir)

# Copy input CPO files (cf. test_gem0.f90)
os.system("cp " + cpo_dir + "/ets_equilibrium_in.cpo "
                + common_dir + "gem0_equilibrium_in.cpo")
os.system("cp " + cpo_dir + "/ets_coreprof_in.cpo "
                + common_dir + "/gem0_coreprof_in.cpo")

# Copy XML and XSD files
os.system("cp " + xml_dir + "/gem0.xml " + common_dir)
os.system("cp " + xml_dir + "/gem0.xsd " + common_dir)

# Copy  exec file
os.system("cp " + obj_dir +"/"+ exec_code + " " + common_dir)
exec_path = os.path.join(common_dir, exec_code)

# Create the encoder and the decoder
input_filename = "gem0_coreprof_in.cpo"
encoder = CPOEncoder(cpo_filename=input_filename,
                     cpo_name=input_cponame,
                     input_dir=common_dir,
                     ftube_index=ftube_index)

decoder = CPODecoder(cpo_filename=output_filename,
                     cpo_name=output_cponame,
                     output_columns=output_columns)

# Add the app (automatically set as current app)
my_campaign.add_app(name=campaign_name,
                    params=params,
                    encoder=encoder,
                    decoder=decoder)

# Create the sampler
my_sampler = uq.sampling.PCESampler(vary=vary, polynomial_order=3)
my_campaign.set_sampler(my_sampler)

# Will draw all (of the finite set of samples)
my_campaign.draw_samples()
my_campaign.populate_runs_dir()

# Execution
qcgpjexec = eqi.Executor(my_campaign)
qcgpjexec.create_manager(log_level='info')

qcgpjexec.add_task(eqi.Task(
    eqi.TaskType.EXECUTION,
    eqi.TaskRequirements(cores=1),
    application=exec_path
))
qcgpjexec.run(processing_scheme=eqi.ProcessingScheme.EXEC_ONLY)
qcgpjexec.terminate_manager()

# Collection of simulation outputs
my_campaign.collate()

t1 = time.time()
# Post-processing analysis
analysis = uq.analysis.PCEAnalysis(sampler=my_sampler, qoi_cols=output_columns)
t2 = time.time()
my_campaign.apply_analysis(analysis)
t3 = time.time()

# Get results
results = my_campaign.get_last_analysis()

# Get Descriptive Statistics

print("t2-t1: ", t2-t1)
print("t3-t1: ", t3-t1)
print('>>> TEST GEM0-UQ: END')
