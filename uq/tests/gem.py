import os
import easyvvuq as uq
# EasyVVUQ/QCG-PJ
import eqi
# from ual
from ascii_cpo import read
# from current package
from base.cpo_encoder import CPOEncoder
from base.cpo_decoder import CPODecoder
from base.xml_element import XMLElement
from base.utils import cpo_inputs

'''
Perform UQ for the Turblence code GEM.
Uncertainties are driven by:
The electon and ion temperatur and their gradient localisd on the Flux tube position.
'''


print('TEST GEM-UQ: START')

# We test 1 flux tube
# run gem_test in strandalone and use:
# base.utils.ftube_indices('gem_coreprof_in.cpo','gem_coretransp_out.cpo') to get the index
ftube_index = 44

# Machine name
SYS = os.environ['SYS']

# Working directory
tmp_dir = os.environ['SCRATCH']

# From Slurm script (intelmpi)
mpi_instance =  os.environ['MPICMD']

# CPO files location
cpo_dir = os.path.abspath("../workflows/AUG_28906_6_8ft_restart")

# XML and XSD files location
xml_dir = os.path.abspath("../workflows")

# The executable code to run
obj_dir = os.path.abspath("../standalone/bin/"+SYS)
exec_code = "gem_test"

# Define the uncertain parameters
# Electron temperature and its gradient
input_params = {
    "te.value": {"dist": "Uniform", "err":  0.2, "min": 0.},
    "ti.value": {"dist": "Uniform", "err":  0.2, "min": 0.},
    "te.ddrho": {"dist": "Uniform", "err":  0.2, "max": 0.},
    "ti.ddrho": {"dist": "Uniform", "err":  0.2, "max": 0.}
}

# CPO file containg initial values of uncertain params
input_filename = "ets_coreprof_in.cpo"
input_cponame = "coreprof"

# The quantities of intersts and the cpo file to set them
output_columns = ["te_transp.flux", "ti_transp.flux"]
output_filename = "gem_coretransp_out.cpo"
output_cponame = "coretransp"

# parameter space for campaign and the distributions list for the sampler
params, vary = cpo_inputs(cpo_filename=input_filename,
                          cpo_name=input_cponame,
                          input_dir=cpo_dir,
                          input_params=input_params,
                          ftube_index=ftube_index)

# Initialize Campaign object
campaign_name = "UQ_1FTGEM_"
my_campaign = uq.Campaign(name=campaign_name, work_dir=tmp_dir)

# Create new directory for inputs
campaign_dir = my_campaign.campaign_dir
common_dir = campaign_dir +"/common/"
os.mkdir(common_dir)

# Copy input CPO files (cf. test_gem0.f90)
os.system("cp " + cpo_dir + "/ets_equilibrium_in.cpo "
                + common_dir + "gem_equilibrium_in.cpo")
os.system("cp " + cpo_dir + "/ets_coreprof_in.cpo "
                + common_dir + "/gem_coreprof_in.cpo")

# Copy restart file
os.system("cp " + cpo_dir + "/t00.dat " + common_dir)

# Copy XML and XSD files
os.system("cp " + xml_dir + "/gem.xml " + common_dir)
os.system("cp " + xml_dir + "/gem.xsd " + common_dir)

# Copy  exec file
os.system("cp " + obj_dir +"/"+ exec_code + " " + common_dir)
exec_path = os.path.join(common_dir, exec_code)

# Create the encoder and the decoder
input_filename = "gem_coreprof_in.cpo"
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

# get ncores
gemxml = XMLElement(xml_dir + "/gem.xml")
npesx = gemxml.get_value("cpu_parameters.domain_decomposition.npesx")
npess = gemxml.get_value("cpu_parameters.domain_decomposition.npess")
nftubes = gemxml.get_value("cpu_parameters.parallel_cases.nftubes")
ncores = npesx*npess*nftubes

# Execution
qcgpjexec = eqi.Executor(my_campaign)
qcgpjexec.create_manager(log_level='info')

qcgpjexec.add_task(eqi.Task(
    eqi.TaskType.EXECUTION,
    eqi.TaskRequirements(cores=ncores),
    model=mpi_instance,
    application=exec_path
))
qcgpjexec.run(processing_scheme=eqi.ProcessingScheme.EXEC_ONLY)
qcgpjexec.terminate_manager()

# Collection of simulation outputs
my_campaign.collate()

# Post-processing analysis
analysis = uq.analysis.PCEAnalysis(sampler=my_sampler, qoi_cols=output_columns)
my_campaign.apply_analysis(analysis)

# Get results
results = my_campaign.get_last_analysis()

# Get Descriptive Statistics
mean_el = results.describe('te_transp.flux', 'mean')
std_el = results.describe('te_transp.flux', 'std')
mean_io = results.describe('ti_transp.flux', 'mean')
std_io = results.describe('ti_transp.flux', 'std')

s1_el = results.sobols_first('te_transp.flux')
s1_io = results.sobols_first('ti_transp.flux')

print("TE TRANSP FLUX")
print("Mean: ", mean_el)
print("Std: ", std_el)
print("Sob1: ", s1_el)

print("TI TRANSP FLUX")
print("Mean: ", mean_io)
print("Std: ", std_io)
print("Sob1: ", s1_io)

print('>>> TEST GEM-UQ: END')
