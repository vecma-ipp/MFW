import os
import easyvvuq as uq
from easymfw.templates.cpo_encoder import CPOEncoder
from easymfw.templates.cpo_decoder import CPODecoder
from easymfw.templates.xml_encoder import XMLEncoder
from easymfw.utils.io_tools import get_cpo_inputs, get_xml_inputs
from eqi import TaskRequirements, Executor
from eqi import Task, TaskType, SubmitOrder


'''
UQ for the workflow Transport-Equilibrium-Turblence: ETS - CHEASE - GEM.
Uncertainties are driven by:
    External sources of Electrons heating.
    Boundary conditions (Plasma Edge) of electrons tempurature.
UQ Method: Non intrusive (UQP1) with PCE.
Execution: with QCG-Pilot Job.
'''


print('>>> LOOP GEM: START\n')

# Machine name
SYS = os.environ['SYS']

# Working directory
tmp_dir = os.environ['SCRATCH']

mpi_instance =  os.environ['MPICMD']


# CPO files location
cpo_dir = os.path.abspath("../workflows/AUG_28906_6_1ft_restart")

# XML and XSD files location
xml_dir = cpo_dir

# The execuatble code to run
obj_dir = os.path.abspath("../standalone/bin/"+SYS)
exec_code = "loop_gem"

# Define the uncertain parameters
# Electrons boudary condition
input_params_bc = {
    "te.boundary.value": {
        "dist": "Uniform",
        "err": 0.2,
    }
}
# Gaussian Sources: Electrons heating
input_params_src = {
    "electrons.heating_el.WTOT_el":{
        "dist": "Uniform",
        "err": 0.2,
    },
    "electrons.heating_el.RHEAT_el":{
        "dist": "Uniform",
        "err": 0.2,
    }
}

# CPO and XML files containg initial values of uncertain params
input_cpo_filename = "ets_coreprof_in.cpo"
input_cponame = "coreprof"
input_xml_filename = "source_dummy.xml"
input_xsd_filename = "source_dummy.xsd"

# The quantities of intersts list and the cpo file to set them
output_columns = ["te.value"]
output_filename = "ets_coreprof_out.cpo"
output_cponame = "coreprof"

print('>>> Get input parmeters\n')
# params: the parameter space for campaign object
# vary: adistributions list for the sampler
input_cpo_file = os.path.join(cpo_dir, input_cpo_filename)
params_cpo, vary_cpo = get_cpo_inputs(cpo_file=input_cpo_file,
                                      cpo_name=input_cponame,
                                      input_params=input_params_bc)

input_xml_file = os.path.join(xml_dir, input_xml_filename)
input_xsd_file = os.path.join(xml_dir, input_xsd_filename)
params_xml, vary_xml = get_xml_inputs(xml_file=input_xml_file,
                                      xsd_file=input_xsd_file,
                                      input_params=input_params_src)

# Merge the params dict
params = {**params_cpo, **params_xml}
vary = {**vary_cpo, **vary_xml}

# Initialize Campaign object
print('>>> Initialize Campaign object\n')
campaign_name = "UQ_LOOPGEM_"
my_campaign = uq.Campaign(name=campaign_name, work_dir=tmp_dir)

# Create new directory for commons inputs
campaign_dir = my_campaign.campaign_dir
common_dir = campaign_dir +"/common/"
os.mkdir(common_dir)

# Copy XML and XSD files
os.system("cp " + xml_dir + "/ets.x* "    + common_dir)
os.system("cp " + xml_dir + "/chease.x* " + common_dir)
os.system("cp " + xml_dir + "/gem.x* "   + common_dir)
os.system("cp " + xml_dir + "/source_dummy.x* " + common_dir)

# Copy input CPO files in common directory
os.system("cp " + cpo_dir + "/ets_coreprof_in.cpo "    + common_dir)
os.system("cp " + cpo_dir + "/ets_equilibrium_in.cpo " + common_dir)
os.system("cp " + cpo_dir + "/ets_coreimpur_in.cpo "   + common_dir)
os.system("cp " + cpo_dir + "/ets_coretransp_in.cpo "  + common_dir)
os.system("cp " + cpo_dir + "/ets_toroidfield_in.cpo " + common_dir)

# Copy exec file
exec_code = os.path.join(obj_dir, exec_code)
os.system("cp " + exec_code + " " + common_dir)

# Create the encoder and get the app parameters
print('>>> Create the encoders\n')
# params_names dict is given here because we will use MultiEncoder
encoder_cpo = CPOEncoder(template_filename=input_cpo_filename,
                         target_filename=input_cpo_filename,
                         input_cponame=input_cponame,
                         common_dir=common_dir,
                         input_params=input_params_bc)

encoder_xml = XMLEncoder(template_filename = input_xml_filename,
                         target_filename = input_xml_filename,
                         input_params=input_params_src,
                         common_dir=common_dir)

# Combine both encoders into a single encoder
encoder = uq.encoders.MultiEncoder(encoder_cpo, encoder_xml)

# Create the encoder
print('>>> Create the decoder\n')
decoder = CPODecoder(target_filename=output_filename,
                     output_columns=output_columns,
                     output_cponame=output_cponame)

# Create a collation element for this campaign
print('>>> Create Collater\n')
collater = uq.collate.AggregateSamples(average=False)

# Add the ETS app (automatically set as current app)
print('>>> Add app to campagn object\n')
my_campaign.add_app(name=campaign_name,
                    params=params,
                    encoder=encoder,
                    decoder=decoder,
                    collater=collater)

# Create the sampler
print('>>> Create the sampler\n')
my_sampler = uq.sampling.PCESampler(vary=vary,
                                    polynomial_order = 3)
my_campaign.set_sampler(my_sampler)

# Will draw all (of the finite set of samples)
print('>>> Draw Samples- Ns = ', my_sampler._number_of_samples)
my_campaign.draw_samples()

print('>>> Populate runs_dir\n')
my_campaign.populate_runs_dir()

# get ncores
gemxml = XMLElement(xml_dir + "/gem.xml")
npesx = gemxml.get_value("cpu_parameters.domain_decomposition.npesx")
npess = gemxml.get_value("cpu_parameters.domain_decomposition.npess")
nftubes = gemxml.get_value("cpu_parameters.parallel_cases.nftubes")
ncores = npesx*npess*nftubes
exec_path = os.path.join(common_dir, exec_code)

print(">>> Starting PJ execution\n")
qcgpjexec = Executor()
qcgpjexec.create_manager(dir=my_campaign.campaign_dir, log_level='info')

qcgpjexec.add_task(Task(
    TaskType.EXECUTION,
    TaskRequirements(cores=Resources(exact=ncores)),
    model=mpi_instance,
    application=exec_path
))

qcgpjexec.run(
    campaign=my_campaign,
    submit_order=SubmitOrder.EXEC_ONLY
)

qcgpjexec.terminate_manager()

print('>>> Collate\n')
my_campaign.collate()

# Post-processing analysis
print('>>> Post-processing analysis\n')
analysis = uq.analysis.PCEAnalysis(sampler=my_sampler, qoi_cols=output_columns)
my_campaign.apply_analysis(analysis)

print('>>> Get results\n')
results = my_campaign.get_last_analysis()

# Get Descriptive Statistics
print('>>> Get Descriptive Statistics\n')
stat = {}
sob1 = {}
dist = {}
for qoi in output_columns:
    stat[qoi] = results['statistical_moments'][qoi]
    sob1[qoi] = results['sobols_first'][qoi]
    dist[qoi] = results['output_distributions'][qoi]

#
from ascii_cpo import read

corep = read(input_cpo_file, "coreprof")
rho = corep.rho_tor_norm

# Save CSV file
import numpy as np

for qoi in output_columns:
    mean = list(stat[qoi]['mean'])
    std  = list(stat[qoi]['std'])

    header = 'RHO_TOR_NORM\tMEAN\tSTD'
    suf = qoi.split('.')[0]
    np.savetxt('outputs/STATS'+suf+'.csv',
               np.c_[rho, mean, std],
               delimiter='\t', comments='', header=header)


# Save graphics
from easymfw.utils import plots

uparams_names = list(params.keys())

for qoi in output_columns:
    fig = campaign_name + qoi.split('.')[0]
    plots.plot_stats(rho, stat[qoi],
                     xlabel=r'$\rho_{tor}$', ylabel=qoi,
                     ftitle=qoi+' profile',
                     fname='data/outputs/STAT_'+fig)

    plots.plot_sobols_all(rho, sob1[qoi], uparams_names,
                      ftitle='1st Sobol indices: '+qoi,
                      fname='data/outputs/SA_'+fig)

print('>>> LOOP GEM:: END')
