import os
import easyvvuq as uq
from ascii_cpo import read
from mfw.templates.cpo_element import get_inputs
from mfw.templates.cpo_encoder import CPOEncoder
from mfw.templates.cpo_decoder import CPODecoder
from mfw.utils import plots
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
#cpo_dir = os.path.abspath("../workflows/AUG_28906_6")
cpo_dir = os.path.abspath("../workflows/JET_92436_23066")

# XML and XSD files
xml_dir = os.path.abspath("../workflows")

# The executable code to run
obj_dir = os.path.abspath("../standalone/bin/"+SYS)
exec_code = "ets_test"
exec_path = os.path.join(obj_dir, exec_code)

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
encoder = CPOEncoder(template_filename = input_filename,
                     target_filename = input_filename,
                     cpo_name = input_cponame,
                     common_dir = common_dir)

# Create the encoder
print('>>> Create the decoder')
decoder = CPODecoder(target_filename=output_filename,
                     output_columns=output_columns,
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
my_sampler = uq.sampling.PCESampler(vary=vary, polynomial_order=3,
                                    regression=True)
my_campaign.set_sampler(my_sampler)

# Will draw all (of the finite set of samples)
my_campaign.draw_samples()

print('>>> Populate runs_dir')
my_campaign.populate_runs_dir()

print(">>> Starting PJ execution\n")
qcgpjexec = easypj.Executor()
qcgpjexec.create_manager(dir=my_campaign.campaign_dir, log_level='info')

#qcgpjexec.add_task(Task(
#    TaskType.ENCODING,
#    TaskRequirements(cores=Resources(exact=1))
#))

qcgpjexec.add_task(Task(
    TaskType.EXECUTION,
    TaskRequirements(cores=Resources(exact=1)),
    application=exec_path
))

qcgpjexec.run(
    campaign=my_campaign,
    submit_order=SubmitOrder.EXEC_ONLY
)

qcgpjexec.terminate_manager()

print('>>> Collate')
my_campaign.collate()

# Post-processing analysis
print('>>> Post-processing analysis')
analysis = uq.analysis.PCEAnalysis(sampler=my_sampler, qoi_cols=output_columns)
my_campaign.apply_analysis(analysis)

print('>>> Get results')
results = my_campaign.get_last_analysis()

print('>>> Get Descriptive Statistics')
stat = {}
sob1 = {}
for qoi in output_columns:
    stat[qoi] = results['statistical_moments'][qoi]
    sob1[qoi] = results['sobols_first'][qoi]

#  Graphics for Descriptive satatistics
corep = read(os.path.join(cpo_dir,  "ets_coreprof_in.cpo"), "coreprof")
rho = corep.rho_tor_norm
uparams_names = list(params.keys())

#for qoi in output_columns:
#    plots.plot_stats(rho, stat[qoi],
#                     xlabel=r'$\rho_{tor}$', ylabel=qoi,
#                     ftitle=qoi+' profile',
#                     fname='outputs/STAT_'+qoi)
#
#
#    plots.plot_sobols_all(rho, sob1[qoi], uparams_names,
#                      ftitle='1st Sobol indices: '+qoi,
#                      fname='outputs/SA_'+qoi)

print('>>> test ETS PJ : END')
