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

# execustion with QCJ-PilotJob
EXEC_PJ = False

# Machine name
SYS = os.environ['SYS']

# Working directory
tmp_dir = os.environ['SCRATCH']

# CPO files location
cpo_dir = os.path.abspath("../workflows/AUG_28906_6")
#cpo_dir = os.path.abspath("../workflows/JET_92436_23066")

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

# params: the parameter dict for campaign object
# vary: distributions list for the sampler
print('>>> Get input parmeters')
input_cpo_file = os.path.join(cpo_dir, input_filename)
params, vary = get_cpo_inputs(cpo_file = input_cpo_file,
                              cpo_name = input_cponame,
                              input_params = uncertain_params)

# Initialize Campaign object
print('>>> Initialize Campaign object')
test_case = cpo_dir.split('/')[-1]
campaign_name = "UQBC_"+test_case
my_campaign = uq.Campaign(name=campaign_name, work_dir=tmp_dir)

# Create new directory for inputs
campaign_dir = my_campaign.campaign_dir
common_dir = campaign_dir +"/common/"
os.mkdir(common_dir)

# Copy input CPO files
os.system("cp " + cpo_dir + "/*.cpo " + common_dir)

# Copy XML and XSD files
os.system("cp " + xml_dir + "/ets.xml "    + common_dir)
os.system("cp " + xml_dir + "/ets.xsd "    + common_dir)
os.system("cp " + xml_dir + "/chease.xml " + common_dir)
os.system("cp " + xml_dir + "/chease.xsd " + common_dir)
os.system("cp " + xml_dir + "/gem0.xml "   + common_dir)
os.system("cp " + xml_dir + "/gem0.xsd "   + common_dir)

# Copy exec file
os.system("cp " + exec_code + " " + common_dir)

# Create the encoder and get the app parameters
print('>>> Create the encoder')
encoder = CPOEncoder(template_filename=input_filename,
                     target_filename=input_filename,
                     input_cponame=input_cponame,
                     common_dir=common_dir)

# Create the encoder
print('>>> Create the decoder')
decoder = CPODecoder(target_filename=output_filename,
                     output_columns=output_columns,
                     output_cponame=output_cponame)

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
                                    regression=True)
my_campaign.set_sampler(my_sampler)

# Will draw all (of the finite set of samples)
print('>>> Draw Samples- Ns = ', my_sampler._number_of_samples)
my_campaign.draw_samples()

print('>>> Populate runs_dir')
my_campaign.populate_runs_dir()

exec_path = os.path.join(obj_dir, exec_code)
if EXEC_PJ:
    # GCG-PJ wrapper
    import easypj
    from easypj import TaskRequirements, Resources
    from easypj import Task, TaskType, SubmitOrder

    print(">>> Starting PJ execution\n")
    qcgpjexec = easypj.Executor()
    qcgpjexec.create_manager(dir=my_campaign.campaign_dir, log_level='info')

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
else:
    print('>>> Starting Local execution')
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

# Save graphics
from easymfw.utils import plots
from ascii_cpo import read

corep = read(input_cpo_file, "coreprof")
rho = corep.rho_tor_norm

uparams_names = list(params.keys())

for qoi in output_columns:
    plots.plot_stats(rho, stat[qoi],
                     xlabel=r'$\rho_{tor}$', ylabel=qoi,
                     ftitle=qoi+' profile',
                     fname='data/outputs/STAT_'+qoi+"_"+campaign_name)

    plots.plot_sobols_all(rho, sob1[qoi], uparams_names,
                      ftitle='1st Sobol indices: '+qoi,
                      fname='data/outputs/SA_'+qoi+"_"+campaign_name)

print('>>> UQ-Workflow BC: END')
