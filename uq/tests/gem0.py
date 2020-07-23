import os
import easyvvuq as uq
from ascii_cpo import read
from easymfw.templates.cpo_encoder import CPOEncoder
from easymfw.templates.cpo_decoder import CPODecoder
from easymfw.utils.io_tools import get_cpo_inputs

'''
Perform UQ for the Turblence code GEM
Uncertainties are driven by: The electon and ion temperatur and
their gradient localisd by a Flux tube position.
IMPORTANT CHECK: in gem0.xml, nrho_transp = 1
'''


print('TEST GEM0-UQ: START')

# We test 1 flux tube
flux_indices = [69]
#flux_indices = [15, 31, 44, 55, 66, 76, 85, 94]
# execustion with QCJ-PJ
EXEC_PJ = True #False

# Machine name
SYS = os.environ['SYS']

# Working directory
tmp_dir = os.environ['SCRATCH']

# CPO files location
cpo_dir = os.path.abspath("../workflows/AUG_28906_6_1ft_restart")
#cpo_dir = os.path.abspath("../workflows/AUG_28906_6_8ft_restart")
print("cpodir: " + cpo_dir + "\n")
#cpo_dir = os.path.abspath("../workflows/JET_92436_23066")

# XML and XSD files location
xml_dir = os.path.abspath("../workflows")

# The executable code to run
obj_dir = os.path.abspath("../standalone/bin/"+SYS)
exec_code = "gem0_test"

# Define the uncertain parameters
# Electron temperature and its gradient
input_params = {
    "te.value": {
        "dist": "Normal",
        "err":  0.2,
        "idx": flux_indices,
    },
    "te.ddrho": {
        "dist": "Normal",
        "err": 0.2,
        "idx": flux_indices,
    } #,
#    "ti.value": {
#        "dist": "Normal",
#        "err":  0.2,
#        "idx": flux_indices,
#    },
#    "ti.ddrho": {
#        "dist": "Normal",
#        "err": 0.2,
#        "ids": flux_indices,
#    }
}

# CPO file containg initial values of uncertain params
input_filename = "ets_coreprof_in.cpo"
input_cponame = "coreprof"

# The quantities of intersts and the cpo file to set them
output_columns = ["te_transp.flux"] #, "ti_transp.flux"]
output_filename = "gem0_coretransp_out.cpo"
output_cponame = "coretransp"

# parameter space for campaign and the distributions list for the sampler
input_cpo_file = os.path.join(cpo_dir, input_filename)
print("input file: " + input_cpo_file + "\n")
params, vary = get_cpo_inputs(cpo_file=input_cpo_file,
                              cpo_name=input_cponame,
                              input_params=input_params)

# Initialize Campaign object
print('>>> Initialize Campaign object')
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

# Create the encoder
print('>>> Create the encoder')
input_filename = "gem0_coreprof_in.cpo"
encoder = CPOEncoder(template_filename=input_filename,
                     target_filename=input_filename,
                     input_cponame=input_cponame,
                     input_params=input_params,
                     common_dir=common_dir)

# Create the decoder
print('>>> Create the decoder')
decoder = CPODecoder(target_filename=output_filename,
                     output_columns=output_columns,
                     output_cponame=output_cponame)

# Create a collation element for this campaign
print('>>> Create Collater')
collater = uq.collate.AggregateSamples(average=False)

# Add the app (automatically set as current app)
print('>>> Add app to campaign object')
my_campaign.add_app(name=campaign_name,
                    params=params,
                    encoder=encoder,
                    decoder=decoder,
                    collater=collater)

# Create the sampler
print('>>> Create the sampler')
my_sampler = uq.sampling.PCESampler(vary=vary,
                                    polynomial_order=3)
my_campaign.set_sampler(my_sampler)

# Will draw all (of the finite set of samples)
print('>>> Draw Samples - Ns = ', my_sampler.n_samples)
my_campaign.draw_samples()

print('>>> Populate runs_dir')
my_campaign.populate_runs_dir()

exec_path = os.path.join(common_dir, exec_code)
if EXEC_PJ:
    # GCG-PJ wrapper
    import easypj
    from easypj import TaskRequirements, Resources
    from easypj import Task, TaskType, SubmitOrder

    print(">>> Starting PJ execution\n")
    qcgpjexec = easypj.Executor()
    qcgpjexec.create_manager(dir=my_campaign.campaign_dir, log_level='debug')

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
print('>>> Get Descriptive Statistics: \n')
stat = {}
sob1 = {}
dist = {}
for qoi in output_columns:
    stat[qoi] = results['statistical_moments'][qoi]
    sob1[qoi] = results['sobols_first'][qoi]

    print(qoi)
    print('Stats: \n', stat[qoi] )
    print('Sobol 1st: \n',sob1[qoi])

print('>>> TEST GEM0-UQ: END')
