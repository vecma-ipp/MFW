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
Perform SA for the workflow Equilibrium (EquilUpdate + CHEASE).
Uncertainties are driven by:
    Ti and Te GRADIENT
'''

print('>>> test_equil: START')

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
exec_code = "equil_test"
exec_path = os.path.join(obj_dir, exec_code)

# Define a specific parameter space
temp_params = {
    "te": {
        "dist": "Normal",
        "err": 0.2,
    },
    "ti": {
        "dist": "Normal",
        "err": 0.2,
    }
}

uncertain_params = {}
uncertain_params.update(temp_params)

# CPO file containg initiail values of uncertain params
input_filename = "ets_coreprof_in.cpo"
input_cponame = "coreprof"

# The quantities of intersts and the cpo file to set them
output_filename = "eq_equilibrium_out.cpo"
output_columns = ["gm1", "gm2", "gm3", "gm4", "gm5", "gm6", "gm7", "gm8", "gm9"]
output_cponame = "equilibrium"

# Parameter space for campaign and the distributions list for the Sampler
params, vary = cpo_io.get_inputs(dirname=cpo_dir, filename=input_filename,
                                 config_dict=uncertain_params)

# Initialize Campaign object
print('>>> Initialize Campaign object')
campaign_name = "UQ_EQL_"
my_campaign = uq.Campaign(name=campaign_name, work_dir=tmp_dir)

# Create new directory for inputs (to be ended with /)
campaign_dir = my_campaign.campaign_dir
common_dir = campaign_dir +"/common/"
os.system("mkdir " + common_dir)
print('>>> common_dir = ', common_dir)

# Copy input CPO files (cf test_gem0.f90)
os.system("cp " +cpo_dir+ "/ets_equilibrium_in.cpo " +common_dir+ "equil_equilibrium_in.cpo")
os.system("cp " +cpo_dir+ "/ets_coreprof_in.cpo " +common_dir+ "/equil_coreprof_in.cpo")
os.system("cp " +cpo_dir+ "/ets_toroidfield_in.cpo " +common_dir+ "/equil_toroidfield_in.cpo")

# Copy XML and XSD files
os.system("cp " +xml_dir+ "/chease.xml " +common_dir)
os.system("cp " +xml_dir+ "/chease.xsd " +common_dir)


# Create the encoder and get the app parameters
print('>>> Create the encoder')
input_filename = "equil_coreprof_in.cpo"
encoder = CPOEncoder(template_filename=input_filename,
                     target_filename=input_filename,
                     cpo_name = input_cponame,
                     common_dir=common_dir)

# Create the decoder
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

# Get Descriptive Statistics
print('>>> Get Descriptive Statistics: \n')

# Get Descriptive Statistics
print('>>> Get Descriptive Statistics')

stat = []
sob1 = []

for qoi in  output_columns:
    stat.append(results['statistical_moments'][qoi])
    sob1.append(results['sobols_first'][qoi])

# Plots STAT and SA
__PLOTS = True
if __PLOTS:
    from mfw.utils import plots
    equil = read(os.path.join(cpo_dir,  "ets_equilibrium_in.cpo"), "equilibrium")
    rho = equil.profiles_1d.rho_tor
    uparams_names = list(params.keys())

    for i in range(10):

        plots.plot_stats(rho, stat[i],
                         xlabel=r'$\rho_{tor} ~ [m]$', ylabel='GM'+str(i+1),
                         ftitle='GM'+str(i+1)+' profile',
                         fname='outputs/GM'+str(i+1)+'_STAT')

        plots.plot_sobols(rho, sob1[i], uparams_names,
                          ftitle=' First-Order Sobol indices - QoI: GM'+str(i+1),
                          fname='outputs/GM'+str(i+1)+'_SOB1')

print('>>> equilibrium_uq : END')
