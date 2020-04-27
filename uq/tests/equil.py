import os
import easyvvuq as uq
from easymfw.templates.cpo_encoder import CPOEncoder
from easymfw.templates.cpo_decoder import CPODecoder
from easymfw.utils.io_tools import get_cpo_inputs

'''
Perform SA for the workflow Equilibrium (EquilUpdate + CHEASE).
Uncertainties are driven by:
    Ti and Te GRADIENT
'''

print('>>> test_equil: START')

indices = [25, 50]

# execustion with QCJ-PJ
EXEC_PJ = True

# Machine name
SYS = os.environ['SYS']

# Working directory
tmp_dir = os.environ['SCRATCH']

# CPO files
cpo_dir = os.path.abspath("../workflows/AUG_28906_6")
#cpo_dir = os.path.abspath("../workflows/JET_92436_23066")

# XML and XSD files
xml_dir = os.path.abspath("../workflows")

# The executable code to run
obj_dir = os.path.abspath("../standalone/bin/"+SYS)
exec_code = "equil_test"

# Define uncertain parameters
temp_params = {
    "te": {
        "dist": "Normal",
        "err": 0.2,
        "ids": indices,
    }#,
    #"ti": {
    #    "dist": "Normal",
    #    "err": 0.2,
    #    "ids": indices,
    #}
}

input_params = {}
input_params.update(temp_params)

# CPO file containg initiail values of uncertain params
input_filename = "ets_coreprof_in.cpo"
input_cponame = "coreprof"

# The quantities of intersts and the cpo file to set them
output_columns = ["profiles_1d.gm3", "profiles_1d.pressure"]
output_filename = "equil_equilibrium_out.cpo"
output_cponame = "equilibrium"

# Parameter space for campaign and the distributions list for the Sampler
input_cpo_file = os.path.join(cpo_dir, input_filename)
params, vary = get_cpo_inputs(cpo_file=input_cpo_file,
                              cpo_name=input_cponame,
                              input_params=input_params)

# Initialize Campaign object
print('>>> Initialize Campaign object')
campaign_name = "UQEQUIL_"
my_campaign = uq.Campaign(name=campaign_name, work_dir=tmp_dir)

# Create new directory for inputs
campaign_dir = my_campaign.campaign_dir
common_dir = campaign_dir +"/common/"
os.mkdir(common_dir)
print('>>> common_dir = ', common_dir)

# Copy input CPO files (cf test_gem0.f90)
os.system("cp " +cpo_dir+ "/ets_equilibrium_in.cpo " + common_dir+ "equil_equilibrium_in.cpo")
os.system("cp " +cpo_dir+ "/ets_coreprof_in.cpo " + common_dir+ "/equil_coreprof_in.cpo")
os.system("cp " +cpo_dir+ "/ets_toroidfield_in.cpo " + common_dir+ "/equil_toroidfield_in.cpo")

# Copy XML and XSD files
os.system("cp " +xml_dir+ "/chease.xml " +common_dir)
os.system("cp " +xml_dir+ "/chease.xsd " +common_dir)

# Copy  exec file
os.system("cp " + obj_dir +"/" + exec_code + " " + common_dir)

# Create the encoder and get the app parameters
print('>>> Create the encoder')
input_filename = "equil_coreprof_in.cpo"
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
print('>>> Draw Samples')
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
print('>>> Get Descriptive Statistics: \n')

stat = {}
sob1 = {}
dist = {}
for qoi in output_columns:
    stat[qoi] = results['statistical_moments'][qoi]
    sob1[qoi] = results['sobols_first'][qoi]

# Save graphics
from easymfw.utils import plots
from ascii_cpo import read

equil_file = os.path.join(cpo_dir,  "ets_equilibrium_in.cpo")
equil = read(equil_file, "equilibrium")
rho = equil.profiles_1d.rho_tor

uparams_names = list(params.keys())

for qoi in output_columns:
    q = qoi.split('.')[-1]
    plots.plot_stats(rho, stat[qoi],
                     xlabel=r'$\rho_{tor}$', ylabel=q,
                     ftitle=qoi+' profile',
                     fname='data/outputs/STAT_'+q+"_"+campaign_name)

    plots.plot_sobols_all(rho, sob1[qoi], uparams_names,
                      ftitle='1st Sobol indices: '+q,
                      fname='data/outputs/SA_'+q+"_"+campaign_name)


print('>>> equilibrium_uq : END')
