import os
import easyvvuq as uq
import chaospy as cp
from ascii_cpo import read
from easymfw.templates.cpo_encoder import CPOEncoder
from easymfw.templates.cpo_decoder import CPODecoder
from easymfw.templates.cpo_element import CPOElement
from easymfw.utils.io_tools import get_cpo_inputs

# GCG-PJ wrapper
import easypj
from easypj import TaskRequirements, Resources
from easypj import Task, TaskType, SubmitOrder
# skl models wrapper
import pickle



# BOX1 - GPR model by SKL

# location of the flux tubes
flux_indices = 69
# execustion with QCJ-PJ
EXEC_PJ = False

# Machine name
SYS = os.environ['SYS']
# Working directory
tmp_dir = os.environ['SCRATCH']

# CPO files location
cpo_dir = os.path.abspath("../workflows/AUG_28906_6")
# XML and XSD files location
xml_dir = os.path.abspath("../workflows")

# The executable code to run --- key difference for a box
#obj_dir = os.path.abspath("data/models/")
obj_dir = os.path.abspath("surr_p1")
exec_code = "model_predictor.py"

# Define the uncertain parameters
# Electron temperature and its gradient
input_params = {
    "te.value": {
        "dist": "Normal",
        "err":  0.2,
        "ft_index": flux_indices,
    }#,
#    "te.ddrho": {
#        "dist": "Normal",
#        "err": 0.2,
#        "idx": flux_indices,
#    },
#    "ti.value": {
#        "dist": "Normal",
#        "err":  0.2,
#        "idx": flux_indices,
#    } #,
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
output_columns = ["te_transp.flux", "ti_transp.flux"]
output_filename = "gem_coretransp_out.cpo"
output_cponame = "coretransp"


# parameter space for campaign and the distributions list for the sampler
input_cpo_file = os.path.join(cpo_dir, input_filename)
params, vary = get_cpo_inputs(cpo_file=input_cpo_file,
                              cpo_name=input_cponame,
                              input_params=input_params)

# Initialize Campaign object
print('>>> Initialize Campaign object')
campaign_name = "UQ_GEM_SURROGATE_"
my_campaign = uq.Campaign(name=campaign_name, work_dir=tmp_dir)

# Create new directory for inputs
campaign_dir = my_campaign.campaign_dir
common_dir = campaign_dir +"/common/"
os.mkdir(common_dir)

# Copy input CPO files (cf. test_gem.f90)
os.system("cp " + cpo_dir + "/ets_equilibrium_in.cpo "
                + common_dir + "gem_equilibrium_in.cpo")
os.system("cp " + cpo_dir + "/ets_coreprof_in.cpo "
                + common_dir + "/gem_coreprof_in.cpo")

#os.system("cp " + cpo_dir + "/t00.dat " + common_dir)

# Copy XML and XSD files
os.system("cp " + xml_dir + "/gem.xml " + common_dir)
os.system("cp " + xml_dir + "/gem.xsd " + common_dir)

# Copy  exec file -- key difference
os.system("cp " + obj_dir + "/" + exec_code + " " + common_dir)

# Create the encoder
print('>>> Create the encoder')
input_filename = "gem_coreprof_in.cpo"
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
                                    polynomial_order=2)
#my_sampler = uq.sampling.QMCSampler(vary=vary, n_mc_samples=1000)

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
    qcgpjexec.create_manager(dir=my_campaign.campaign_dir,
                             log_level='debug')

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
