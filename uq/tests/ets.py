import os
import easyvvuq as uq
# from ual
from ascii_cpo import read
# from current package
from base.cpo_encoder import CPOEncoder
from base.cpo_decoder import CPODecoder
from base.utils import cpo_inputs
from base.plots import plot_moments

'''
Perform UQ for the Transport: ETS
Uncertainties are driven by:
    Boundary conditions (Plasma Edge) of electrons and ions tempurature.
Method: Non intrusive with PCE.
'''

# Execution with QCG-PJ
EXEC_PJ = True

# Machine name
SYS = os.environ['SYS']

# Working directory
tmp_dir = os.environ['SCRATCH']

# CPO files location
cpo_dir = os.path.abspath("../workflows/AUG_28906_6")

# XML and XSD files
xml_dir = os.path.abspath("../workflows")

# The executable code to run
obj_dir = os.path.abspath("../standalone/bin/"+SYS)
exec_code = "ets_test"

# Define the uncertain parameters
input_params = {
    "te.boundary.value": {
        "dist": "Normal",
        "err":  0.2,
    }
    ,
    "ti.boundary.value": {
        "dist": "Normal",
        "err": 0.2,
    }
}
# CPO file containg initial values of uncertain params
input_filename = "ets_coreprof_in.cpo"
input_cponame = "coreprof"

# The quantities of intersts and the cpo file to set them
#output_columns = ["te.value", "ti.value", "ne.value", "ni.value", "psi.value"]
output_columns = ["te.value", "ti.value"]
output_filename = "ets_coreprof_out.cpo"
output_cponame = "coreprof"

# parameter space for campaign and the distributions list for the sampler
params, vary = cpo_inputs(cpo_filename=input_filename,
                          cpo_name=input_cponame,
                          input_dir=cpo_dir,
                          input_params=input_params)

# Initialize Campaign object
campaign_name = "UQBC_ETS_"
my_campaign = uq.Campaign(name=campaign_name, work_dir=tmp_dir)

# Create new directory for inputs
campaign_dir = my_campaign.campaign_dir
common_dir = campaign_dir +"/common/"
os.mkdir(common_dir)

# Copy input CPO files (cf test_ets.f90)
os.system("cp " + cpo_dir + "/*.cpo " + common_dir)

# Copy XML and XSD files
os.system("cp " + xml_dir + "/ets.xml " + common_dir)
os.system("cp " + xml_dir + "/ets.xsd " + common_dir)

# Copy  exec file
os.system("cp " + obj_dir +"/" + exec_code + " " + common_dir)
exec_path = os.path.join(common_dir, exec_code)

# Create the encoder and the decoder
encoder = CPOEncoder(cpo_filename=input_filename,
                     cpo_name=input_cponame,
                     input_dir=common_dir)

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
if EXEC_PJ:
    # With QCG-Pilot Job
    import eqi
    qcgpjexec = eqi.Executor(my_campaign)
    qcgpjexec.create_manager(log_level='info')

    qcgpjexec.add_task(eqi.Task(
        eqi.TaskType.EXECUTION,
        eqi.TaskRequirements(cores=1),
        application=exec_path
    ))
    qcgpjexec.run(processing_scheme=eqi.ProcessingScheme.EXEC_ONLY)
    qcgpjexec.terminate_manager()
else:
    #Local execution
    my_campaign.apply_for_each_run_dir(uq.actions.ExecuteLocal(exec_path))

# Collection of simulation outputs
my_campaign.collate()

# Post-processing analysis
analysis = uq.analysis.PCEAnalysis(sampler=my_sampler, qoi_cols=output_columns)
my_campaign.apply_analysis(analysis)

# Get results
results = my_campaign.get_last_analysis()

#  Graphics for Descriptive satatistics
corep = read(os.path.join(cpo_dir,  "ets_coreprof_in.cpo"), "coreprof")
rho = corep.rho_tor_norm
for i, qoi in enumerate(output_columns):
    results.plot_moments(qoi, xlabel="rho", xvalues=rho, filename="stats_"+str(i))
