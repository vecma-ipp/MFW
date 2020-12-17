import os
import easyvvuq as uq
# from ual
from ascii_cpo import read
# from current package
from base.cpo_encoder import CPOEncoder
from base.xml_encoder import XMLEncoder
from base.cpo_decoder import CPODecoder
from base.utils import cpo_inputs, xml_inputs

'''
Perform UQ for the workflow Transport-Equilibrium-Turblence:
    ETS-CHEASE-GEM0.
Uncertainties are driven by:
    External sources of Electrons heating.
    Boundary conditions (Plasma Edge) of electrons tempurature.
UQ Method: Non intrusive (UQP1) with PCE.
'''


print('UQ-Workflow LOOP-BC_SR: START')

# execustion with QCG-PJ
EXEC_PJ = True

# Machine name
SYS = os.environ['SYS']

# Working directory
tmp_dir = os.environ['SCRATCH']

# CPO files location
cpo_dir = os.path.abspath("../workflows/AUG_28906_6")

# XML and XSD files location
xml_dir = os.path.abspath("../workflows")

# The execuatble code to run
obj_dir = os.path.abspath("../standalone/bin/"+SYS)
exec_code = "loop_gem0"

# Define the uncertain parameters
# Electrons boudary condition
input_params_bc = {
    "te.boundary.value": {
        "dist_name": "Normal",
        "var_coeff":  0.2,
    }
}
# Gaussian Sources: Electrons heating
input_params_sr = {
    "electrons.heating_el.WTOT_el":{
        "dist_name": "Uniform",
        "var_coeff": 0.2,
    },
    "electrons.heating_el.RHEAT_el":{
        "dist_name": "Uniform",
        "var_coeff": 0.2,
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

# params: the parameter space for campaign object
# vary: adistributions list for the sampler
params_cpo, vary_cpo = cpo_inputs(cpo_filename=input_cpo_filename,
                                  cpo_name=input_cponame,
                                  input_dir=cpo_dir,
                                  input_params=input_params_bc)

params_xml, vary_xml = xml_inputs(xml_filename=input_xml_filename,
                                  xsd_filename=input_xsd_filename,
                                  input_dir=xml_dir,
                                  input_params=input_params_sr)

# Merge the params and vary
params = {**params_cpo, **params_xml}
vary = {**vary_cpo, **vary_xml}

# Initialize Campaign object
campaign_name = "UQ-BCSR_loopGem0_"
my_campaign = uq.Campaign(name=campaign_name, work_dir=tmp_dir)

# Create new directory for commons inputs
campaign_dir = my_campaign.campaign_dir
common_dir = campaign_dir +"/common/"
os.mkdir(common_dir)

# Copy input CPO files in common directory
os.system("cp " + cpo_dir + "/ets_coreprof_in.cpo "    + common_dir)
os.system("cp " + cpo_dir + "/ets_equilibrium_in.cpo " + common_dir)
os.system("cp " + cpo_dir + "/ets_coreimpur_in.cpo "   + common_dir)
os.system("cp " + cpo_dir + "/ets_coretransp_in.cpo "  + common_dir)
os.system("cp " + cpo_dir + "/ets_toroidfield_in.cpo " + common_dir)

# Copy XML and XSD files
os.system("cp " + xml_dir + "/ets.x* "    + common_dir)
os.system("cp " + xml_dir + "/chease.x* " + common_dir)
os.system("cp " + xml_dir + "/gem0.x* "   + common_dir)
os.system("cp " + xml_dir + "/source_dummy.x* " + common_dir)


# Copy exec file
os.system("cp " + obj_dir +"/" + exec_code + " " + common_dir)
exec_path = os.path.join(obj_dir, exec_code)

# Create the encoders
# params_names dict is given here because we will use MultiEncoder
encoder_cpo = CPOEncoder(cpo_filename=input_cpo_filename,
                         cpo_name=input_cponame,
                         input_dir=common_dir,
                         input_params=input_params_bc)

encoder_xml = XMLEncoder(xml_filename=input_xml_filename,
                         input_dir=common_dir,
                         input_params=input_params_sr)

# Combine both encoders into a single encoder
encoder = uq.encoders.MultiEncoder(encoder_cpo, encoder_xml)

# Create the encoder
decoder = CPODecoder(cpo_filename=output_filename,
                     cpo_name=output_cponame,
                     output_columns=output_columns)

# Add the ETS app (automatically set as current app)
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

# Get Descriptive Statistics
input_cpo_file = os.path.join(cpo_dir, input_cpo_filename)
corep = read(input_cpo_file, "coreprof")
rho = corep.rho_tor_norm

print('UQ-Workflow LOOP-BC_SR: START')
