import os
import time
import easyvvuq as uq
# EasyVVUQ/QCG-PJ
import eqi
# from ual
from ascii_cpo import read
# from current package
from base.cpo_encoder import CPOEncoder
from base.xml_encoder import XMLEncoder
from base.cpo_decoder import CPODecoder
from base.utils import cpo_inputs, xml_inputs

'''
Perform UQ for the workflow ETS + CHEASE.
Uncertainties are driven by:
- External sources of Electrons heating.
- Boundary condition in Plasma Edge of electrons tempurature.
'''

print('UQ ETS-CHEASE: START')

t0 = time.time()

# Eexecution with QCG-PJ
EXEC_PJ = True

# Machine name
SYS = os.environ['SYS']

# Working directory
tmp_dir = os.environ['SCRATCH']

# CPO files
cpo_dir = os.path.abspath("../workflows/AUG_28906_6")

# XML and XSD files
xml_dir = os.path.abspath("../workflows")

# The executable code to run
obj_dir = os.path.abspath("../standalone/bin/"+SYS)
exec_code = "ets_chease_test"

# Define the uncertain parameters
# Electron boudary condition
input_params_bc = {
    "te.boundary.value": {
        "dist_name": "Normal",
        "var_coeff":  0.2,
    }
}
# Electron heating Sources
input_params_sr = {
    "electrons.heating_el.WTOT_el":{
        "dist_name": "Uniform",
        "var_coeff": 0.2,
    },
    "electrons.heating_el.RHEAT_el":{
        "dist_name": "Uniform",
        "var_coeff": 0.2,
    },
    "electrons.heating_el.FWHEAT_el":{
        "dist_name": "Uniform",
        "var_coeff": 0.2,
    }
}

# CPO and XML files containg initiail values of uncertain params
input_cpo_filename = "ets_coreprof_in.cpo"
input_cponame = "coreprof"
input_xml_filename = "source_dummy.xml"
input_xsd_filename = "source_dummy.xsd"

# The quantities of intersts and the cpo file to set them
output_columns = ["profiles_1d.pressure"]
output_filename = "chease_equilibrium_out.cpo"
output_cponame = "equilibrium"

# params: the parameter space for campaign object
# vary: distributions list for the sampler
params_cpo, vary_cpo = cpo_inputs(cpo_filename=input_cpo_filename,
                                  cpo_name=input_cponame,
                                  input_dir=cpo_dir,
                                  input_params=input_params_bc)

params_xml, vary_xml = xml_inputs(xml_filename=input_xml_filename,
                                  xsd_filename=input_xsd_filename,
                                  input_dir=xml_dir,
                                  input_params=input_params_sr)

# Merge the params dict
params = {**params_cpo, **params_xml}
vary = {**vary_cpo, **vary_xml}

# Initialize Campaign object
campaign_name = "UQ-BCSR_ETS-CHEASE_"
my_campaign = uq.Campaign(name=campaign_name, work_dir=tmp_dir)

# Create new directory for inputs
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
os.system("cp " + xml_dir + "/ets.xml "    + common_dir)
os.system("cp " + xml_dir + "/ets.xsd "    + common_dir)
os.system("cp " + xml_dir + "/chease.xml " + common_dir)
os.system("cp " + xml_dir + "/chease.xsd " + common_dir)
os.system("cp " + xml_dir + "/source_dummy.xml " + common_dir)
os.system("cp " + xml_dir + "/source_dummy.xsd " + common_dir)


# Copy  exec file
os.system("cp " + obj_dir +"/" + exec_code + " " + common_dir)
exec_path = os.path.join(obj_dir, exec_code)

t1 = time.time()

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

# Add the app (automatically set as current app)
my_campaign.add_app(name=campaign_name,
                    params=params,
                    encoder=encoder,
                    decoder=decoder)

t2 = time.time()

# Create the sampler
my_sampler = uq.sampling.PCESampler(vary=vary, polynomial_order=3)
my_campaign.set_sampler(my_sampler)

# Will draw all (of the finite set of samples)
my_campaign.draw_samples()

t3 = time.time()

# Encoding
my_campaign.populate_runs_dir()

t4 = time.time()

# Running
qcgpjexec = eqi.Executor(my_campaign)
qcgpjexec.create_manager(log_level='info')

qcgpjexec.add_task(eqi.Task(
    eqi.TaskType.EXECUTION,
    eqi.TaskRequirements(cores=1),
    application=exec_path
))
qcgpjexec.run(processing_scheme=eqi.ProcessingScheme.EXEC_ONLY)
qcgpjexec.terminate_manager()

t5 = time.time()

# Collating
my_campaign.collate()

t6 = time.time()

# Post-processing analysis
analysis = uq.analysis.PCEAnalysis(sampler=my_sampler, qoi_cols=output_columns)
my_campaign.apply_analysis(analysis)

# Get results
results = my_campaign.get_last_analysis()

t7 = time.time()

# Get Descriptive Statistics
equil_file = os.path.join(cpo_dir, "ets_equilibrium_in.cpo")
equil = read(equil_file, "equilibrium")
rho = equil.profiles_1d.rho_tor

uparams_names = list(params.keys())

t8 = time.time()

print('Time for initializing = %.3f' %(t1-t0))
print('Time for initializing Campaign = %.3f' %(t2-t1))
print('Time for sampling = %.3f' %(t3-t2))
print('Time for populating  = %.3f' %(t4-t3))
print('Time for running  = %.3f' %(t5-t4))
print('Time for collating = %.3f' %(t6-t5))
print('Time for analysis = %.3f' %(t7-t6))
print('Time for plotting  = %.3f' % (t8-t7))

print('UQ ETS-CHEASE: END')
