import os
import numpy as np
import easyvvuq as uq
# from ual
from ascii_cpo import read
# from current package
from base.xml_encoder import XMLEncoder
from base.cpo_decoder import CPODecoder
from base.utils import xml_inputs

'''
Perform UQ for the workflow Transport-Equilibrium-Turblence:
    ETS-CHEASE-GEM0.
Uncertainties are driven by:
    External sources of Electrons or/and Ions heating.
Method: Non intrusive with PCE.
'''


print('UQ LOOP_GEM0-SR: START')

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

# The execuatble code
obj_dir = os.path.abspath("../standalone/bin/"+SYS)
exec_code = "loop_gem0"

# Define the uncertain parameters
# Amplitude, Position and Width from source_dummy.xml
elec_heating_params = {
    "electrons.heating_el.WTOT_el":{
        "dist_name": "Uniform",
        "var_coeff": 0.2,
    }#,
    #"electrons.heating_el.RHEAT_el":{
    #    "dist_name": "Uniform",
    #    "var_coeff": 0.2,
    #},
    #"electrons.heating_el.FWHEAT_el":{
    #    "dist_name": "Uniform",
    #    "var_coeff": 0.2,
    #}
}
ions_heating_params = {
    "ions.heating.WTOT":{
        "dist_name": "Uniform",
        "var_coeff": 0.2,
    },
    "ions.heating.RHEAT_el":{
        "dist_name": "Uniform",
        "var_coeff": 0.2,
    },
    "ions.heating.FWHEAT":{
        "dist_name": "Uniform",
        "var_coeff": 0.2,
    }
}

# Choose one of the uncertain params dict, or merge them using
input_params = {}
input_params.update(elec_heating_params)
#input_params.update(ions_heating_params)

# XML file containg initial values of uncertain params
input_xml_filename = "source_dummy.xml"
input_xsd_filename = "source_dummy.xsd"

# The quantities of intersts and the cpo file to set them
output_columns = ["te.value"]
output_filename = "ets_coreprof_out.cpo"
output_cponame = "coreprof"

# Parameter dict for Campaign and distributions list for Sampler
params, vary = xml_inputs(xml_filename=input_xml_filename,
                          xsd_filename=input_xsd_filename,
                          input_dir=xml_dir,
                          input_params=input_params)

# Initialize Campaign object
test_case = cpo_dir.split('/')[-1]
campaign_name = "UQSR_loopGem0_"+test_case
my_campaign = uq.Campaign(name=campaign_name, work_dir=tmp_dir)

# Create new directory for inputs
campaign_dir = my_campaign.campaign_dir
common_dir = campaign_dir +"/common/"
os.mkdir(common_dir)

# Copy input CPO files
os.system("cp " + cpo_dir + "/ets_coreprof_in.cpo "    + common_dir)
os.system("cp " + cpo_dir + "/ets_equilibrium_in.cpo " + common_dir)
os.system("cp " + cpo_dir + "/ets_coreimpur_in.cpo "   + common_dir)
os.system("cp " + cpo_dir + "/ets_coretransp_in.cpo "  + common_dir)
os.system("cp " + cpo_dir + "/ets_toroidfield_in.cpo " + common_dir)

# Copy input XML and XSD files
os.system("cp " + xml_dir + "/ets.x* "    + common_dir)
os.system("cp " + xml_dir + "/chease.x* " + common_dir)
os.system("cp " + xml_dir + "/gem0.x* " + common_dir)
os.system("cp " + xml_dir + "/source_dummy.x* " + common_dir)

# Copy exec file
os.system("cp " + obj_dir +"/" + exec_code + " " + common_dir)
exec_path = os.path.join(obj_dir, exec_code)

# Create the encoder and the decoder
encoder = XMLEncoder(xml_filename=input_xml_filename,
                     input_dir=common_dir)

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

#  Graphics for Descriptive satatistics
cpo_file = os.path.join(cpo_dir, "ets_coreprof_in.cpo")
corep = read(cpo_file, "coreprof")
rho = corep.rho_tor_norm


# Save STATS into the csv file
header = 'rho_tor\t'
values = [rho]
for qoi in output_columns:

    mean = results.describe(qoi, 'mean')
    std = results.describe(qoi, 'std')
    values.append(list(mean))
    values.append(list(std))
    header = header + '\tmean_'+qoi + '\tstd_'+qoi

    np.savetxt('data/outputs/'+campaign_name+'STATS.csv',
               np.c_[values].T, delimiter='\t', header=header)

print('UQ LOOP_GEM0-SR: END')
