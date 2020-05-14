import os
import easyvvuq as uq
from ascii_cpo import read
from easymfw.templates.xml_encoder import XMLEncoder
from easymfw.templates.cpo_decoder import CPODecoder
from easymfw.utils.io_tools import get_xml_inputs

'''
Perform UQ for the workflow Transport-Equilibrium-Turblence.
Uncertainties are driven by:
    External sources of Electrons or/and Ions heating.
Method: Non intrusive with PCE.
Execution with QCG-Pilot Job.
'''


print('>>> UQ Workflow Sources w/PJ: START')

# execustion with QCJ-PJ
EXEC_PJ = True

# Machine name
SYS = os.environ['SYS']

# Working directory
tmp_dir = os.environ['SCRATCH']

# CPO files location
cpo_dir = os.path.abspath("../workflows/AUG_28906_6")
#cpo_dir = os.path.abspath("../workflows/JET_92436_23066")

# XML and XSD files location
xml_dir = os.path.abspath("../workflows")

# The execuatble code
obj_dir = os.path.abspath("../standalone/bin/"+SYS)
exec_code = "loop_gem0"

# Define the uncertain parameters
# Amplitude, Position and Width from source_dummy.xml
elec_heating_params = {
    "electrons.heating_el.WTOT_el":{
        "dist": "Uniform",
        "err": 0.2,
    },
    "electrons.heating_el.RHEAT_el":{
        "dist": "Uniform",
        "err": 0.2,
    }#,
    #"electrons.heating_el.FWHEAT_el":{
    #    "dist": "Uniform",
    #    "err": 0.2,
    #}
}
ions_heating_params = {
    "ions.heating.WTOT":{
        "dist": "Uniform",
        "err": 0.2,
    },
    "ions.heating.RHEAT_el":{
        "dist": "Uniform",
        "err": 0.2,
    },
    "ions.heating.FWHEAT":{
        "dist": "Uniform",
        "err": 0.2,
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
input_xml_file = os.path.join(xml_dir, input_xml_filename)
input_xsd_file = os.path.join(xml_dir, input_xsd_filename)
params, vary = get_xml_inputs(xml_file=input_xml_file,
                              xsd_file=input_xsd_file,
                              input_params = input_params)

# Initialize Campaign object
print('>>> Initialize Campaign object')
test_case = cpo_dir.split('/')[-1]
campaign_name = "UQ_SRC_"+test_case
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

# Create the encoder and get the app parameters
print('>>> Create the encoder')
encoder = XMLEncoder(template_filename=input_xml_filename,
                     target_filename=input_xml_filename,
                     input_params=input_params,
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
my_sampler = uq.sampling.PCESampler(vary=vary, polynomial_order=4,
                                    regression=False)
my_campaign.set_sampler(my_sampler)

# Will draw all (of the finite set of samples)
print('>>> Draw Samples - Ns = ', my_sampler._number_of_samples)
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

cpo_file = os.path.join(cpo_dir, "ets_coreprof_in.cpo")
corep = read(cpo_file, "coreprof")
rho = corep.rho_tor_norm

uparams_names = list(params.keys())

for qoi in output_columns:
    fig = campaign_name + qoi.split('.')[0]
    plots.plot_stats(rho, stat[qoi],
                     xlabel=r'$\rho_{tor}$', ylabel=qoi,
                     ftitle=qoi+' profile',
                     fname='data/outputs/STAT_'+fig)

    plots.plot_sobols_all(rho, sob1[qoi], uparams_names,
                      ftitle='1st Sobol indices: '+qoi,
                      fname='data/outputs/SA_'+fig)

# Save STATS into the csv file
#    import numpy as np
#    header = 'rho_tor\t'
#    values = [rho]
#    for qoi in output_columns:
#        values.append(list(stat[qoi]['mean']))
#        values.append(list(stat[qoi]['std']))
#        header = header + '\tmean_'+qoi + '\tstd_'+qoi
#
#    np.savetxt('data/outputs/'+campaign_name+'STATS.csv',
#               np.c_[values].T, delimiter='\t', header=header)
#
print('>>> UQ Workflow Sources: END')
