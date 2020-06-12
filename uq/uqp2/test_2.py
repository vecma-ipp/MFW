import os
import easyvvuq as uq
from ascii_cpo import read
from easymfw.templates.cpo_encoder import CPOEncoder
from easymfw.templates.cpo_decoder import CPODecoder
from easymfw.templates.xml_encoder import XMLEncoder
from easymfw.utils.io_tools import get_xml_inputs
from easymfw.utils.io_tools import get_cpo_inputs
# GCG-PJ wrapper
import easypj
from easypj import TaskRequirements, Resources
from easypj import Task, TaskType, SubmitOrder


print('>>> test ETS/EQUIL: START')

# Machine name
SYS = os.environ['SYS']

# Working directory
tmp_dir = os.environ['SCRATCH']

# CPO files location
#cpo_dir = os.path.abspath("../workflows/AUG_28906_6")
cpo_dir = os.path.abspath("../workflows/AUG_28906_6_BgB")

# XML and XSD files
xml_dir = os.path.abspath("../workflows")

# The executable code to run
obj_dir = os.path.abspath("../standalone/bin/"+SYS)

# ETS: the 1st BOX
def ets(input_params):

    exec_code = "ets_src"

    # XML file containg initial values of uncertain params
    input_xml_filename = "source_dummy.xml"
    input_xsd_filename = "source_dummy.xsd"

    # The quantities of intersts and the cpo file to set them
    #output_columns = ["te", "ti", "ne", "ni", "psi"]
    output_columns = ["te.value"]
    output_filename = "ets_coreprof_out.cpo"
    output_cponame = "coreprof"

    # parameter space for campaign and the distributions list for the sampler
    input_xml_file = os.path.join(xml_dir, input_xml_filename)
    input_xsd_file = os.path.join(xml_dir, input_xsd_filename)
    params, vary = get_xml_inputs(xml_file=input_xml_file,
                              xsd_file=input_xsd_file,
                              input_params = input_params)

    # Initialize Campaign object
    campaign_name = "UQETS_"
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
    os.system("cp " + xml_dir + "/source_dummy.xml " + common_dir)
    os.system("cp " + xml_dir + "/source_dummy.xsd " + common_dir)

    # Copy  exec file
    os.system("cp " + obj_dir +"/" + exec_code + " " + common_dir)

    # Create the encoder
    encoder = XMLEncoder(template_filename=input_xml_filename,
                         target_filename=input_xml_filename,
                         input_params=input_params,
                         common_dir=common_dir)

    # Create the encoder
    decoder = CPODecoder(target_filename=output_filename,
                         output_columns=output_columns,
                         output_cponame=output_cponame)

    # Create a collation element for this campaign
    collater = uq.collate.AggregateSamples(average=False)

    # Add the app (automatically set as current app)
    my_campaign.add_app(name=campaign_name,
                        params=params,
                        encoder=encoder,
                        decoder=decoder,
                        collater=collater)

    # Create the sampler
    my_sampler = uq.sampling.PCESampler(vary=vary,
                                        polynomial_order=3,
                                        regression=True)
    my_campaign.set_sampler(my_sampler)

    # Will draw all (of the finite set of samples)
    my_campaign.draw_samples()

    my_campaign.populate_runs_dir()

    exec_path = os.path.join(common_dir, exec_code)
    my_campaign.apply_for_each_run_dir(uq.actions.ExecuteLocal(exec_path))

    my_campaign.collate()

    # Post-processing analysis
    analysis = uq.analysis.PCEAnalysis(sampler=my_sampler, qoi_cols=output_columns)
    my_campaign.apply_analysis(analysis)

    results = my_campaign.get_last_analysis()

    # Get Descriptive Statistics
    mean = {}
    dist = {}
    std = {}
    for qoi in output_columns:
        mean[qoi] = results['statistical_moments'][qoi]["mean"]
        std[qoi] = results['statistical_moments'][qoi]["std"]
        dist = results['output_distributions'][qoi]

    return mean, std, dist

# EquilUpdatq + CHEASE: 2nd BOX
def equil():
    EXEC_PJ = True

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
    input_filename = "equil_coreprof_in.cpo"
    encoder = CPOEncoder(template_filename=input_filename,
                         target_filename=input_filename,
                         input_cponame=input_cponame,
                         input_params=input_params,
                         common_dir=common_dir)

    # Create the decoder
    decoder = CPODecoder(target_filename=output_filename,
                         output_columns=output_columns,
                         output_cponame=output_cponame)

    # Create a collation element for this campaign
    collater = uq.collate.AggregateSamples(average=False)

    # Add the ETS app (automatically set as current app)
    my_campaign.add_app(name=campaign_name,
                        params=params,
                        encoder=encoder,
                        decoder=decoder,
                        collater=collater)

    # Create the sampler
    my_sampler = uq.sampling.PCESampler(vary=vary,
                                        polynomial_order=3,
                                        regression=True)
    my_campaign.set_sampler(my_sampler)

    # Will draw all (of the finite set of samples)
    my_campaign.draw_samples()

    my_campaign.populate_runs_dir()

    exec_path = os.path.join(common_dir, exec_code)
    if EXEC_PJ:
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
        my_campaign.apply_for_each_run_dir(uq.actions.ExecuteLocal(exec_path))

    my_campaign.collate()

    # Post-processing analysis
    analysis = uq.analysis.PCEAnalysis(sampler=my_sampler, qoi_cols=output_columns)
    my_campaign.apply_analysis(analysis)

    results = my_campaign.get_last_analysis()

    # Get Descriptive Statistics
    stat = {}
    sob1 = {}
    dist = {}
    for qoi in output_columns:
        stat[qoi] = results['statistical_moments'][qoi]
        sob1[qoi] = results['sobols_first'][qoi]


# Main
if __name__ == "__main__":

    # Uncertain parameters
    input_params_1 = {
        "electrons.heating_el.WTOT_el":{
        :q

            "dist": "Uniform",
            "err": 0.2,
        },
        "electrons.heating_el.RHEAT_el":{
            "dist": "Uniform",
            "err": 0.2,
        },
        "electrons.heating_el.FWHEAT_el":{
            "dist": "Uniform",
            "err": 0.2,
        }
    }

    # The 1st box: ETS
    mean_1, std_1, dist_1 = ets(input_params_1)

    # The 2nd box: CHEASE
    #stats, sobols =  chease_test(dist)

    # PLOTS
    cp_file = cpo_dir + "/ets_coreprof_in.cpo"
    corep = read(cp_file, "coreprof")

    rho = corep.rho_tor_norm

