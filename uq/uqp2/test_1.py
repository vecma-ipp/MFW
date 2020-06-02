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

from easymfw.utils.splines import *

print('>>> test UQ ETS-CHEASE: START')

# execustion with QCJ-PJ
EXEC_PJ = True

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

    exec_code = "ets_test"

    # CPO file containg initial values of uncertain params
    input_filename = "ets_coreprof_in.cpo"
    input_cponame = "coreprof"

    # The quantities of intersts and the cpo file to set them
    #output_columns = ["te", "ti", "ne", "ni", "psi"]
    output_columns = ["te.value"]
    output_filename = "ets_coreprof_out.cpo"
    output_cponame = "coreprof"

    # parameter space for campaign and the distributions list for the sampler
    input_cpo_file = os.path.join(cpo_dir, input_filename)
    params, vary = get_cpo_inputs(cpo_file=input_cpo_file,
                                  cpo_name=input_cponame,
                                  input_params=input_params)

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

    # Copy  exec file
    os.system("cp " + obj_dir +"/" + exec_code + " " + common_dir)

    # Create the encoder
    encoder = CPOEncoder(template_filename=input_filename,
                         target_filename=input_filename,
                         input_cponame=input_cponame,
                         common_dir=common_dir,
                         input_params=input_params)

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
    #  TODO : use indep. routine
    if EXEC_PJ:
        # PJ execution
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
        # Local execution
        my_campaign.apply_for_each_run_dir(uq.actions.ExecuteLocal(exec_path))

    my_campaign.collate()

    # Post-processing analysis
    analysis = uq.analysis.PCEAnalysis(sampler=my_sampler, qoi_cols=output_columns)
    my_campaign.apply_analysis(analysis)

    results = my_campaign.get_last_analysis()

    # Get Descriptive Statistics
    mean = {}
    #sob1 = {}
    dist = {}
    std = {}
    for qoi in output_columns:
        mean[qoi] = results['statistical_moments'][qoi]["mean"]
        std[qoi] = results['statistical_moments'][qoi]["std"]
        #sob1[qoi] = results['sobols_first'][qoi]
        dist = results['output_distributions'][qoi]

    return mean, std, dist

# EquilUpdatq + CHEASE: 2nd BOX
def equil(vary, params, input_params):

    exec_code = "equil_test"

    # The quantities of intersts and the cpo file to set them
    #output_columns = ["te", "ti", "ne", "ni", "psi"]
    output_columns = ["te.value"]
    output_filename = "ets_coreprof_out.cpo"
    output_cponame = "coreprof"

    # The quantities of intersts and the cpo file to set them
    output_columns = ["profiles_1d.gm3", "profiles_1d.pressure"]
    output_filename = "equil_equilibrium_out.cpo"
    output_cponame = "equilibrium"


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
    os.system("cp " +cpo_dir+ "/ets_coreprof_in_1.cpo " + common_dir+ "/equil_coreprof_in.cpo")
    os.system("cp " +cpo_dir+ "/ets_toroidfield_in.cpo " + common_dir+ "/equil_toroidfield_in.cpo")

    # Copy XML and XSD files
    os.system("cp " +xml_dir+ "/chease.xml " +common_dir)
    os.system("cp " +xml_dir+ "/chease.xsd " +common_dir)

    # Copy  exec file
    os.system("cp " + obj_dir +"/" + exec_code + " " + common_dir)

    # Create the encoder and get the app parameters
    input_filename = "equil_coreprof_in.cpo"
    input_cponame = "coreprof"
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

    return stat, sob1

# Main
if __name__ == "__main__":

    # Uncertain parameters
    input_params_1 = {
        "te.boundary.value": {
            "dist": "Normal",
            "err":  0.4,
        }
        ,
        "ti.boundary.value": {
            "dist": "Normal",
            "err": 0.4,
        }
    }

    # The 1st box: ETS
    mean_1, std_1, dist_1 = ets(input_params_1)

    def closet_rho(r, i):
        rc = r.flat[np.abs(r - i).argmin()]
        rl = r.tolist()
        ri = rl.index(rc)
        return rc, ri

    # The 2nd box: CHEASE
    #stats, sobols =  chease_test(dist)

    # PLOTS
    cp_file = cpo_dir + "/ets_coreprof_in.cpo"
    corep = read(cp_file, "coreprof")
    te = mean_1["te.value"]
    cpo = CPOElement(cp_file,  "coreprof")
    cpo.set_value("te.value", te.tolist())
    cpo.save("ets_coreprof_in_1.cpo")
    os.system("cp ets_coreprof_in_1.cpo " + cpo_dir)

    rho = corep.rho_tor_norm

    ty, cy = spl_fit(te, 6, 3)
    tx, cx = spl_fit(rho, 6, 3)

    dist_i = []
    rho_i = []
    te_i = []
    for x in cx:
        rc, ri = closet_rho(rho, x)
        rho_i.append(ri)
        dist_i.append(cp.Normal(te[ri], 0.2*te[ri]))
        te_i.append(te[ri])

    params_2 = {'te.value': {'type': 'list', 'default': te_i[3:]}}
    vary_2 = {'te.value': cp.J(*dist_i[3:])}

    input_params_2 = {
    "te.value": {
        "dist": "Normal",
        "err":  0.2,
        "idx": rho_i[3:]
    }
    }
    stat, sob1 = equil(vary_2, params_2, input_params_2)

