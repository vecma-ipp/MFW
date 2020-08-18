import os
import easyvvuq as uq
from easypj import Executor
from easypj import TaskRequirements, Resources
from easypj import Task, TaskType, SubmitOrder
from easymfw.templates.cpo_encoder import CPOEncoder
from easymfw.templates.cpo_decoder import CPODecoder
from easymfw.utils.io_tools import get_cpo_inputs

# Machine name
SYS = os.environ['SYS']
# Working directory
tmp_dir = os.environ['SCRATCH']
# CPO files location
cpo_dir = os.path.abspath("../workflows/AUG_28906_6_1ft_restart")
# Model file location
model_dir = os.path.abspath("data/models")
# surrogate model location
obj_dir = os.path.abspath("surr_p1")
exec_code = "model_predictor.py"


# BOX1 - GPR model by SKL
def setup_surrogate(common_dir, ft_index):
    # Define the uncertain parameters
    input_params = {
        "te.value": {
            "dist": "Normal",
            "err":  0.2,
            "ft_index": ft_index,
        },
        "te.ddrho": {
            "dist": "Normal",
            "err": 0.2,
            "ft_index": ft_index,
        },
        "ti.value": {
            "dist": "Normal",
            "err":  0.2,
            "ft_index": ft_index,
        },
        "ti.ddrho": {
            "dist": "Normal",
            "err": 0.2,
            "ft_index": ft_index,
        }
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

    # Copy input CPO files
    os.system("cp " + cpo_dir + "/ets_equilibrium_in.cpo "
              + common_dir + "gem_equilibrium_in.cpo")
    os.system("cp " + cpo_dir + "/ets_coreprof_in.cpo "
              + common_dir + "/gem_coreprof_in.cpo")
    # Copy output examples in surrigate model -- key difference
    os.system("cp " + cpo_dir + "/ets_coretransp_in.cpo "
              + surrogate_dir + "/gem_coretransp_out.cpo")

    # Create the encoder
    #print('>>> Create the encoder')
    input_filename = "gem_coreprof_in.cpo"
    encoder = CPOEncoder(template_filename=input_filename,
                         target_filename=input_filename,
                         input_cponame=input_cponame,
                         input_params=input_params,
                         common_dir=common_dir)

    # Create the decoder
    #print('>>> Create the decoder')
    decoder = CPODecoder(target_filename=output_filename,
                         output_columns=output_columns,
                         output_cponame=output_cponame)

    # Create a collation element for this campaign
    #print('>>> Create Collater')
    collater = uq.collate.AggregateSamples(average=False)

    # Create the sampler
    #print('>>> Create the sampler')
    sampler = uq.sampling.PCESampler(vary=vary,
                                     polynomial_order=2)

    #print('>>> Create post-processing analysis')
    analysis = uq.analysis.PCEAnalysis(sampler=sampler,
                                       qoi_cols=output_columns)

    return params, encoder, decoder, collater, sampler, analysis, output_columns


def exec_pj(campaign, app_path):
    qcgpjexec = Executor()
    qcgpjexec.create_manager(dir=campaign.campaign_dir, log_level='info')

    qcgpjexec.add_task(Task(
        TaskType.EXECUTION,
        TaskRequirements(cores=Resources(exact=1)),
        application=exec_path
    ))
    qcgpjexec.run(
        campaign=campaign,
        submit_order=SubmitOrder.EXEC_ONLY
    )
    qcgpjexec.terminate_manager()


def res_postprocess(result, output_columns=[]):
    # Get Descriptive Statistics
    print('Get Descriptive Statistics: \n')
    stat = {}
    sob1 = {}
    for qoi in output_columns:
        stat[qoi] = result['statistical_moments']
        sob1[qoi] = result['sobols_first']

        print(qoi)
        print('Stats: \n', stat[qoi])
        print('Sobol 1st: \n', sob1[qoi])


if __name__ == "__main__":

    # Initialize Campaign object
    campaign_name = "UQ_GEM_SURROGATE_"
    campaign = uq.Campaign(name=campaign_name, work_dir=tmp_dir)

    # Create new directory for inputs
    campaign_dir = campaign.campaign_dir
    common_dir = campaign_dir + "/common/"
    os.mkdir(common_dir)

    # Copy model file -- key difference
    surrogate_dir = campaign_dir + "/common/surrogate/"
    os.mkdir(surrogate_dir)
    os.system("cp " + model_dir + "/gpr_gem_1.joblib "
              + surrogate_dir + "/GPR.joblib")

    # Copy python script file -- key difference
    os.system("cp " + obj_dir + "/" + exec_code + " " + common_dir)
    exec_path = os.path.join(common_dir, exec_code)

    # location of the flux tubes
    flux_indices = 69

    (params, encoder, decoder, collater, sampler, analysis, qois) = setup_surrogate(common_dir, flux_indices)

    campaign.add_app(name="surrogate-test",
                     params=params,
                     encoder=encoder,
                     decoder=decoder,
                     collater=collater)

    # Set and run the surrigate model campaign
    campaign.set_app("surrogate-test")
    campaign.set_sampler(sampler)
    campaign.draw_samples()
    campaign.populate_runs_dir()

    print(">>> Starting PJ execution")
    exec_pj(campaign, exec_path)
    campaign.collate()
    campaign.apply_analysis(analysis)

    #print('>>> Post-processing analysis')
    result = campaign.get_last_analysis()
    res_postprocess(result, qois)
