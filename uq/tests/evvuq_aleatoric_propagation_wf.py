import os

import numpy as np
import pandas as pd

from itertools import product

import chaospy as cp

# from easyvvuq
import easyvvuq as uq
from easyvvuq.actions import Encode, Decode, Actions, CreateRunDirectory, ExecuteQCGPJ, ExecuteLocal, ExecuteSLURM, QCGPJPool
from easyvvuq.actions.execute_qcgpj import EasyVVUQParallelTemplate

# form qcg-pj
from qcg.pilotjob.executor_api.qcgpj_executor import QCGPJExecutor

# from current package
from base.evvuq_partemplate_wenv import EasyVVUQParallelTemplateWithEnv
from base.transport_csv_encoder import TransportCSVEncoder
from base.profile_cpo_decoder import ProfileCPODecoder

def input_params_stub_perft(nfts=8):
    """
    Define the uncertain parameters
    Returns:   dict: uncertain parameters
        Creates default parameters for heat flux value distribution: N(mu, sigma)
    """

    input_params_dict = { f"ft{i}": {
                "Qe": {"dist": "Normal", "mu": 2.5E4+1.5E4*i, "sigma": 2.5E3},
                "Qi": {"dist": "Normal", "mu": 2.5E4+1.5E4*i, "sigma": 2.5E3}
                                    }
                          for i in range(nfts)}

    return input_params_dict

def input_params_stub_batch(nfts=8):
    """
    Define the uncertain parameters
    Returns:   dict: uncertain parameters
        Creates default parameters for heat flux value distribution: N(mu, sigma)
    """
    species = ['e', 'i']
    fts = [i for i in range(nfts)]

    input_params_dict = {
                "Q{sp}_{ft}": {"dist": "Normal", "mu": 2.5E4+1.5E4*ft, "sigma": 2.5E3}
                        for ft,sp in product(range(nfts), species)}

    return input_params_dict

def input_params_stub_relative(nfts=8):
    """
    Define the uncertain parameters
    Returns:   dict: uncertain parameters
        Creates default parameters for heat flux value distribution: N(mu, sigma)
    """
    species = ['e', 'i']
    fts = [i for i in range(nfts)]

    input_params_dict = {
                f"Q{sp}_{ft}": {"dist": "Normal", "mu":0.0, "sigma": 0.5*(ft+1)*1E-1}
                        for ft,sp in product(fts, species)}

    return input_params_dict

def exec_pj(campaign, exec_path, ncores, nnodes=1, mpi_instance='mpiexec', log_level="debug"):

    exec_res = 0

    template_par_short = {
                    'numCores': ncores,
                    'numNodes': nnodes,
                    'model': mpi_model,
                         }
    
    try:
        print("Creating resource pool")

        with QCGPJPool(
                qcgpj_executor=QCGPJExecutor(),
                template=EasyVVUQParallelTemplateWithEnv(),
                template_params=template_par_short,
                ) as qcgpj:
            
            print('> Executing jobs and collating results')
            exec_res = campaign.execute(pool=qcgpj)
            exec_res.collate()

    except Exception as e:
        
        print('!>> Exception during batch execution! :')
        print(e)
    
    return exec_res

def exec_pj_no_templ(campaign,):

    exec_res = 0

    try:
        print(">> Creating resource pool")

        with QCGPJPool(
                #qcgpj_executor=QCGPJExecutor(),
                template=EasyVVUQParallelTemplate(),
                template_params={'numCores':1},
                ) as qcgpj:
            
            print(f">> Executing on a HPC machine and collating results")
            
            campaign.execute(pool=qcgpj).collate()
            exec_res = campaign.get_collation_result()

    except Exception as e:
        
        print('!>> Exception during batch execution! :')
        print(e)
    
    return exec_res

def prepare_results(result, output_columns):

    moment_list = ['mean', 'std', 'skewnewss', 'kurtosis'] # TODO lookup eUQ docu

    moments = {{moment: result.describe(qoi, moment) for moment in moment_list} for qoi in output_columns}

    print(f"QoI moments:\n{moments}")

    return moments

if __name__ == "__main__":

    # Global params
    print(f"> Reading environment variables")
    SYS = os.environ['SYS']
    mpi_instance =  os.environ['MPICMD']
    mpi_model = os.environ['MPIMOD']
    wrk_dir = tmp_dir = os.environ['SCRATCH']

    # - option 1 - use PCE
    #p = int(os.environ['POLORDER'])
    # - option 2 - use MC
    n_samples = int(os.environ['NSAMPLES'])

    base_dataset_filename = "gem0py_new_baseline.csv"
    target_dataset_filename = "gem0py_new_local.csv"
    output_filename = "ets_coreprof_out.cpo"

    test_script_dir = "tests"

    ### Uncertain input definition
    # 2[fluxes] * 8[flux-tubes] (flux tubes are independent!)
    # Normal PDF for each: mu, sigma
    print(f"> Making UQ run parameters: i/o, parallelisation")
    # Choice: do not have tensor product over flux tubes, treat them in batch
    input_params_propr = input_params_stub_relative()
    
    # TODO assumes discription["dist"] == "Normal"
    # Turn input param description into 'vary'
    vary = {key: cp.Normal(description['mu'], description['sigma']) for key,description in input_params_propr.items()}

    # Turn input param description into eUQ param dict
    sigma_lim = 3.0
    input_params = {key: {
        "type": "float", 
        "default": description['mu'],
        #"min": description['mu'] - sigma_lim*description['sigma'],
        #"max": description['mu'] + sigma_lim*description['sigma'],
                       } for key,description in input_params_propr.items()}

    ft_coords = [0.143587306141853 , 0.309813886880875 , 0.442991137504578 , 0.560640752315521 , 0.668475985527039 , 0.769291400909424 , 0.864721715450287 , 0.955828309059143]

    # Output columns
    output_columns = ["te_value_0", "ti_value_0"]

    ### Define parallelisation paramaters
    #    e.g. surrogate: t_s ~= 10m. ; workflow t_w ~= 10m. ; buffer/overhead: t_b ~= 10m 
    #    t_wf ~= 0.5h, nodes:1 , cores:40, t_tot = 24h
    if SYS == 'MARCONI':
        n_cores_p_node = 48
    elif SYS == 'COBRA':
        n_cores_p_node = 40
    else:
        n_cores_p_node = 32
        print("HPC Machine unknown, assuming {n_cores_p_node} cores per node")

    nnodes = 1
    ncores = 40

    nparams = 2

    # - option 1 - PCE
    #nruns = p**2
    # - option 2 - MC
    nruns = n_samples

    nnodes_tot = nnodes
    ncores_tot = nnodes * n_cores_p_node

    print('> {2} Runs requiring totally {0} cores at {1} nodes'.format(ncores_tot, nnodes_tot, nruns))

    ### Pepare the campaign: crate the campaign object, copy the right files to the right places
 
    # Create a new campaign
    print(f"> Creating a new campaign")
    campaign = uq.Campaign(name='UQ_8FTGEM0_WF_AL_', work_dir=wrk_dir)

    common_dir = campaign.campaign_dir +"/common/"
    os.mkdir(common_dir)

    # baseline training dataset (and other surrogate files)
    print(f"> Copying data for surrogates")
    surrogate_data_dir = os.path.abspath("basicda")
    os.system(f"cp {surrogate_data_dir}/{base_dataset_filename} {common_dir}/{base_dataset_filename}")
    surrogate_script_dir = os.path.abspath("../../EasySurrogate/tests/gem_gp")
    surrogate_scripts = ['process_gpr_ind.sh', 'gem_data_ind.py', 'train_model_ind.py', 'test_model_ind.py']
    for filename in surrogate_scripts:
        os.system(f"cp {surrogate_script_dir}/{filename} {common_dir}/") 
        #TODO else?

    # initial state for the M3WF (this is done inside *.sh file)
    print(f"> Copying data for simulations")

    simulation_data_dir  = os.path.abspath("../muscle3")
    os.system(f"cp {simulation_data_dir}/read_profs.py {common_dir}/")

    # files for configuration of workflow and its componenrts
    sim_nec_dir = 'workflow'
    sim_nec_files = ['gem_surr_workflow_independent.sh', 'gem-surr-mft-fusion-independent.ymmsl', 'ets.xml', 'ets.xsd', 'chease.xml', 'chease.xsd', 'gem0.xml', 'gem0.xsd']
    for filename in sim_nec_files:
        os.system(f"cp {simulation_data_dir}/{sim_nec_dir}/{filename} {common_dir}/") 

    # files for comparison of states
    os.system(f"cp {surrogate_data_dir}/compare_workflow_states.py {common_dir}/")
    os.system(f"cp {surrogate_data_dir}/gem0wf_stst/ets_coreprof_out.cpo {common_dir}/ets_coreprof_stst.cpo")

    # files for 'initial conditions' of the workflow
    init_cpo_list = ['ets_coreprof_in.cpo', 'ets_equilibrium_in.cpo', 'ets_coretransp_in.cpo', 'ets_toroidfield_in.cpo', 'ets_coresource_in.cpo', 'ets_coreimpur_in.cpo']
    simulation_cpo_dir = "workflow/gem0_surr_resume_data"
    for initcpo in init_cpo_list:
        os.system(f"cp {simulation_data_dir}/{simulation_cpo_dir}/{initcpo} {common_dir}/")

    ### Encoders
    # Sample from PDF: flux perturbation [Ind. variable] -> code of turbulence model variation
    # TODO consider two options: overwrighting flux values completely or using value in the file to modify it
    print("> Creating Encoder")
    encoder = TransportCSVEncoder(csv_filename=base_dataset_filename, 
                                  input_dir=common_dir,
                                  target_filename=target_dataset_filename)

    ### Decoders
    # Code of M3-WF run (foldername) -> [QoI] (ion) temperature @rho_tor_norm=0
    print("> Creating Decoder")
    output_cpo_path = ''
    decoder = ProfileCPODecoder(cpo_filename=output_filename, 
                                cpo_path=output_cpo_path,
                                output_columns=output_columns)

    ### Actions
    # Shell script to: flux perturbation -> labels for data set -> traning data -> surrogate -> M3-WF run -> final CPO files
    print("> Creating Actions")
    exec_code = "workflow.sh" #TODO modify the retrainer script
    os.system(f"cp {test_script_dir}/{exec_code} {common_dir}/")
    exec_path = os.path.join(common_dir, exec_code)
    exec_path_comm = exec_path

    actions = Actions(
        CreateRunDirectory('/runs'),
        Encode(encoder),
        ExecuteLocal(exec_path_comm),
        Decode(decoder),
                     )

    # Sampler
    print("> Creating Sampler")
    # - option 1 - PCE
    #sampler = uq.sampling.PCESampler(vary=input_params, polynomial_order=p)
    # - option 2 - MC 
    print(f">>> Using {n_samples} samples") ###DEBUG
    sampler = uq.sampling.MCSampler(vary=vary, n_mc_samples=n_samples,)

    # Analysis
    print("> Creating Analysis")
    # - option 1 - PCE
    #analysis = uq.analysis.PCEAnalysis(sampler=sampler, qoi_cols=output_columns)
    # - option 2 - MC
    analysis = uq.analysis.BasicStats()

    ### Run EasyVVUQ campaign

    # QCJ-PG specs
    # eUQ campaign
    print(f"> Setting Campaign parameters")
    camp_name = "UQ_8FTGEM0_WF_AL_"

    campaign.add_app(name=camp_name,
                     params=input_params,
                     actions=actions)
    
    campaign.set_app(camp_name)
    campaign.set_sampler(sampler)

    print("> Exectung the Actions!")
    #TODO are ncores and nnodes needed for one job or all jobs?
    #exec_res = exec_pj(campaign, exec_path_comm, ncores, nnodes, mpi_instance)
    exec_res = exec_pj_no_templ(campaign)

    ### Result analysis
    print(f"> Performing Analysis")
    campaign.apply_analysis(analysis)
    result = campaign.get_last_analysis()

    # Moments of QoI: AVG, STD, SCW, KRT
    print(result)
