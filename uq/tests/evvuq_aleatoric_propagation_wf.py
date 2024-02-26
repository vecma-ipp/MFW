import os
import sys

import numpy as np
import pandas as pd

from itertools import product

import pickle

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
from base.plots import plot_moments, plot_sobols
from base.utils import cpo_inputs, ftube_indices

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

def input_params_stub_relative(nfts=8,):
    """
    Define the uncertain parameters
    Returns:   dict: uncertain parameters
        Creates default parameters for heat flux value distribution: N(mu, sigma)
    """
    species = ['e', 'i']
    fts = [i for i in range(nfts)]

    # sigma in fact means coefficient of variation here
    input_params_dict = {
                f"Q{sp}_{ft}": {"dist": "Normal", "mu": 0.0, "sigma": 0.5*(ft+1)*1E-1}
                        for ft,sp in product(fts, species)}

    return input_params_dict

def input_params_scan_relative(nfts=8, cov=0.1):
    """
    Define the uncertain parameters
    Accept CoV of the (aleatoric flux) uncertainties as input argument
    Returns:   dict: uncertain parameters
        Creates default parameters for heat flux value distribution: N(mu, sigma)
    """
    species = ['e', 'i']
    fts = [i for i in range(nfts)]

    # sigma in fact means coefficient of variation here
    input_params_dict = {
                f"Q{sp}_{ft}": {"dist": "Normal", "mu": 0.0, "sigma": cov}
                        for ft,sp in product(fts, species)}

    return input_params_dict

def input_params_gem_scan(nfts=8):
    """
    Define the uncertain parameters
    Uses data from GEM UQ campaign run 'csldevnei'
    Returns:   dict: uncertain parameters
        Creates default parameters for heat flux value distribution: N(mu, sigma)
    """

    species = ['e', 'i']
    fts = [i for i in range(nfts)]

    # Next list is taken as AVERAGES from recorded analysis of resuq_main_ti_transp_flux_all_csldvnei_43.csv (GEM data for 8ft*4params*3abcissas)
    gem_transp_flux_covs = {
    'ti': [0.2500634474332664, 0.0450563453261807, 0.09314760909732511, 0.04813051952201777, 0.0409145110215701, 0.04812928853007205, 0.06868580222134828, 0.11021902993747867],
    'te': [0.2370463147118546, 0.042502557708595665, 0.08202910452564564, 0.04455334874318351, 0.04031436777758668, 0.04840126342067821, 0.06963293699469307, 0.11072505485271966],
    }

    # sigma in fact means coefficient of variation here
    input_params_dict = {
                f"Q{sp}_{ft}": {"dist": "Normal", "mu": 0.0, "sigma": gem_transp_flux_covs[f"t{sp}"][ft]}
                        for ft,sp in product(range(nfts), species)}

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

def plot_results(result, input_params, output_columns):

    # To store Statistics and Sobols
    means = {qoi: [] for qoi in output_columns}
    stds = {qoi: [] for qoi in output_columns}
    sob1 = {qoi: [] for qoi in output_columns}
    sobt = {qoi: [] for qoi in output_columns}

    # Store results - TODO has to be done on per f.t. basis
    for qoi in output_columns:
        means[qoi].append(result.describe(qoi, 'mean'))
        stds[qoi].append(result.describe(qoi, 'std'))
        s1 = {}
        st = {}
        for par in list(input_params.keys()):
            s1.update({par: result.sobols_first(qoi)[par]})
            st.update({par: result.sobols_total(qoi)[par]})
        sob1[qoi].append(s1)
        sobt[qoi].append(st)

    # Plot the results
    for i, qoi in enumerate(output_columns):
        plot_moments(means[qoi], stds[qoi],
                     xlabel="Flux tubes", ylabel=qoi,
                     ftitle="GEM0SUR-WF: descriptive statistics for "+qoi,
                     fname="aleatoric_stats"+str(i)+".png")

        #TODO: for radially well-resolved quantities, plot mean+shaded ~2*sigma
        #TODO: if KDE exists, plot KDE for ti_value_0, te_value_0
        for j in range(8):
                plot_sobols(sob1[qoi][j],
                            xlabel="Uncertain parameters", ylabel="First sobol",
                            ftitle='GEM0SUR-WF: First sobol indices \n Flux tube: '+str(j+1)+' - QoI: '+qoi,
                            fname='gem0surrwf_al_sob1_'+str(i)+str(j)+'.png')
                plot_sobols(sobt[qoi][j],
                            xlabel="Uncertain parameters", ylabel="Total sobol",
                            ftitle='GEM0SUR-WF: Total sobol indices \n Flux tube: '+str(j+1)+' - QoI: '+qoi,
                            fname='gem0surrwf_al_sobt_'+str(i)+str(j)+'.png')

if __name__ == "__main__":

    # Global params
    print(f"> Reading environment variables")
    SYS = os.environ['SYS']
    mpi_instance =  os.environ['MPICMD']
    mpi_model = os.environ['MPIMOD']
    wrk_dir = tmp_dir = os.environ['SCRATCH']
    slurm_nodes = int(os.environ['SLURM_NNODES'])

    # - option 1.1 - use PCE
    #p = int(os.environ['POLORDER'])
    # - option 1.2 - use MC
    # # - option 1.2.1 - read environmental variable
    # n_samples = int(os.environ['NSAMPLES'])
    # - option 1.2.2 - read script argument
    n_samples = int(sys.argv[2]) if len(sys.argv) > 2 else 32
    print(f">> Using number of samples: {n_samples}")

    # Input variable: CoV of Q variation - to perfrom scan in level oa aleatrocic uncertainty
    # # - option 2.1 - read from environmental variable
    # if 'INPUTCOV' in os.environ:
    #     input_cov = os.environ['INPUTCOV']
    #     print(f">> Using input C.o.V. of {input_cov}")
    # else:
    #     input_cov = 0.1
    #     print(f">> Using DEFAULT input C.o.V. of {input_cov}")
  
    # - option 2.2 - read from script argument
    input_cov = float(sys.argv[1]) if len(sys.argv) > 1 else 0.1
    print(f">> Using input C.o.V. of {input_cov}")

    species = ['e', 'i']

    # # - option 3.1 - use pyGEM0 data
    # base_dataset_filename = "gem0py_new_baseline.csv"
    # - option 3.2 - use GEM data
    base_dataset_filename = "gem_new_baseline.csv"
    
    target_dataset_filename = "gem0py_new_local.csv"
    output_filename = "ets_coreprof_out.cpo"

    test_script_dir = "tests"

    ### Uncertain input definition
    # 2[fluxes] * 8[flux-tubes] (flux tubes are independent!)
    # Normal PDF for each: mu, sigma
    print(f"> Making UQ run parameters: i/o, parallelisation")

    # Choice: do not have tensor product over flux tubes, treat them in batch
    # - option 4.1 - use stub input parameters
    #input_params_propr = input_params_stub_relative()
    # - option 4.2 - use variable CoV QoI
    # input_params_propr = input_params_scan_relative(cov=input_cov)
    # - option 4.3 - use mean QoI from GEM scan
    input_params_propr = input_params_gem_scan()
    
    # TODO assumes discription["dist"] == "Normal"
    # Turn input param description into 'vary' - it has to be in the flux-average scale, so STD should in fact be CoV
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
    prof_coord_inds = [i for i in range(100)]

    # Output columns
    # # - option 5.1 - read only axis values
    #output_columns = ["te_value_0", "ti_value_0"]
    # # - option 5.2 - read values around flux tube locations
    # output_columns = ["te_value_0", "te_value_15", "te_value_30", "te_value_45", "te_value_57", "te_value_67", "te_value_77", "te_value_87", "te_value_95", 
    #                   "ti_value_0", "ti_value_15", "ti_value_30", "ti_value_45", "ti_value_57", "ti_value_67", "ti_value_77", "ti_value_87", "ti_value_95",]
    # - option 5.3 - read all core profile values
    output_columns = [f"t{sp}_value_{coord}" for sp,coord in zip(species, prof_coord_inds)]

    ### Define parallelisation paramaters
    #    e.g. surrogate: t_s ~= 10m. ; workflow t_w ~= 10m. ; buffer/overhead: t_b ~= 10m 
    #    t_wf ~= 0.5h, nodes:1 , cores:40, t_tot = 24h
    if SYS == 'MARCONI':
        n_cores_p_node = 48
    elif SYS == 'COBRA':
        n_cores_p_node = 40
    else:
        n_cores_p_node = 32
        print(f"HPC Machine unknown, assuming {n_cores_p_node} cores per node")

    nnodes = slurm_nodes
    ncores = nnodes * n_cores_p_node

    nfts = len(ft_coords)
    n_params = 2
    n_params = n_params*nfts

    # - option 1.1 - PCE
    #nruns = p**nparams
    # - option 1.2 - MC: with Saltelli sampling total number of runs is: 
    nruns = n_samples*(n_params + 2)
    #nruns = n_samples

    nnodes_tot = nnodes
    ncores_tot = nnodes_tot * n_cores_p_node

    print(f"> {nruns} runs requiring totally {ncores_tot} cores at {nnodes_tot} nodes")

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

    # files for configuration of workflow and its componenrts
    sim_nec_dir = 'workflow'
    sim_nec_files = ['read_profs.py', 'gem_surr_workflow_independent.sh', 'gem-surr-mft-fusion-independent.ymmsl', 
                      'ets.xml', 'ets.xsd', 'chease.xml', 'chease.xsd', 'gem0.xml', 'gem0.xsd', 
                      'gem0_equilibrium_in.cpo', 'gem0_coreprof_in.cpo']
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
    # - option 1.1 - PCE
    #sampler = uq.sampling.PCESampler(vary=input_params, polynomial_order=p)
    # - option 1.2 - MC 
    print(f">>> Using {n_samples} samples") ###DEBUG
    sampler = uq.sampling.MCSampler(vary=vary, n_mc_samples=n_samples,)

    # Analysis
    print("> Creating Analysis")
    # - option 1.1 - PCE
    #analysis = uq.analysis.PCEAnalysis(sampler=sampler, qoi_cols=output_columns)
    # - option 1.2 - MC: default analysis class would be QMCAnalysis
    #analysis = uq.analysis.BasicStats()
    analysis = uq.analysis.QMCAnalysis(sampler=sampler, qoi_cols=output_columns)
    # TODO: get percentiles (and 3rd&4th moments if possible), also perfrom KDE fit?

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
    analysis_result = campaign.get_last_analysis()

    # Moments of QoI: AVG, STD, SCW, KRT
    print(analysis_result.describe())

    pickle_filename = f"result_aleatoric_wf_ns{n_samples}_cv{input_cov}.pickle"
    with open(pickle_filename, "bw") as file_pickle:
        pickle.dump(analysis_result, file_pickle)

    # pickle_filename = "result_raw_data_aleatoric_wf.pickle"
    # with open(pickle_filename, "bw") as file_pickle:
    #     pickle.dump(analysis_result.raw_data, file_pickle)

    # pickle_filename = "result_samples_aleatoric_wf.pickle"
    # with open(pickle_filename, "bw") as file_pickle:
    #     pickle.dump(analysis_result.samples, file_pickle)

    #result.to_csv("result_aleatoric_wf.csv")

    plot_results(analysis_result, input_params, output_columns)
