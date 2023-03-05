import csv
import os

# from easyvvuq
import easyvvuq as uq
from easyvvuq.actions import Encode, Decode, Actions, CreateRunDirectory, ExecuteQCGPJ, ExecuteLocal, ExecuteSLURM, QCGPJPool

# form qcg-pj
from qcg.pilotjob.executor_api.qcgpj_executor import QCGPJExecutor

# from ual
from ascii_cpo import read

# from current package
from base.cpo_encoder import CPOEncoder
from base.cpo_decoder import CPODecoder
from base.xml_element import XMLElement
from base.xml_encoder import XMLEncoder
from base.utils import cpo_inputs, ftube_indices
from base.plots import plot_moments, plot_sobols
from base.evvuq_partemplate_wenv import EasyVVUQParallelTemplateWithEnv

from math import ceil

'''
Perform UQ for the Turblence code gem (using 8 flux tubes).
Uncertainties are driven by:
The electon and ion temperature and their gradient localised on flux tube positions.
'''

# UQ app
def setup_gem(ftube_index, common_dir, input_params, output_columns, xml=None, pol_order=2):
    # CPO file containg initial values of uncertain params
    input_filename = "ets_coreprof_in.cpo"
    input_cponame = "coreprof"

    # CPO file containing the quantities of intersts
    #output_filename = "gem_coretransp_out.cpo"
    output_filename = "gem_coretransp_0100.cpo"
    output_cponame = "coretransp"

    # Parameter space for campaign and the distributions list for the sampler
    params, vary = cpo_inputs(cpo_filename=input_filename,
                              cpo_name=input_cponame,
                              input_dir=cpo_dir,
                              input_params=input_params,
                              ftube_index=ftube_index)

    # Encoder, decoder, sampler
    input_filename = "gem_coreprof_in.cpo"
    encoder = CPOEncoder(cpo_filename=input_filename,
                          cpo_name=input_cponame,
                          input_dir=common_dir,
                          ftube_index=ftube_index,
                          xmlelement=xml)

    # The decoder
    decoder = CPODecoder(cpo_filename=output_filename,
                         cpo_name=output_cponame,
                         output_columns=output_columns)

    # The sampler
    sampler = uq.sampling.PCESampler(vary=vary, polynomial_order=pol_order)

    # The Analysis
    stats = uq.analysis.PCEAnalysis(sampler=sampler, qoi_cols=output_columns)

    return params, encoder, decoder, sampler, stats

# Execution using QCG Pilot-Job
def exec_pj(campaign, exec_path, ncores, nnodes, mpi_instance, log_level="debug"):

    exec_res = None

    template_par_short = {
                       'numCores': ncores,
                       'numNodes': nnodes,
                       'model': mpi_model,
                         }
    try:
        print('Creating resource pool')

        with QCGPJPool(
                    qcgpj_executor=QCGPJExecutor(log_level=log_level, ),
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

def exec_loc(campaign, exec_path, log_level="debug"):
    exec_res = campaign.execute()
    exec_res.collate()
    return exec_res

def plot_results(means, stds, sob1, sobt, output_columns):

    for i, qoi in enumerate(output_columns):
        plot_moments(means[qoi], stds[qoi],
                     xlabel="Flux tubes", ylabel=qoi,
                     ftitle="GEM: descriptive statistics for "+qoi,
                     fname="gem_stats"+str(i)+".png")

        for j in range(8):
                plot_sobols(sob1[qoi][j],
                            xlabel="Uncertain parameters", ylabel="First sobol",
                            ftitle='GEM: First sobol indices \n Flux tube: '+str(j+1)+' - QoI: '+qoi,
                            fname='gem_sob1_'+str(i)+str(j)+'.png')
                plot_sobols(sobt[qoi][j],
                            xlabel="Uncertain parameters", ylabel="Total sobol",
                            ftitle='GEM: Total sobol indices \n Flux tube: '+str(j+1)+' - QoI: '+qoi,
                            fname='gem_sobt_'+str(i)+str(j)+'.png')


# Main
if __name__ == "__main__":
    # Global params
    SYS = os.environ['SYS']
    tmp_dir = os.environ['SCRATCH']
    mpi_instance =  os.environ['MPICMD']
    mpi_model = os.environ['MPIMOD']
    cpo_dir = os.path.abspath("../workflows/AUG_28906_6_8ft_restart")
    xml_dir = cpo_dir
    obj_dir = os.path.abspath("../standalone/bin/"+SYS)
    exec_code = "loop_gem_notransp"

    alpha_unc = 0.25

    # Define the uncertain parameters (UQ inputs)
    input_params = {
        "te.value": {"dist": "Uniform", "err":  alpha_unc, "min": 0.},
        "ti.value": {"dist": "Uniform", "err":  alpha_unc, "min": 0.},
        "te.ddrho": {"dist": "Uniform", "err":  alpha_unc, "max": 0.},
        "ti.ddrho": {"dist": "Uniform", "err":  alpha_unc, "max": 0.}
    }

    # The quantities of intersts (UQ outputs)
    output_columns = [
                "te_transp.flux", 
                "ti_transp.flux"
                     ]

    # Flux Tubes position indices. Run gem_test in strandalone and use:
    # base.utils.ftube_indices('gem_coreprof_in.cpo','gem_coretransp_out.cpo')
    # to get the list
    ftube_indices = [15, 31, 44, 55, 66, 76, 85, 94]
    ftube_rhos = [0.14, 0.31, 0.44, 0.56, 0.67, 0.77, 0.86, 0.95] # these are rho_tor_norm
    # TODO might be possible to read from some existing cpo files

    # Campaign for mutliapp
    campaign = uq.Campaign(name='UQ_8FTgem_', work_dir=tmp_dir)

    # Create common directory for ETS inputs
    common_dir = campaign.campaign_dir +"/common/"
    os.mkdir(common_dir)

    # Copy input CPO files (cf. test_gem.f90)
    os.system("cp " + cpo_dir + "/ets_equilibrium_in.cpo "
                    + common_dir + "gem_equilibrium_in.cpo")
    os.system("cp " + cpo_dir + "/ets_coreprof_in.cpo "
                    + common_dir + "/gem_coreprof_in.cpo")

    # Copy restart files
    os.system("cp " + cpo_dir + "/t0?.dat " + common_dir)

    # Copy XML and XSD files
    # Check if nrho_transp = 8 in gem.xml
    os.system("cp " + xml_dir + "/gem.xml " + common_dir)
    os.system("cp " + xml_dir + "/gem.xsd " + common_dir)

    # Copy exec file
    os.system("cp " + obj_dir +"/"+ exec_code + " " + common_dir)
    exec_path = os.path.join(common_dir, exec_code)

    exec_comm_flags = ' '
    exec_path_comm = mpi_instance + exec_comm_flags + ' ' + exec_path

    # get ncores
    gemxml = XMLElement(xml_dir + "/gem.xml")
    npesx = gemxml.get_value("cpu_parameters.domain_decomposition.npesx")
    npess = gemxml.get_value("cpu_parameters.domain_decomposition.npess")
    nftubes = gemxml.get_value("cpu_parameters.parallel_cases.nftubes")
    ncores = npesx*npess*nftubes

    n_cores_p_node = 40
    if SYS == 'MARCONI':
        n_cores_p_node = 48
    elif SYS == 'COBRA':
        n_cores_p_node = 40
    nnodes = ceil(1.*ncores/n_cores_p_node)
    pol_order = int(os.environ['POLORDER'])
    nparams = len(input_params)
    nruns = (pol_order + 1)**nparams #TODO  should probably rely only on eVVUQ information
    ncores_tot = ncores * nruns
    nnodes_tot = nnodes * nruns #nnodes_tot = ceil(1.*ncores_tot/n_cores_p_node)
    # Current implementation: one run needs an entire node of 40 cores and only 32 will be used...
    #    single submission of one node can cary about 3 complete runs
    #    each runs here is multiplicated for number of flux tubes, here it is 8

    print('> {2} Runs requiring totally {0} cores at {1} nodes'.format(ncores_tot, nnodes_tot, nruns))

    # To store Statistics and Sobols
    means = {qoi: [] for qoi in output_columns}
    stds = {qoi: [] for qoi in output_columns}
    sob1 = {qoi: [] for qoi in output_columns}
    sobt = {qoi: [] for qoi in output_columns}

    # Run
    for i, ft_index in enumerate(ftube_indices):

        gemxml.set_value('equilibrium_parameters.geometric.ra0', ftube_rhos[i])

        params, encoder, decoder, sampler, stats = setup_gem(ft_index, common_dir, input_params, output_columns, xml=gemxml, pol_order=pol_order)


        actions = Actions(
                            CreateRunDirectory('/runs'),
                            Encode(encoder),
                            ExecuteLocal(exec_path_comm),
                            Decode(decoder),
                            )

        camp_name =  "gem_FT"+str(i)
        campaign.add_app(name=camp_name,
                            params=params,
                            actions=actions)

        # Set and run campaign
        campaign.set_sampler(sampler)

        exec_res = exec_pj(campaign, exec_path, ncores, nnodes, mpi_instance)
        #exex_res = exec_loc(campaign, exec_path)

        campaign.apply_analysis(stats)
        result = campaign.get_last_analysis()

        # Store results - TODO has to be done on per f.t. basis
        for qoi in output_columns:
            means[qoi].append(result.describe(qoi, 'mean')[i])
            stds[qoi].append(result.describe(qoi, 'std')[i])
            s1 = {}
            st = {}
            for par in list(input_params.keys()):
                s1.update({par: result.sobols_first(qoi)[par][i]})
                st.update({par: result.sobols_total(qoi)[par][i]})
            sob1[qoi].append(s1)
            sobt[qoi].append(st)

    # Plot descrtiptive statistics and sensitivity analysis
    #plot_results(means, stds, sob1, sobt, output_columns)