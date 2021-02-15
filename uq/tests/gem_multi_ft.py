import csv
import os
import easyvvuq as uq
# EasyVVUQ/QCG-PJ
import eqi
# from ual
from ascii_cpo import read
# from current package
from base.cpo_encoder import CPOEncoder
from base.cpo_decoder import CPODecoder
from base.xml_element import XMLElement
from base.utils import cpo_inputs
from base.plots import plot_moments, plot_sobols

'''
Perform UQ for the Turblence code gem (using 8 flux tubes).
Uncertainties are driven by:
The electon and ion temperature and their gradient localisd on flux tube positions.
'''

# UQ app
def setup_gem(ftube_index, common_dir, input_params, output_columns):
    # CPO file containg initial values of uncertain params
    input_filename = "ets_coreprof_in.cpo"
    input_cponame = "coreprof"

    # CPO file containing the quantities of intersts
    output_filename = "gem_coretransp_out.cpo"
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
                          ftube_index=ftube_index)

    # The decoder
    decoder = CPODecoder(cpo_filename=output_filename,
                         cpo_name=output_cponame,
                         output_columns=output_columns)

    # The sampler
    sampler = uq.sampling.PCESampler(vary=vary, polynomial_order=3)

    # The Analysis
    stats = uq.analysis.PCEAnalysis(sampler=sampler, qoi_cols=output_columns)

    return params, encoder, decoder, sampler, stats

# Execution using QCG Pilot-Job
def exec_pj(campaign, exec_path, ncores, log_level="info"):
    qcgpjexec = eqi.Executor(campaign)
    qcgpjexec.create_manager(log_level=log_level)

    qcgpjexec.add_task(eqi.Task(
        eqi.TaskType.EXECUTION,
        eqi.TaskRequirements(cores=ncores),
        model=mpi_instance,
        application=exec_path
    ))
    qcgpjexec.run(processing_scheme=eqi.ProcessingScheme.EXEC_ONLY)
    qcgpjexec.terminate_manager()


# Main
if __name__ == "__main__":
    # Global params
    SYS = os.environ['SYS']
    tmp_dir = os.environ['SCRATCH']
    mpi_instance =  os.environ['MPICMD']
    cpo_dir = os.path.abspath("../workflows/AUG_28906_6_8ft_restart")
    xml_dir = cpo_dir
    obj_dir = os.path.abspath("../standalone/bin/"+SYS)
    exec_code = "gem_test"

    # Define the uncertain parameters (UQ inputs)
    input_params = {
        "te.value": {"dist": "Uniform", "err":  0.2, "min": 0.},
        "ti.value": {"dist": "Uniform", "err":  0.2, "min": 0.},
        "te.ddrho": {"dist": "Uniform", "err":  0.2, "max": 0.},
        "ti.ddrho": {"dist": "Uniform", "err":  0.2, "max": 0.}
    }

    # The quantities of intersts (UQ outputs)
    output_columns = ["te_transp.flux", "ti_transp.flux"]

    # Flux Tubes position indices. Run gem_test in strandalone and use:
    # base.utils.ftube_indices('gem_coreprof_in.cpo','gem_coretransp_out.cpo')
    # to get the list
    ftube_indices = [15, 31, 44, 55, 66, 76, 85, 94]

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

    # Copy  exec file
    os.system("cp " + obj_dir +"/"+ exec_code + " " + common_dir)
    exec_path = os.path.join(common_dir, exec_code)

    # get ncores
    gemxml = XMLElement(xml_dir + "/gem.xml")
    npesx = gemxml.get_value("cpu_parameters.domain_decomposition.npesx")
    npess = gemxml.get_value("cpu_parameters.domain_decomposition.npess")
    nftubes = gemxml.get_value("cpu_parameters.parallel_cases.nftubes")
    ncores = npesx*npess*nftubes

    # To store Statistics and Sobols
    means = {qoi: [] for qoi in output_columns}
    stds = {qoi: [] for qoi in output_columns}
    sob1 = {qoi: [] for qoi in output_columns}
    sobt = {qoi: [] for qoi in output_columns}

    # Run Mutliapp
    for i, ft_index in enumerate(ftube_indices):
        params, encoder, decoder, sampler, stats = setup_gem(ft_index, common_dir, input_params, output_columns)

        camp_name =  "gem_FT"+str(i+1)
        campaign.add_app(name=camp_name,
                         params=params,
                         encoder=encoder,
                         decoder=decoder)

        # Set and run campaign
        campaign.set_app(camp_name)
        campaign.set_sampler(sampler)
        campaign.draw_samples()
        campaign.populate_runs_dir()
        exec_pj(campaign, exec_path, ncores)
        campaign.collate()
        campaign.apply_analysis(stats)
        result = campaign.get_last_analysis()

        # Sore results
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
