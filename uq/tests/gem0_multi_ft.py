import os
import easyvvuq as uq
from ascii_cpo import read
import easypj
from easypj import TaskRequirements, Resources
from easypj import Task, TaskType, SubmitOrder
from easymfw.templates.cpo_encoder import CPOEncoder
from easymfw.templates.cpo_decoder import CPODecoder
from easymfw.utils.io_tools import get_cpo_inputs


# Update gem0.xml: set nrho_transp = 2

# Global params
SYS = os.environ['SYS']
tmp_dir = os.environ['SCRATCH']
cpo_dir = os.path.abspath("../workflows/AUG_28906_6_8ft_restart")
xml_dir = os.path.abspath("../workflows")
obj_dir = os.path.abspath("../standalone/bin/"+SYS)

# The executable code to run
obj_dir = os.path.abspath("../standalone/bin/"+SYS)
exec_code = "gem0_test"


def setup_gem0(common_dir, n_polyn, ft_index):
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
    output_filename = "gem0_coretransp_out.cpo"
    output_cponame = "coretransp"

    # parameter space for campaign and the distributions list for the sampler
    input_cpo_file = os.path.join(cpo_dir, input_filename)
    params, vary = get_cpo_inputs(cpo_file=input_cpo_file,
                                  cpo_name=input_cponame,
                                  input_params=input_params)

    # Copy input CPO files (cf. test_gem0.f90)
    os.system("cp " + cpo_dir + "/ets_equilibrium_in.cpo "
                    + common_dir + "gem0_equilibrium_in.cpo")
    os.system("cp " + cpo_dir + "/ets_coreprof_in.cpo "
                    + common_dir + "/gem0_coreprof_in.cpo")

    # Copy XML and XSD files
    os.system("cp " + xml_dir + "/gem0_8ft.xml " + common_dir + "/gem0.xml")
    os.system("cp " + xml_dir + "/gem0_8ft.xsd " + common_dir + "/gem0.xsd")

    # Create the encoder, decoder, collater and the sampler
    input_filename = "gem0_coreprof_in.cpo"
    encoder = CPOEncoder(template_filename=input_filename,
                         target_filename=input_filename,
                         input_cponame=input_cponame,
                         input_params=input_params,
                         common_dir=common_dir)
    decoder = CPODecoder(target_filename=output_filename,
                     output_columns=output_columns,
                     output_cponame=output_cponame)
    collater = uq.collate.AggregateSamples(average=False)
    #sampler = uq.sampling.QMCSampler(vary=vary, n_mc_samples=n_mc_samples)
    sampler = uq.sampling.PCESampler(vary=vary, polynomial_order=n_polyn)

    # The Analysis
    #analysis = uq.analysis.QMCAnalysis(sampler=sampler, qoi_cols=output_columns)
    analysis = uq.analysis.PCEAnalysis(sampler=sampler, qoi_cols=output_columns)

    # The setup outputs
    return params, encoder, decoder, collater, sampler, analysis

# Execution using QCG Pilot Job
def exec_pj(campaign, exec_path):
    qcgpjexec = easypj.Executor()
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


# Main
if __name__ == "__main__":
    # Campaign for mutliapp
    campaign = uq.Campaign(name='gem08ftuq_pce_', work_dir=tmp_dir)

    # Create common directory for ETS inputs
    common_dir = campaign.campaign_dir +"/common/"
    os.mkdir(common_dir)

    # Copy exec code
    os.system("cp " + obj_dir +"/"+ exec_code + " " + common_dir)
    exec_path = os.path.join(common_dir, exec_code)

    # The number of Monte-Carlo samples
    #n_mc_samples = 1000
    n_polyn = 3
    ft_indices = [15, 31, 44, 55, 66, 76, 85, 94]
    #ft_indices = [69]

    # Get setup for the 1st Flux tube and set it to the campaign
    results = []
    dfs = []
    for i, j in enumerate(ft_indices):
        # Get setup and set it to the campaign
        (params, encoder, decoder, collater, sampler, analysis) = setup_gem0(common_dir, n_polyn, ft_index=j)
        campaign.add_app(name="gem0uq-ft"+str(i+1),
                         params=params,
                         encoder=encoder,
                         decoder=decoder,
                         collater=collater)

        # Set and run the 1st campaign
        campaign.set_app("gem0uq-ft"+str(i+1))
        campaign.set_sampler(sampler)
        campaign.draw_samples()
        campaign.populate_runs_dir()
        exec_pj(campaign, exec_path)
        campaign.collate() # TODO: check two flux tubes and see if they are independent
        campaign.apply_analysis(analysis)

        result = campaign.get_last_analysis()
        results.append(result)
        dfs.append(campaign.get_collation_result())

    
    for i in range(len(dfs)):  # TODO: collate the result with a ready campaign folders
        dfs[i].to_csv(os.path.join(common_dir, 'res' + str(i) + '.csv'))
    
    # Get Descriptive Statistics
    print('Save descriptive Statistics: \n')
    with open(os.path.join(common_dir, 'gem0py_uq_8ft.csv'), 'w') as f:
        for i in range(len(ft_indices)):
            f.write("%s %i\n"%("Flux Tube: ", i+1))
            for key in results[i].keys():
                f.write("%s,%s\n"%(key, results[i][key]))
