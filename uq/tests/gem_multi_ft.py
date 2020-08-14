import os
import csv
import easyvvuq as uq
from ascii_cpo import read
import easypj
from easypj import TaskRequirements, Resources
from easypj import Task, TaskType, SubmitOrder
from easymfw.templates.cpo_encoder import CPOEncoder
from easymfw.templates.cpo_decoder import CPODecoder
from easymfw.templates.xml_element import XMLElement
from easymfw.utils.io_tools import get_cpo_inputs


# Global params
SYS = os.environ['SYS']
tmp_dir = os.environ['SCRATCH']
cpo_dir = os.path.abspath("../workflows/AUG_28906_6_8ft_restart")
xml_dir = os.path.abspath("../workflows")
obj_dir = os.path.abspath("../standalone/bin/"+SYS)

# From Slurm script
mpi_instance =  os.environ['MPICMD']

# The executable code to run
obj_dir = os.path.abspath("../standalone/bin/"+SYS)
exec_code = "gem_test"


def setup_gem(common_dir, ft_index):
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

    # Copy input CPO files (cf. test_gem.f90)
    os.system("cp " + cpo_dir + "/ets_equilibrium_in.cpo "
                    + common_dir + "gem_equilibrium_in.cpo")
    os.system("cp " + cpo_dir + "/ets_coreprof_in.cpo "
                    + common_dir + "/gem_coreprof_in.cpo")

    # Copy XML and XSD files
    os.system("cp " + xml_dir + "/gem.xml " + common_dir)
    os.system("cp " + xml_dir + "/gem.xsd " + common_dir)

    # Create the encoder, decoder, collater and the sampler
    input_filename = "gem_coreprof_in.cpo"
    encoder = CPOEncoder(template_filename=input_filename,
                         target_filename=input_filename,
                         input_cponame=input_cponame,
                         input_params=input_params,
                         common_dir=common_dir)
    decoder = CPODecoder(target_filename=output_filename,
                     output_columns=output_columns,
                     output_cponame=output_cponame)
    collater = uq.collate.AggregateSamples(average=False)
    sampler = uq.sampling.PCESampler(vary=vary, polynomial_order=3)

    # The Analysis
    analysis = uq.analysis.PCEAnalysis(sampler=sampler, qoi_cols=output_columns)

    # The setup outputs
    return params, encoder, decoder, collater, sampler, analysis

# Execution using QCG Pilot Job
def exec_pj(campaign, app_path):
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
    campaign = uq.Campaign(name='gemuq_', work_dir=tmp_dir)

    # Create common directory for ETS inputs
    common_dir = campaign.campaign_dir +"/common/"
    os.mkdir(common_dir)

    # Copy exec code
    os.system("cp " + obj_dir +"/"+ exec_code + " " + common_dir)

    # Get ncores
    gemxml = XMLElement(xml_dir + "/gem.xml")
    npesx = gemxml.get_value("cpu_parameters.domain_decomposition.npesx")
    npess = gemxml.get_value("cpu_parameters.domain_decomposition.npess")
    nftubes = gemxml.get_value("cpu_parameters.parallel_cases.nftubes")
    ncores = npesx*npess*nftubes

    exec_path = os.path.join(common_dir, exec_code)
    mpi_app = " ".join([mpi_instance, "-n", str(ncores), exec_path])

    # From: Run Gem once and use easymfw.utils.get_fluxtube_index
    ft_indices = [15, 31, 44, 55, 66, 76, 85, 94]

    results = []
    for i, j in enumerate(ft_indices):
        # Get setup and set it to the campaign
        (params, encoder, decoder, collater, sampler, analysis) = setup_gem(common_dir, ft_index=j)
        campaign.add_app(name="gemuq-ft"+str(i+1),
                         params=params,
                         encoder=encoder,
                         decoder=decoder,
                         collater=collater)

        # Set and run the 1st campaign
        campaign.set_app("gemuq-ft"+str(i+1))
        campaign.set_sampler(sampler)
        campaign.draw_samples()
        campaign.populate_runs_dir()
        exec_pj(campaign, mpi_app)
        campaign.collate()
        campaign.apply_analysis(analysis)

        result = campaign.get_last_analysis()
        results.append(result)

    # Get Descriptive Statistics
    print('Save escriptive Statistics: \n')
    with open('gem_uq.csv', 'w') as f:
        for i in range(7):
            f.write("%s %i\n"%("Flux Tube: ", i+1))
            for key in results[i].keys():
                f.write("%s,%s\n"%(key, results[i][key]))
