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
from base.utils import cpo_inputs

'''
Perform UQ for the Turblence code GEM0 (using 8 flux tubes).
Uncertainties are driven by:
The electon and ion temperature and their gradient localisd on flux tube positions.
'''


# Global params
SYS = os.environ['SYS']
tmp_dir = os.environ['SCRATCH']
cpo_dir = os.path.abspath("../workflows/AUG_28906_6")
xml_dir = os.path.abspath("../workflows")
obj_dir = os.path.abspath("../standalone/bin/"+SYS)
exec_code = "gem0_test"


# Execution using QCG Pilot-Job
def exec_pj(campaign, exec_path, ncores, log_level="info"):
    qcgpjexec = eqi.Executor(campaign)
    qcgpjexec.create_manager(log_level=log_level)

    qcgpjexec.add_task(eqi.Task(
        eqi.TaskType.EXECUTION,
        eqi.TaskRequirements(cores=ncores),
        application=exec_path
    ))
    qcgpjexec.run(processing_scheme=eqi.ProcessingScheme.EXEC_ONLY)
    qcgpjexec.terminate_manager()


# To use in mutliapp Campaign
def setup_gem0(ftube_index, common_dir):

    # Define the uncertain parameters
    dist = { "dist": "Uniform", "err":  0.2}
    input_params = {
        "te.value": dist,
        "te.ddrho": dist,
        "ti.value": dist,
        "ti.ddrho": dist
    }

    # CPO file containg initial values of uncertain params
    input_filename = "ets_coreprof_in.cpo"
    input_cponame = "coreprof"

    # The quantities of intersts and the cpo file to set them
    output_columns = ["te_transp.flux", "ti_transp.flux"]
    output_filename = "gem0_coretransp_out.cpo"
    output_cponame = "coretransp"

    # Parameter space for campaign and the distributions list for the sampler
    params, vary = cpo_inputs(cpo_filename=input_filename,
                              cpo_name=input_cponame,
                              input_dir=cpo_dir,
                              input_params=input_params,
                              ftube_index=ftube_index)

    # Encoder, decoder, sampler
    input_filename = "gem0_coreprof_in.cpo"
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


# Main
if __name__ == "__main__":

    # Flux Tubes position indices. Run gem0_test in strandalone and use:
    # base.utils.ftube_indices('gem0_coreprof_in.cpo','gem0_coretransp_out.cpo')
    # to get the list
    ftube_indices = [15, 31, 44, 55, 66, 76, 85, 94]

    # Campaign for mutliapp
    campaign = uq.Campaign(name='UQ_8FTGEM0_', work_dir=tmp_dir)

    # Create common directory for ETS inputs
    common_dir = campaign.campaign_dir +"/common/"
    os.mkdir(common_dir)

    # Copy input CPO files (cf. test_gem0.f90)
    os.system("cp " + cpo_dir + "/ets_equilibrium_in.cpo "
                    + common_dir + "gem0_equilibrium_in.cpo")
    os.system("cp " + cpo_dir + "/ets_coreprof_in.cpo "
                    + common_dir + "/gem0_coreprof_in.cpo")

    # Copy XML and XSD files
    # Check if nrho_transp = 8 in gem0.xml
    os.system("cp " + xml_dir + "/gem0.xml " + common_dir)
    os.system("cp " + xml_dir + "/gem0.xsd " + common_dir)

    # Copy  exec file
    os.system("cp " + obj_dir +"/"+ exec_code + " " + common_dir)
    exec_path = os.path.join(common_dir, exec_code)

    # The executable
    exec_path = os.path.join(common_dir, exec_code)

    # Run Mutliapp
    results = []
    for i, j in enumerate(ftube_indices):
        params, encoder, decoder, sampler, stats = setup_gem0(j, common_dir)

        camp_name =  "GEM0_FT"+str(i)
        campaign.add_app(name=camp_name,
                         params=params,
                         encoder=encoder,
                         decoder=decoder)

        # Set and run campaign
        campaign.set_app(camp_name)
        campaign.set_sampler(sampler)
        campaign.draw_samples()
        campaign.populate_runs_dir()
        exec_pj(campaign, exec_path, 1)
        campaign.collate()
        campaign.apply_analysis(stats)

        result = campaign.get_last_analysis()
        results.append(result)


# Descrtiptive Statistics
#  stats_te_transp_flux[i]['mean'] = mean value of te_transp.flux in  the FluxTube i+1
#  stats_te_transp_flux[i]['std'] = std of te_transp.flux in the FluxTube i+1
stats_te_transp_flux  = []
stats_ti_transp_flux  = []
# Eg. s1_te_transp_flux[i] = Sobols 1st indices for te_transp.flux in  the FluxTube i+1
s1_te_transp_flux  = []
s1_ti_transp_flux  = []
for i in range(8):
    # Means and stds
    ste = {}
    sti = {}
    ste.update({'mean': results[i].describe()['te_transp.flux', i]['mean']})
    ste.update({'std': results[i].describe()['te_transp.flux', i]['std']})
    sti.update({'mean': results[i].describe()['ti_transp.flux', i]['mean']})
    sti.update({'std': results[i].describe()['ti_transp.flux', i]['std']})
    stats_te_transp_flux.append(ste)
    stats_ti_transp_flux.append(sti)

    # Sobols
    s1e = {}
    s1i = {}
    for uparam in list(params.keys()):
        s1e.update({uparam: results[i].sobols_first('te_transp.flux')[uparam][i]})
        s1i.update({uparam: results[i].sobols_first('ti_transp.flux')[uparam][i]})
    s1_te_transp_flux.append(s1e)
    s1_ti_transp_flux.append(s1i)

print("STATS TE-TRANSP_FLUX:")
print(stats_te_transp_flux)

print("STATS TI-TRANSP_FLUX:")
print(stats_ti_transp_flux)

print("SOBOLS TE-TRANSP_FLUX:")
print(s1_te_transp_flux)

print("SOBOLS TI-TRANSP_FLUX:")
print(s1_ti_transp_flux)
