import os
import time
import easyvvuq as uq
import numpy as np
import chaospy as cp
from ascii_cpo import read
from easymfw.templates.cpo_encoder import CPOEncoder
from easymfw.templates.cpo_decoder import CPODecoder
from easymfw.templates.xml_encoder import XMLEncoder
from easymfw.utils.io_tools import get_cpo_inputs
from easymfw.utils.io_tools import get_xml_inputs
from easymfw.utils.splines import spl_fit
from easymfw.utils.splines import spl_eval
from easymfw.utils import plots
import easypj
from easypj import TaskRequirements, Resources
from easypj import Task, TaskType, SubmitOrder


# Global params
SYS = os.environ['SYS']
tmp_dir = os.environ['SCRATCH']
cpo_dir = os.path.abspath("../workflows/AUG_28906_6_BgB")
xml_dir = os.path.abspath("../workflows")
obj_dir = os.path.abspath("../standalone/bin/"+SYS)


# The 1st BOX
def setup_ets(ets_dir, ets_code, ets_input, ets_output):
    input_params_bc = ets_input[0]
    input_params_src = ets_input[1]
    input_params = {}
    input_params.update(input_params_bc)
    input_params.update(input_params_src)

    input_cpofilename = "ets_coreprof_in.cpo"
    input_cponame = "coreprof"
    input_xmlfilename = "source_dummy.xml"
    input_xsdfilename = "source_dummy.xsd"

    output_filename = "ets_coreprof_out.cpo"
    output_cponame = "coreprof"

    # Copy XML and XSD files
    os.system("cp " + xml_dir + "/ets.xml "    + ets_dir)
    os.system("cp " + xml_dir + "/ets.xsd "    + ets_dir)
    os.system("cp " + xml_dir + "/source_dummy.xml " + ets_dir)
    os.system("cp " + xml_dir + "/source_dummy.xsd " + ets_dir)

    # Copy input CPO files in common directory
    os.system("cp " + cpo_dir + "/ets_coreprof_in.cpo "    + ets_dir)
    os.system("cp " + cpo_dir + "/ets_equilibrium_in.cpo " + ets_dir)
    os.system("cp " + cpo_dir + "/ets_coreimpur_in.cpo "   + ets_dir)
    os.system("cp " + cpo_dir + "/ets_coretransp_in.cpo "  + ets_dir)
    os.system("cp " + cpo_dir + "/ets_toroidfield_in.cpo " + ets_dir)

    os.system("cp " + obj_dir +"/" + ets_code + " " + ets_dir)
    exec_code = os.path.join(ets_dir, ets_code)

    #
    input_cpofile = os.path.join(cpo_dir, input_cpofilename)
    params_cpo, vary_cpo = get_cpo_inputs(cpo_file=input_cpofile,
                                          cpo_name=input_cponame,
                                          input_params=input_params_bc)

    input_xmlfile = os.path.join(xml_dir, input_xmlfilename)
    input_xsdfile = os.path.join(xml_dir, input_xsdfilename)
    params_xml, vary_xml = get_xml_inputs(xml_file=input_xmlfile,
                                          xsd_file=input_xsdfile,
                                          input_params=input_params_src)

    params = {**params_cpo, **params_xml}
    vary = {**vary_cpo, **vary_xml}

    encoder_cpo = CPOEncoder(template_filename=input_cpofilename,
                             target_filename=input_cpofilename,
                             input_cponame=input_cponame,
                             common_dir=ets_dir,
                             input_params=input_params_bc)
    encoder_xml = XMLEncoder(template_filename = input_xmlfilename,
                             target_filename = input_xmlfilename,
                             input_params=input_params_src,
                             common_dir=ets_dir)
    encoder = uq.encoders.MultiEncoder(encoder_cpo, encoder_xml)

    decoder = CPODecoder(target_filename=output_filename,
                         output_columns=ets_output,
                         output_cponame=output_cponame)

    collater = uq.collate.AggregateSamples(average=False)

    sampler = uq.sampling.PCESampler(vary=vary, polynomial_order=4)
    action = uq.actions.ExecuteLocal(exec_code)
    stats = uq.analysis.PCEAnalysis(sampler=sampler, qoi_cols=ets_output)

    return params, encoder, decoder, collater, sampler, action, stats


# The 2nd BOX
def setup_eq(eq_dir, eq_code, eq_input, eq_output):
    params = eq_input[0]
    spline = eq_input[1]
    vary = eq_input[2]

    input_cpofilename = "equil_coreprof_in.cpo"
    input_cponame = "coreprof"

    output_filename = "equil_equilibrium_out.cpo"
    output_cponame = "equilibrium"

    # Copy XML and XSD files
    os.system("cp " + xml_dir + "/chease.xml " + eq_dir)
    os.system("cp " + xml_dir + "/chease.xsd " + eq_dir)

    # Copy input CPO files in common directory
    os.system("cp " + cpo_dir + "/ets_coreprof_in.cpo "    + eq_dir + "/equil_coreprof_in.cpo")
    os.system("cp " + cpo_dir + "/ets_equilibrium_in.cpo " + eq_dir + "/equil_equilibrium_in.cpo")
    os.system("cp " + cpo_dir + "/ets_toroidfield_in.cpo " + eq_dir + "/equil_toroidfield_in.cpo")

    os.system("cp " + obj_dir +"/" + eq_code + " " + eq_dir)
    exec_code = os.path.join(eq_dir, eq_code)

    encoder = CPOEncoder(template_filename=input_cpofilename,
                             target_filename=input_cpofilename,
                             input_cponame=input_cponame,
                             common_dir=eq_dir,
                             input_params=spline)

    decoder = CPODecoder(target_filename=output_filename,
                         output_columns=eq_output,
                         output_cponame=output_cponame)

    collater = uq.collate.AggregateSamples(average=False)

    sampler = uq.sampling.PCESampler(vary=vary, polynomial_order=4, regression=True)
    #action = uq.actions.ExecuteLocal(exec_code)
    stats = uq.analysis.PCEAnalysis(sampler=sampler, qoi_cols=eq_output)

    return params, encoder, decoder, collater, sampler, exec_code, stats

def exec_pj(campaign, exec_code):
    qcgpjexec = easypj.Executor()
    qcgpjexec.create_manager(dir=campaign.campaign_dir, log_level='info')
    qcgpjexec.add_task(Task(
        TaskType.EXECUTION,
        TaskRequirements(cores=Resources(exact=1)),
        application=exec_code
    ))
    qcgpjexec.run(
        campaign=campaign,
        submit_order=SubmitOrder.EXEC_ONLY
    )
    qcgpjexec.terminate_manager()


# Main
if __name__ == "__main__":
    # Campaign for mutliapp
    campaign = uq.Campaign(name='uqp2_', work_dir=tmp_dir)

    # Create common directory for ETS inputs
    ets_dir = campaign.campaign_dir +"/ets/"
    os.mkdir(ets_dir)

    # ETS-UQ inputs/outputs
    input_params_bc = {
        "te.boundary.value": {
            "dist": "Normal",
            "err": 0.2,
        }
    }
    input_params_src = {
        "electrons.heating_el.WTOT_el":{
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
    ets_input = [input_params_bc, input_params_src]
    ets_output = ["te.value"]

    # The execute code
    ets_code = "ets_src"

    # Get ETS setup
    (params1, encoder1, decoder1, collater1, sampler1, action1, stats1) = setup_ets(ets_dir, ets_code, ets_input, ets_output)

    # Add the ETS app to the campaign
    campaign.add_app(name="ets",
                     params=params1,
                     encoder=encoder1,
                     decoder=decoder1,
                     collater=collater1)

    campaign.set_app("ets")
    campaign.set_sampler(sampler1)
    campaign.draw_samples()
    campaign.populate_runs_dir()
    campaign.apply_for_each_run_dir(action1)
    campaign.collate()
    campaign.apply_analysis(stats1)
    results1 = campaign.get_last_analysis()

    # Get Descriptive Statistics
    mean = results1['statistical_moments']["te.value"]["mean"]
    stat = results1['statistical_moments']["te.value"]
    sob1 = results1['sobols_first']["te.value"]
    perc = results1['percentiles']["te.value"]
    dist = results1['output_distributions']["te.value"]

    corep_file = os.path.join(cpo_dir,  "ets_coreprof_in.cpo")
    corep = read(corep_file, "coreprof")
    rho = corep.rho_tor_norm

    import sys
    sys.exit()
    # Approxiamte mean by spline of dgree 3 using 5 elements
    ne = 4
    k = 3
    ty, cy = spl_fit(mean, ne, k)
    tx, cx = spl_fit(rho, ne, k)

    # Distribtion for CP
    dist_cp = []
    te = []
    for i in range(ne):
        # index of the closet rho to CP abscissa cx
        i = np.abs(rho - cx[i]).argmin()
        #dist_cp.append(dist[i])
        dist_cp.append(cp.Normal(mean[i], 0.2*mean[i]))
        te.append(mean[i])

    eq_input = [{"te.value": {"type": "list", "default": te}},
                    {"te.value": {"knot": ty.tolist(), "k": k, "rho": rho.tolist()}},
                    {"te.value": cp.J(*dist_cp)}
                   ]
    eq_output = ["profiles_1d.pressue"]

    eq_code = "equil_test"

    eq_dir = campaign.campaign_dir +"/eq/"
    os.mkdir(eq_dir)

    # Get EQUIL setup
    (params2, encoder2, decoder2, collater2, sampler2, exec_code, stats2) = setup_eq(eq_dir, eq_code, eq_input, eq_output)

    campaign.add_app(name="eq",
                     params=params2,
                     encoder=encoder2,
                     decoder=decoder2,
                     collater=collater2)

    campaign.set_app("eq")
    campaign.set_sampler(sampler2)
    campaign.draw_samples()
    campaign.populate_runs_dir()
    print("Ns = ", sampler2.n_samples)
    #campaign.apply_for_each_run_dir(action2)
    exec_pj(campaign, exec_code)
    campaign.collate()
    campaign.apply_analysis(stats2)
    results2 = campaign.get_last_analysis()

    # Get Descriptive Statistics
    stat = results2['statistical_moments']["profiles_1d.pressure"]

    sob1 = results2['sobols_first']["profiles_1d.pressure"]
    perc = results2['percentiles']["profiles_1d.pressure"]
    distp = results2['output_distributions']["profiles_1d.pressure"]

    equil_file = os.path.join(cpo_dir, "ets_equilibrium_in.cpo")
    equil = read(equil_file, "equilibrium")
    rho = equil.profiles_1d.rho_tor

#    plots.plot_stats_all(rho, stat, perc, dist,
#                 xlabel=r'$\rho_{tor} [m]$', ylabel="Te [eV]",
#                 ftitle='Temperature profile',
#                 fname='data/outputs/STAT_Te')
#
#    plots.plot_sobols_all(rho, sob1, ["te.value"]
#                  ftitle='1st Sobol indices',
#                  fname='data/outputs/SA_Te')
