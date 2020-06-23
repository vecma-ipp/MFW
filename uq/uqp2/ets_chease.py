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
#cpo_dir = os.path.abspath("../workflows/AUG_28906_6")
cpo_dir = os.path.abspath("../workflows/AUG_28906_6_8ft_restart")
#cpo_dir = os.path.abspath("../workflows/JET_92436_23066")
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

    output_filename = "ets_equilibrium_out.cpo"
    output_cponame = "equilibrium"

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

    sampler = uq.sampling.PCESampler(vary=vary, polynomial_order=3,
                                     regression=True)
    action = uq.actions.ExecuteLocal(exec_code)
    stats = uq.analysis.PCEAnalysis(sampler=sampler, qoi_cols=ets_output)

    return params, encoder, decoder, collater, sampler, action, stats


# The 2nd BOX
def setup_eq(eq_dir, eq_code, eq_input, eq_output):
    params = eq_input[0]
    spline = eq_input[1]
    vary = eq_input[2]

    input_cpofilename = "chease_equilibrium_in.cpo"
    input_cponame = "equilibrium"

    output_filename = "chease_equilibrium_out.cpo"
    output_cponame = "equilibrium"

    # Copy XML and XSD files
    os.system("cp " + xml_dir + "/chease.xml " + eq_dir)
    os.system("cp " + xml_dir + "/chease.xsd " + eq_dir)

    # Copy input CPO files in common directory
    os.system("cp " + cpo_dir + "/ets_coreprof_in.cpo "    + eq_dir + "/chease_coreprof_in.cpo")
    os.system("cp " + cpo_dir + "/ets_equilibrium_in.cpo " + eq_dir + "/chease_equilibrium_in.cpo")

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

    sampler = uq.sampling.PCESampler(vary=vary, polynomial_order=3,
                                     regression=True)
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
    ets_output = ["profiles_1d.pressure"]

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
    mean_1 = results1['statistical_moments']["profiles_1d.pressure"]["mean"]
    stat_1 = results1['statistical_moments']["profiles_1d.pressure"]
    sob1_1 = results1['sobols_first']["profiles_1d.pressure"]
    perc_1 = results1['percentiles']["profiles_1d.pressure"]
    dist_1 = results1['output_distributions']["profiles_1d.pressure"]

    equil_file = os.path.join(cpo_dir, "ets_equilibrium_in.cpo")
    equil = read(equil_file, "equilibrium")
    rho = equil.profiles_1d.rho_tor

    # Approxiamte mean by spline of dgree 3 using 5 elements
    ne = 4
    #u = rho/rho.max()
    ty, cy = spl_fit(mean_1, ne)
    tx, cx = spl_fit(rho, ne)

    # Distribtion for CP
    dist_cp = []
    te = []
    for i in range(ne):
        # index of the closet rho to CP abscissa cx
        i = np.abs(rho - cx[i]).argmin()
        #dist_cp.append(dist[i])
        if i==0:
            dist_cp.append(cp.Normal(mean_1[i], 0.2*mean_1[i]))
        else:
            dist_cp.append(cp.Uniform(0.9*mean_1[i], 1.1*mean_1[i]))
        te.append(mean_1[i])

    eq_input = [{"profiles_1d.pressure": {"type": "list", "default": te}},
                    {"profiles_1d.pressure": {"knot": ty.tolist(), "rho": rho.tolist()}},
                    {"profiles_1d.pressure": cp.J(*dist_cp)}
                   ]
    eq_output = ["profiles_1d.gm3"]

    eq_code = "chease_test"

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
    stat_2 = results2['statistical_moments']["profiles_1d.gm3"]
    sob1_2 = results2['sobols_first']["profiles_1d.gm3"]
    perc_2 = results2['percentiles']["profiles_1d.gm3"]
    dist_2 = results2['output_distributions']["profiles_1d.gm3"]

    # Plots
#    plots.plot_stats_all(rho, stat_1, perc_1, dist_1,
#                 xlabel=r'$\rho_{tor} [m]$', ylabel="Pressure",
#                 ftitle='Pressure profile',
#                 fname='data/outputs/STAT_1')
#
#    plots.plot_stats_all(rho, stat_2, perc_2, dist_2,
#                 xlabel=r'$\rho_{tor} [m]$', ylabel="Pressure",
#                 ftitle='gm3 profile',
#                 fname='data/outputs/STAT_2')
#
#    plots.plot_spl(rho, mean_1, cx, cy,
#                 xlabel=r'$\rho_{tor} [m]$', ylabel="Pressure",
#                 ftitle='BSpline interpolation',
#                 fname='data/outputs/SPL')
#
#    plots.plot_sobols_all(rho, sob1_1, list(params1.keys()),
#                  ftitle='1st Sobol indices: Pressure',
#                  fname='data/outputs/SA_1')
#
#    plots.plot_sobols_all(rho, sob1_2, ["profiles_1d.pressure"],
#                  ftitle='1st Sobol indices: gm3',
#                  fname='data/outputs/SA_2')

