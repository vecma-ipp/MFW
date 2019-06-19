import os
import time
import numpy as np
import chaospy as cp
import easyvvuq as uq
import matplotlib.pylab as plt
from ascii_cpo import read
from tools import plots, spl


'''
Test of ETS + CHEASE using  UQP2 (Semi_intrusive
'''

# OS env
SYS = os.environ['SYS']

# CPO files
CPO_DIR = os.path.abspath("../data/TESTS/")

# To store input/ouput files and Campaign directories
TMP_DIR = "/ptmp/ljala/"

# The 1st box
def ets_test(uncert_params):

    # The ets_run executable (to run the ets model)
    bin_file = "../bin/"+SYS+"/ets_run "

    # Input/Output template
    input_json  = "inputs/ets_in.json"
    output_json = os.path.join(TMP_DIR, "out_ets.json")

    # Initialize Campaign object
    ets_campaign = uq.Campaign(
        name='ETS_Campaign',
        state_filename=input_json,
        workdir=TMP_DIR,
        default_campaign_dir_prefix='UQP2_ETS_'
    )

    campaign_dir = ets_campaign.campaign_dir

    # Copy xml files needed in the ETS code
    os.system("mkdir " + campaign_dir +"/workflows")
    os.system("cp ../../workflows/ets.xml "+ campaign_dir +"/workflows")
    os.system("cp ../../workflows/ets.xsd "+ campaign_dir +"/workflows")

    # Copy CPO files in common directory
    common_dir = campaign_dir +"/common/"
    os.system("cp " + CPO_DIR + "/*.cpo " + common_dir)

    # Get uncertain parameters distrubutions
    coret_file = common_dir + "ets_coretransp_in.cpo"
    coret = read(coret_file, "coretransp")
    diff_eff = coret.values[0].te_transp.diff_eff
    list_dist = [cp.Normal(diff_eff[i], 0.2*diff_eff[i]) for i in range(4)]

    # Define the parameters dictionary
    for i in range(4):
        ets_campaign.vary_param(uncert_params[i], dist=list_dist[i])

    # Create the sampler
    ets_sampler  = uq.elements.sampling.PCESampler(ets_campaign)

    # Generate runs
    ets_campaign.add_runs(ets_sampler)
    ets_campaign.populate_runs_dir()

    # Execute runs
    cmd = bin_file + common_dir + " ets_input.nml"
    ets_campaign.apply_for_each_run_dir(uq.actions.ExecuteLocal(cmd))

    # Aggregate the results from all runs.
    output_filename = ets_campaign.params_info['out_file']['default']
    output_columns = ['c']

    aggregate = uq.elements.collate.AggregateSamples(
        ets_campaign,
        output_filename=output_filename,
        output_columns=output_columns,
        header=0,
    )

    aggregate.apply()

    # Analysis
    analysis = uq.elements.analysis.PCEAnalysis(
        ets_campaign, value_cols=output_columns)

    dist, cov = analysis.apply()

    # Results
    cov_mnat= cov["c"]

    return dist["c"]

# The 2nd box
def chease_test(list_dist):

    # Uncertain parameters
    uncert_params = ["C0", "C2", "C3", "C4"]

    # The chease executable (to run the eq model)
    bin_file = "../bin/"+SYS+"/chease_run "

    # Input/Output template
    input_json  = "inputs/eq_in.json"
    output_json = os.path.join(TMP_DIR, "out_eq.json")

    # Initialize Campaign object
    eq_campaign = uq.Campaign(
        name='EQ_Campaign',
        state_filename=input_json,
        workdir=TMP_DIR,
        default_campaign_dir_prefix='UQP2_EQ_'
    )

    campaign_dir = eq_campaign.campaign_dir

    # Copy xml files needed in the eq code
    os.system("mkdir " + campaign_dir +"/workflows")
    os.system("cp ../../workflows/chease.xml "+ campaign_dir +"/workflows")
    os.system("cp ../../workflows/chease.xsd "+ campaign_dir +"/workflows")

    # Copy CPO files in common directory
    common_dir = campaign_dir +"/common/"
    os.system("cp " + CPO_DIR + "/*.cpo " + common_dir)

    # Define the parameters dictionary
    for i in range(4):
        eq_campaign.vary_param(uncert_params[i], dist=list_dist[i])

    # Create the sampler
    eq_sampler  = uq.elements.sampling.PCESampler(eq_campaign)

    # Generate runs
    eq_campaign.add_runs(eq_sampler)
    eq_campaign.populate_runs_dir()

    # Execute runs
    cmd = bin_file + common_dir + " eq_input.nml"
    eq_campaign.apply_for_each_run_dir(uq.actions.ExecuteLocal(cmd))

    # Aggregate the results from all runs.
    output_filename = eq_campaign.params_info['out_file']['default']
    output_columns = ['b_av']

    aggregate = uq.elements.collate.AggregateSamples(
        eq_campaign,
        output_filename=output_filename,
        output_columns=output_columns,
        header=0,
    )

    aggregate.apply()

    # Analysis
    analysis = uq.elements.analysis.PCEAnalysis(
        eq_campaign, value_cols=output_columns)

    analysis.apply()

    # Results
    stats = analysis.statistical_moments('b_av')
    sobols= analysis.sobol_indices('b_av', 'first_order')

    return stats, sobols

# Main
if __name__ == "__main__":

    # Uncertain parameters
    uncert_params_in = ["D1", "D2", "D3", "D4"]

    # The 1st box: ETS
    dist = ets_test(uncert_params_in)

    # The 2nd box: CHEASE
    stats, sobols =  chease_test(dist)

    # PLOTS
    eq_file = CPO_DIR + "/ets_equilibrium_in.cpo"
    eq = read(eq_file, "equilibrium")

    rho = eq.profiles_1d.rho_tor

    plots.plot_stats(rho, stats,
                 xlabel=r'$\rho_{tor} ~ [m]$', ylabel=r'<B>',
                 ftitle='B_av profile',
                 fname='figs/b_prof.png')

    plot.plot_sobols_4(rho, sobols, uncert_params_in, 'b_av')
