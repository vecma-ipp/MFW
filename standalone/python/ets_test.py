import os
import time
import numpy    as np
import chaospy  as cp
import easyvvuq as uq
import matplotlib.pylab as plt
from ascii_cpo import read
#from wrappers  import run_ets

# To compute elapsed time
start_time = time.time()

# Inputs
cpo_dir = "../data/SP2FT"
input_json  = "inputs/ets_in.json"

# For the outputs
tmp_dir = "/ptmp/ljala/"
output_json = os.path.join(tmp_dir, "out_pce.json")

# Get uncertain parameters and distrubutions
coret_file = cpo_dir + "/ets_coretransp_in.cpo"
coret      = read(coret_file, "coretransp")
diff_eff = coret.values[0].te_transp.diff_eff
dist1 = cp.Normal(diff_eff[0], 0.2*diff_eff[0])
dist2 = cp.Normal(diff_eff[1], 0.2*diff_eff[1])

# Initialize Campaign object
ets_campaign = uq.Campaign(state_filename=input_json, workdir=tmp_dir,
                           default_campaign_dir_prefix='ETS_Campaign_')

# Define the parameters dictionary
ets_campaign.vary_param("D1", dist=dist1)
ets_campaign.vary_param("D2", dist=dist2)

# Create the sampler
ets_sampler  = uq.elements.sampling.PCESampler(ets_campaign)

# Generate runs
ets_campaign.add_runs(ets_sampler)
ets_campaign.populate_runs_dir()

# Execute runs
#ets_campaign.apply_for_each_run_dir(
#    uq.actions.ExecuteLocal("ets_test.py ets_input.nml"))

#
#    corep_file = tmp_dir + '/ets_coreprof_out.cpo'
#    corep      = read(corep_file, "coreprof")
#    samples_te.append(corep.te.value)
#

