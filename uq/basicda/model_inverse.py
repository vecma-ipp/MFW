import numpy as np
import os
import joblib

import chaospy as cp
import easyvvuq as uq

from easyvvuq.encoders import BaseEncoder, GenericEncoder
from easyvvuq.decoders import JSONDecoder

from easymfw.utils.io_tools import get_dist

"""
For given range/distribution of flux values use surrogate model 
to find range/distribution of temperatures/gradients 
"""

# set directories with the surrogate model, campaign files etc.
model_dir = ""
tmp_dir = ""

# execution locally
EXEC_PJ = False

input_params = {
    "te.flux.values" :
        {
            "dist": "Normal",
            "err": 0.1,
            "min": 6e4
        }
}

output_columns = ['te.value']

# get the initial cpo file


# get the model, currently a joblib file for a sklearn model
surrogate_file_name = "gpr_mode_gem0"
surrogate = joblib.load(os.path.join(model_dir, surrogate_file_name))

# Initialise campaign object
campaign_name = "INV_GEM0_"

m_campaign = uq.Campaign(name=campaign_name, 
                        #work_dir=
                        )

# Crete the encoder
encoder = uq.encodersGenericEncoder(template_fname='surrogate_inverse.template',
                         delimiter='$',
                         target_filename='surrogate_in.json')

# Create decoder
decoder = uq.decoders.JSONDecoder(target_filename='surrogate_out.json')

m_campaign.add_app(name=campaign_name,
                   params=input_params,
                   encoder=encoder,
                   decoder=decoder)

vary = {}
for k, v in input_params.items():
    vary[k] = get_dist(k,v["dist"], v["err"])


m_sampler = uq.sampling.QMCSampler(vary=vary, n_mc_samples=1e3)

m_campaign.set_sampler(m_sampler)

# create inputs (samples fro target value distribution) for the surrogate
m_campaign.draw_samples()
m_campaign.populate_runs_dir()

# Execute call to surrogate locally
m_campaign.apply_for_each_run_dir(uq.actions.ExecuteLocal("{} fusion_in.json".
                                                          format(os.path.abspath('tutorial_files/fusion_model.py')),
                                                          interpret="python3")) # TODO make surrogate caller script

m_campaign.collate()


# postprocessing: apply analysis to calculate posterior distributions in X
analysis = uq.analysis.QMCAnalysis(sampler=m_sampler, qoi_cols=output_columns)
m_campaign.apply_analysis(analysis)


# get results: statistics for the X posterior
results = m_campaign.get_last_analysis()

stat = {}
for qoi in output_columns:
    stat[qoi] = results['statistical_moments'][qoi]

    print(qoi)
    print('Stats: \n', stat[qoi] )