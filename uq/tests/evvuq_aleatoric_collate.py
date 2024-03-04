import os
import sys

import numpy as np
import pandas as pd

from itertools import product

import pickle

import chaospy as cp

### From easyvvuq
import easyvvuq as uq
from easyvvuq.actions import Encode, Decode, Actions, CreateRunDirectory, ExecuteLocal, QCGPJPool

### Form qcg-pj

### From current package
from base.transport_csv_encoder import TransportCSVEncoder
from base.profile_cpo_decoder import ProfileCPODecoder
from base.plots import plot_moments, plot_sobols


def prepare_results(result, output_columns):

    moment_list = ['mean', 'std', 'skewnewss', 'kurtosis'] # TODO lookup eUQ docu

    moments = {{moment: result.describe(qoi, moment) for moment in moment_list} for qoi in output_columns}

    print(f"QoI moments:\n{moments}")

    return moments

def plot_results(result, input_params, output_columns):

    # To store Statistics and Sobols
    means = {qoi: [] for qoi in output_columns}
    stds = {qoi: [] for qoi in output_columns}
    sob1 = {qoi: [] for qoi in output_columns}
    sobt = {qoi: [] for qoi in output_columns}

    # Store results - TODO has to be done on per f.t. basis
    for qoi in output_columns:
        means[qoi].append(result.describe(qoi, 'mean'))
        stds[qoi].append(result.describe(qoi, 'std'))
        s1 = {}
        st = {}
        for par in list(input_params.keys()):
            s1.update({par: result.sobols_first(qoi)[par]})
            st.update({par: result.sobols_total(qoi)[par]})
        sob1[qoi].append(s1)
        sobt[qoi].append(st)

    # Plot the results
    for i, qoi in enumerate(output_columns):
        plot_moments(means[qoi], stds[qoi],
                     xlabel="Flux tubes", ylabel=qoi,
                     ftitle="GEM0SUR-WF: descriptive statistics for "+qoi,
                     fname="aleatoric_stats"+str(i)+".png")

        #TODO: for radially well-resolved quantities, plot mean+shaded ~2*sigma
        #TODO: if KDE exists, plot KDE for ti_value_0, te_value_0
        for j in range(8):
                plot_sobols(sob1[qoi][j],
                            xlabel="Uncertain parameters", ylabel="First sobol",
                            ftitle='GEM0SUR-WF: First sobol indices \n Flux tube: '+str(j+1)+' - QoI: '+qoi,
                            fname='gem0surrwf_al_sob1_'+str(i)+str(j)+'.png')
                plot_sobols(sobt[qoi][j],
                            xlabel="Uncertain parameters", ylabel="Total sobol",
                            ftitle='GEM0SUR-WF: Total sobol indices \n Flux tube: '+str(j+1)+' - QoI: '+qoi,
                            fname='gem0surrwf_al_sobt_'+str(i)+str(j)+'.png')


def main(db_location):
    
    # Global params
    # print(f"> Reading environment variables")
    # SYS = os.environ['SYS']
    # mpi_instance =  os.environ['MPICMD']
    # mpi_model = os.environ['MPIMOD']
    # wrk_dir = tmp_dir = os.environ['SCRATCH']
    # slurm_nodes = int(os.environ['SLURM_NNODES'])

    species = ['e', 'i']
    # ft_coords = [0.143587306141853 , 0.309813886880875 , 0.442991137504578 , 0.560640752315521 , 0.668475985527039 , 0.769291400909424 , 0.864721715450287 , 0.955828309059143]
    prof_coord_inds = [i for i in range(100)]

    n_samples = int(sys.argv[2]) if len(sys.argv) > 2 else 32
    print(f">> Using number of samples: {n_samples}")

    input_cv = float(sys.argv[1]) if len(sys.argv) > 3 else 0.1
    print(f">> Using input C.o.V. of {input_cv}")

    # codename = sys.argv[1] if len(sys.argv) > 4 else 'gem0'
    # print(f">> Using data from code {input_cv}")

    # # - option 3.1 - use pyGEM0 data
    # if codename == 'gem0':
    #     base_dataset_filename = "gem0py_new_baseline.csv"
    # # - option 3.2 - use GEM data
    # elif codename == 'gem':
    #     base_dataset_filename = "gem_new_baseline.csv"
    
    # test_script_dir = "tests"
    # target_dataset_filename = "gem0py_new_local.csv"
    output_filename = "ets_coreprof_out.cpo"

    output_columns = [f"t{sp}_value_{coord}" for sp,coord in zip(species, prof_coord_inds)]

    ### Pepare the campaign: crate the campaign object, copy the right files to the right places

    ### Decoders
    # Code of M3-WF run (foldername) -> [QoI] (ion) temperature @rho_tor_norm=0
    print("> Creating Decoder")
    output_cpo_path = ''
    decoder = ProfileCPODecoder(cpo_filename=output_filename, 
                                cpo_path=output_cpo_path,
                                output_columns=output_columns)

    # actions = Actions(
    #
    #     Decode(decoder),
    #                  )

    ### Run EasyVVUQ campaign

    # QCJ-PG specs
    # eUQ campaign
    print(f"> Setting Campaign parameters")
    camp_name = "UQ_8FTGEM0_WF_AL_"

    # Reload the campaign
    campaign = uq.Campaign(name=camp_name, db_location=db_location)
    
    # Reactivare the sampler
    sampler = campaign.get_active_sampler()
    campaign.set_sampler(sampler)

    ### Result analysis
    print(f"> Performing Analysis")
    # campaign.apply_analysis(analysis)
    analysis_result = campaign.get_last_analysis()

    # Moments of QoI: AVG, STD, SCW, KRT
    print(analysis_result.describe())

    pickle_filename = f"result_aleatoric_wf_ns{n_samples}_cv{input_cv}.pickle"
    with open(pickle_filename, "bw") as file_pickle:
        pickle.dump(analysis_result, file_pickle)

    # TODO get input parameters from the campaign DB
    # plot_results(analysis_result, input_params, output_columns)
    

if __name__ == "__main__":

    db_location = ''

    main(db_location)
