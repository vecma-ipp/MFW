import numpy as np
import pandas as pd

from da_utils import plot_1D_scalings, plot_response_cuts


data_file_name = 'gem_data/resuq_main_ti_transp_flux_all_moj202gj_12.csv'
data_file_name = 'gem_data/w468l7ng/ti/resuq/4/resuq_main_ti_transp_flux_all_w468l7ng_4.csv'
data_file_name = 'gem_data/rp1pw2y6/te/resuq/2/resuq_extended_2.csv'

input_names = ['ti_value'] #, 'te_value', 'ti_ddrho', 'te_ddrho']

output_names=['ti_transp_flux']

scale_type = ['ti_value']

foldname = 'all_moj202gj_12_05032023'
foldname = 'all_w468l7ng_4_05032023'
foldname = 'all_rp1pw2y6_moretival_05032023'

data = pd.read_csv(data_file_name)

# plot_1D_scalings(data=data, 
#             #input_names=input_names,
#             output_names=output_names, 
#             scale_type=scale_type, 
#             foldname=foldname)

plot_response_cuts(data, #.iloc[[8,12]], 
                input_names=input_names,
                output_names=output_names,
                foldname=foldname)
