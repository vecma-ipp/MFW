import pandas as pd
import numpy as np
import math
import matplotlib.pylab as plt
import sys
import os
import csv
from itertools import product

from da_utils import *
from extcodehelper import ExtCodeHelper

def rmse(y_observ, y_model):
    if len(y_observ.shape) == 2:
        res_sq = 0
        for j in range(y_observ.shape[1]):
            res_sq = res_sq + np.square(y_observ[:, j] - y_model[:, j]).mean()
    else:
        res_sq = np.square(y_observ - y_model).mean()
    return np.sqrt(res_sq)

def f_score(y_observ, y_model):
    s5 = np.sum(np.square(y_observ - y_model))
    sm = np.sum(np.square(y_observ - y_observ.mean()))
    num_dofs_as = y_observ.shape[0] - 4  # x_observ.shape[0], for our case 4
    num_dofs_me = y_observ.shape[0] - 1
    score = (s5 * num_dofs_me) / (sm * num_dofs_as)
    return score

def load_csv_file(data_dir='', input_file='gem_data_625.txt'):
    N_runs = 625
    input_dim = 4
    output_dim = 2

    input_samples = np.zeros((N_runs, input_dim))
    output_samples = np.zeros((N_runs, output_dim))

    # input_file = data_dir + '\\' + input_file

    with open(input_file, 'r') as inputfile:
        datareader = csv.reader(inputfile, delimiter=',')
        i = 0
        for row in datareader:
            input_samples[i] = row[0:input_dim]
            output_samples[i] = row[input_dim:input_dim + output_dim]
            i = i + 1

    return input_samples, output_samples


### --- Produce GEM0 results for coordiantes of paramteric space read from a GEM campaign

# create a caller to gem0 code
gem0_helper = ExtCodeHelper(1)

# create a function to pass 4 params to gem0 and get 2 targets back
# function = lambda x: np.array(gem0_helper.gem0_call_4param2target_array(x))

# function to load a gem campaign data if stored at csv

x_domain, y_gem = load_csv_file()
# y_test = function(x_domain)
# y_test = y_test.reshape(625, 2)

# df = pd.DataFrame()
# df['te.value'] = x_domain[:, 0]
# df['ti.value'] = x_domain[:, 1]
# df['te.ddrho'] = x_domain[:, 2]
# df['ti.ddrho'] = x_domain[:, 3]
# df['te.flux'] = y_test[:, 0]
# df['ti.flux'] = y_test[:, 1]
# df.to_csv('gem0_625_res.csv')


### --- Try to fit GEM0 for GEM data

from scipy.optimize import curve_fit
from scipy.stats import pearsonr

np.set_printoptions(precision=2)


# function to call gem0 with signature suited for scipy curve_fit
def func(x,
         thresh,
         beta_reduction,
         # etae_pinch
         chi_d,
         chiratio_phi,
         x_names=['te.value', 'ti.value', 'te.ddrho', 'ti.ddrho'],
         y_names=['ti.transp.flux']
         ):
    x_dicts = []
    for j in range(x.shape[0]):
        xrow = x[j, :]

        dict = {}
        for i_n in range(len(x_names)):
            dict[x_names[i_n]] = xrow[i_n]

        x_dicts.append(dict)

    params = {
        'thresh': thresh,
        'beta_reduction': beta_reduction,
         # 'etae_pinch': etae_pinch,
        'chi_d': chi_d,  # 60.
        'chiratio_phi': chiratio_phi  # 1.
    }

    y_res_list_dicts = gem0_helper.gem0obj.gem0_fit_call(x_dicts, params)

    y_result_list = []
    for y_r in y_res_list_dicts:
        y_result_list.append(y_r[y_names[0]])

    y_result_return = np.array(y_result_list)  #.reshape(-1, 1)
    return y_result_return


# get a slice of gem data and run gem0 for same values, then compare
# function = lambda x: np.array(gem0_helper.gem0_call_tefltegrad_array(x))
# function = lambda x: np.array(gem0_helper.gem0_call_tefltigrad_array(x))
# function = lambda x: np.array(gem0_helper.gem0_call_tifltegradtigrad_array(x))
# function = lambda x: np.array(gem0_helper.gem0_call_tefl4params_array(x))

# feat_inds = [3]
# xinds = grid_slice(x_domain, feat_inds)
# xcur = x_domain[xinds.tolist()][:, feat_inds]
# ycur = y_gem[inds, 1]
# y_curgem0 = function(xcur).reshape(-1)

#fit the free paramters of gem0 with curve_fit

# popt, pconv = curve_fit(func, x_domain, y_gem[:, 1],
#                         p0=[
#                             4.,
#                             1.,
#                             # 3.5,
#                             3.,  # 100.
#                             0.7  # 0.1
#                         ],
#                         bounds=(0.0001, 100000.0),
#                         method='dogbox',
#                         # jac='3-point'
#                         )  # check if x should actually be (nsamples, nfeatures)
#
# print('optimal free parameter value: {}'.format(popt))

#for te_transp_flux try [5.04 7.61]
#for ti_transp_flux try [2.53e+01 1.00e-04]
#4 params for te_transp_flux [2.87e+00 2.50e-03 6.00e+01 1.00e-01]
#4 params for ti_transp_flux [5.00e-01 1.00e-04 6.00e+01 5.46e-01]
# print(pconv)

# y_fitted = func(x_domain, *popt)
# pears, _ = pearsonr(y_gem[:, 0], y_fitted)
# print('pearson correlation after fitting: {}'.format(pears))

### --- Get plots for different free parameters

plt.ioff()

y_names = {'te.transp.flux': (0, 'te'), 'ti.transp.flux': (1, 'ti')}
x_names = {'te.value': (0, 'te'), 'ti.value': (1, 'ti'), 'te.ddrho': (2, 'gte'), 'ti.ddrho': (3, 'gti')}

y_lim_gem = [(0., 13000.), (0., 5500.)]

#popt = {'threshold': 2.87, 'beta_reduction': 2.50e-03, 'chi_d': 60., 'chiratio_phi': 0.546}
popt = {'threshold': .5, 'beta_reduction': 1e-4, 'chi_d': 76.7, 'chiratio_phi': 0.7}

popt = {'threshold': 4.0, 'beta_reduction': 1.0, 'chi_d': 3., 'chiratio_phi': 0.7}  # original suggestion

# opt in te_transp_flux [3.51e+00 2.58e-03 5.37e+01 7.00e-01]
#popt = {'threshold': 4.98e-01, 'beta_reduction': 1.00e-04, 'chi_d': 53.7, 'chiratio_phi': 0.487}  # opt in ti_tranps_flux

betars = np.linspace(0.8 * popt['beta_reduction'], 1.2 * popt['beta_reduction'], 3)
thresholds = np.linspace(0.8 * popt['threshold'], 1.2 * popt['threshold'], 3)

#betars = np.logspace(-1, +1, 3) * popt['beta_reduction']
thresholds = np.logspace(-1, +1, 3) * popt['threshold']

# betars = np.logspace(0, 3, 4)
# thresholds = np.linspace(1.5, 9.0, 4)

# betars = [1., 5., 10, 15.]
# thresholds = [1., 4., 6., 10.]


# Plot graphs for single R->R mapping, at each graph multiple combinations of free parameter values
def plot_diff_freparams(thresholds, betars):
    for y_n, (y_dim_num, y_short_name) in y_names.items():
        for x_n, (x_dim_num, x_short_name) in x_names.items():
            xinds = grid_slice(x_domain, [x_dim_num]).tolist()
            xcur = x_domain[xinds][:, [x_dim_num]]
            xcur_more = x_domain[xinds]

            fig, ax = plt.subplots()
            for par in product(thresholds, betars):
                par = par + (popt["chi_d"], popt["chiratio_phi"])
                y_param_st = func(xcur_more, *par, y_names=['ti.transp.flux'])
                ax.plot(xcur, y_param_st, label='thr: {}; beta_r: {}'.format(par[0], par[1]))

            plt.plot(xcur, y_gem, 'o-', label='gem'.format(x_short_name, y_short_name))
            #plt.xlabel(r'$\nabla Ti$')
            plt.xlabel(x_short_name)
            plt.ylabel(r'Ф_{}'.format(y_short_name))
            plt.legend(loc='best')
            plt.title('Transport fluxes from GEM0 for different beta_reduction and threshold values')
            plt.savefig('gem0_{}_{}_thrlin_betarlin.png'.format(y_short_name, x_short_name))
            plt.close()

plot_diff_freparams(thresholds, betars)

# Plot for a concrete free parameter values, at each graph all possible R->R mappings

def plot_diff_response(thresholds, betars):
    for par in product(thresholds, betars):
        par = par + (popt["chi_d"], popt["chiratio_phi"])
        fig, ax = plt.subplots(2, 4, figsize=(19, 10))  #, gridspec_kw={'wspace': 0}), sharey='row')
        si = 0
        ygem0_glob = np.zeros(y_gem.shape)
        for y_n, (y_dim_num, y_short_name) in y_names.items():
            sj = 0
            for x_n, (x_dim_num, x_short_name) in x_names.items():
                xinds = grid_slice(x_domain, [x_dim_num]).tolist()

                xcur = x_domain[xinds][:, [x_dim_num]]
                xcur_more = x_domain[xinds]

                xcur_long = np.linspace(xcur.min(), xcur.max(), 20)
                xcur_more_long = np.zeros((20, xcur_more.shape[1]))
                xcur_more_long[:, x_dim_num] = xcur_long
                for dim in [dim for dim in range(xcur_more.shape[1]) if dim != x_dim_num]:
                    xcur_more_long[:, dim] = xcur_more[0, dim]

                ycur_gem = y_gem[xinds, y_dim_num]
                y_param_st = func(xcur_more_long, *par, y_names=[y_n])  #, x_names=[x_n],)

                y_compare = func(xcur_more, *par, y_names=['ti.transp.flux'])
                ygem0_glob[xinds, y_dim_num] = y_compare

                #ax[si][sj].set_ylim(y_lim_gem[si])

                ax[si][sj].plot(xcur_long, y_param_st, 'o-', label='gem0 opt for {}->{}'.format(x_short_name, y_short_name))
                ax[si][sj].plot(xcur, ycur_gem, 'o-', label='gem data for {}->{}'.format(x_short_name, y_short_name))

                ax[si][sj].set_title('{}->{}'.format(x_short_name, y_short_name))
                ax[si][sj].set_ylabel(r'Ф_{}'.format(y_short_name))
                ax[si][sj].set_xlabel(r'{}'.format(x_n))
                plt.legend(loc='best')
                sj = sj + 1
            si = si + 1
        #print('gem0 rmse for params thr={} b_r={}: {}'.format(par[0], par[1], rmse(y_gem, ygem0_glob)))
        #print(f_score(y_gem, ygem0_glob))

        # plt.legend(loc='best')
        fig.tight_layout()
        fig.suptitle('GEM0 with values of: threshold: {:.4f} ; beta_reduction: {:.7f}'.format(par[0], par[1]))
        fig.subplots_adjust(top=0.88)

        plt.savefig('gem0_thr{:.4f}_betar{:.7f}.png'.format(par[0], par[1]))
        plt.close()

plot_diff_response(thresholds, betars)
