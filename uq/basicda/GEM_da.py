import pandas as pd
import numpy as np
import matplotlib.pylab as plt
import os

import itertools

from sklearn.neighbors import KernelDensity as KDE
from scipy.stats import moment

#from da_utils import 

from ascii_cpo import read

def func_from_data(x, data):
    return data[1, (np.abs(data[0,:] - x)).argmin()]

def AUG_GM_date_explore(filename='AUG_gem_inoutput.txt'):
    """
    :param filename: a csv file with the sim data
    :return:
    """

    """Read GEM data """
    AUG_gem = pd.read_table(filename, delimiter='  *')
    pd.options.display.max_columns = AUG_gem.shape[1]

    """ Data exploration """
    desc = AUG_gem.describe(include='all')
    #print(desc)

    #splot = AUG_gem.plot('dTe-ft1', ['flux-Ti-ft1'], 'scatter', logy=True)
    #splot.figure.savefig('scatterplot1.pdf')
    #plt.show(block=True)

    flti1_plot_time = AUG_gem.plot('time', ['flux-Ti-ft3'])
    flti1_plot_time.figure.savefig('fluxTi3_wtime.pdf')
    plt.show(block=True)

    y1 = AUG_gem['dTe-ft1']
    print('[' + str(y1.min()) + ';' + str(y1.max()))
    y1 = pd.to_numeric(y1, downcast='float')
    y1.fillna(value=pd.np.nan, inplace=True)

    #GP_analysis_toy(X=AUG_gem['time'], y=y1)

def SA_exploite(analysis, qoi):
    # Function to analyses the SVD decomposition of the Sobol indices matrix and find largest eigenvaules among
    # linear combinations of Sobol indices of different order.
    # Further should use the eigen vectors as "directions" for new resampling for UQ quadratures

    #stat = {}
    sob1 = {}
    sob2 = {}
    #stat[qoi] = analysis['statistical_moments'][qoi]
    sob1[qoi] = analysis['sobols_first'][qoi]
    sob2[qoi] = analysis['sobols_second'][qoi]

    sens_mat = np.zeros((qoi.len(), qoi.len()))
    sens_mat.fill_diagonal(sob1)
    for sob in sob2:
        sens_mat[sob.keys()[0]][sob.keys()[1]] = sob.values()
        sens_mat[sob.keys()[1]][sob.keys()[0]] = sob.values()  
    
    sens_eigva, sens_eigve =  np.linalg.eigh(sens_mat)
    for ea, ee in zip(sens_eigva, sens_eigve): 
        print('E.Val. {0} for E.Vec. {1}'.format(ea,ee))

#AUG_GM_date_explore(filename='../data/AUG_gem_inoutput.txt')

def profile_evol_load(rho=0.69, folder_name='../gem_data/cpo5/', prof_names=['ti_transp', 'te_transp'], attrib_names = ['flux'], file_code_name = 'gem', name_postfix=''):
    
    #prof_names = ['ti_transp.flux', 'te_transp.flux']
    #prof_names = ['ti_transp', 'te_transp']
    file_base_name = 'gem_coretransp'
    file_ext = '.cpo'

    file_names = [f for f in os.listdir(folder_name) if 
                             os.path.isfile(os.path.join(folder_name, f)) and 
                             f.endswith(file_ext) and
                             f.startswith(file_code_name)
                ]

    file_names.sort()
    
    n = len(prof_names)
    m = len(attrib_names)
    value_s = [[] for _ in itertools.product(prof_names, attrib_names)]

    for file_name in file_names:
        coretransp = read(os.path.join(folder_name, file_name), 'coretransp')
        for i, profname in enumerate(prof_names):
            prof = getattr(coretransp.values[0], profname)
            for j, attrib in enumerate(attrib_names):
                if profname[1] == 'i':
                     value_s[i*m+j].append(getattr(prof, attrib)[0])
                elif profname[1] == 'e':
                     value_s[i*m+j].append(getattr(prof, attrib)) 
                else:
                     print('attributes have to belong either to ions or electrons')
    
    for i,(prof,attrib) in enumerate(itertools.product(prof_names, attrib_names)):
        np.savetxt(file_code_name + '_' + prof + '_' + attrib  + '_evol' + name_postfix +'.csv', value_s[i], delimiter =", ", fmt ='% s')

    #return [value[0] for value in value_s], file_names
    return value_s, file_names

def profile_evol_plot(value_s, file_names=[], name='gem_ti_flux'):

    value_s_pointwise = []
    ts = np.arange(len(file_names))

    # fig, ax = plt.subplots()
    # rhos = np.arange(len(value_s))
    for num, value in enumerate(value_s):
    #     ax.plot(rhos, value, label=num)
         value_s_pointwise.append(value)
    # plt.savefig(name + '.png')
    # plt.close()

    ##print(value_s_pointwise)

    fig, ax = plt.subplots(figsize=(24.,8.))
    ax.plot(ts, value_s_pointwise, 'bo-')
    plt.savefig(name + '.png')
    plt.close()

    np.savetxt(name + '.csv', value_s_pointwise, delimiter =", ", fmt ='% s')

    return value_s_pointwise

def plot_coreprofval_dist(value_spw, name='ti'):

    # plot historgrams
    fig, ax = plt.subplots()
    ax.hist(value_spw, bins=len(value_spw)//4)
    plt.savefig('hist_' + name + '.png')

    # get and plot KDE fit
    kde = KDE(kernel='gaussian', bandwidth=(value_spw.max()-value_spw.min())/10.).fit(value_spw[:, np.newaxis])

    x = np.linspace(0.9*value_spw.min(), 1.1*value_spw.max(), 100)[:, np.newaxis]
    log_pdf = kde.score_samples(x)
    log_pdf_orig = kde.score_samples(value_spw[:, np.newaxis])

    fig, ax = plt.subplots()
    ax.plot(x[:, 0], np.exp(log_pdf), label='density of core transport values')
    ax.plot(value_spw, (-0.01*np.random.rand(log_pdf_orig.shape[0])) * np.exp(log_pdf_orig).min(),'+k')
    plt.savefig('pdf_' + name + '.png')
    plt.close()

    # print main moments
    mom_len = 5
    moments = []
    print(value_spw.mean())
    #print(((value_spw*value_spw).mean() - value_spw.mean()*value_spw.mean())/1.)
    
    with open('stats_' + name + '.txt', 'w') as wfile:
        for n in range(mom_len):
            m = moment(value_spw, moment=n)
            line = '{0}-th moment is: {1:.3e}'.format(n, m)
            print(line)
            wfile.write(line+'\n')

    return moments

def get_coreprof_ev_acf(value_ev, name='ti', lags=[1,2,3,4,5,6,7,8,9,10]):
    
    res = [1. if l==0 else np.corrcoef(value_ev[l:], value_ev[:-l])[0][1] for l in lags]
    #res = res[res.size//2:]
    res = np.array(res)
    line = 'ACF :' + str(res)
    print(line)
    with open('acf.txt', 'w') as wfile:
        wfile.write(line)

    return res

###########################################

workdir = os.path.join(os.getenv('SCRATCH'), 'MFW_runs')

code_names = ['gem',
              # 'imp4dv',
             ]
profiles = ['ti_transp', 
            'te_transp',
           # 'ni_transp',
           # 'ne_transp'
           ]

for code_name in code_names:
    if code_name == 'imp4dv':
        attributes = ['diff_eff','vconv_eff']
    if code_name == 'gem':
        attributes = ['flux']

    val_ev_s, file_names = profile_evol_load(prof_names=profiles, attrib_names=attributes, folder_name=os.path.join(workdir, 'cpo7'), file_code_name=code_name, name_postfix='_nst25')

    for i,(p,a) in enumerate(itertools.product(profiles, attributes)):
        val = profile_evol_plot(val_ev_s[i], file_names, name=p+'_'+a+'_nst5')
        val = np.array(val).squeeze()
        ##ti_flux = np.genfromtxt('gem_ti_flux.csv', delimiter =", ")
        get_coreprof_ev_acf(val)
        plot_coreprofval_dist(val, name=p+'_'+a+'_nst5')

