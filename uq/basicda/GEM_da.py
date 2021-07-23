import pandas as pd
import numpy as np
import matplotlib.pylab as plt
import os

from sklearn.neighbors import KernelDensity as KDE
from scipy.stats import moment

from da_utils import *

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

def profile_evol_load(rho=0.69, folder_name='../gem_data/cpo5/', attrib_name = ['flux'], file_code_name = 'gem'):
    
    #prof_names = ['ti_transp.flux', 'te_transp.flux']
    prof_names = ['ti_transp', 'te_transp']
    file_base_name = 'gem_coretransp'
    file_ext = '.cpo'

    file_names = [f for f in os.listdir(folder_name) if 
                             os.path.isfile(os.path.join(folder_name, f)) and 
                             f.endswith(file_ext) and
                             f.startswith(file_code_name)
                ]
    value_s = [[] for _ in prof_names]

    for file_name in file_names:
        coretransp = read(os.path.join(folder_name, file_name), 'coretransp')
        for i, prof in enumerate(prof_names):
            prof = getattr(coretransp.values[0], prof)
            for attrib in attrib_name:
                value_s[i].append(getattr(prof, attrib)[0])
    
    for prof in prof_names:
        np.savetxt(file_code_name + '_' + prof + '_evol.csv', value_s, delimiter =", ", fmt ='% s')

    return value_s[0], file_names

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

    fig, ax = plt.subplots()
    ax.plot(ts, value_s_pointwise, 'bo-')
    plt.savefig(name + '_t.png')
    plt.close()

    np.savetxt(name + '.csv', value_s_pointwise, delimiter =", ", fmt ='% s')

    return value_s_pointwise

def plot_coreprofval_dist(value_spw, name='ti'):

    # plot historgrams
    fig, ax = plt.subplots()
    ax.hist(value_spw, bins=len(value_spw)//4)
    plt.savefig('hist_' + name + '_flux.png')

    # get and plot KDE fit
    kde = KDE(kernel='gaussian', bandwidth=(value_spw.max()-value_spw.min())/10.).fit(value_spw[:, np.newaxis])

    x = np.linspace(0.9*value_spw.min(), 1.1*value_spw.max(), 100)[:, np.newaxis]
    log_pdf = kde.score_samples(x)
    log_pdf_orig = kde.score_samples(value_spw[:, np.newaxis])

    fig, ax = plt.subplots()
    ax.plot(x[:, 0], np.exp(log_pdf), label='density of core transport values')
    ax.plot(value_spw, (-0.01*np.random.rand(log_pdf_orig.shape[0])) * np.exp(log_pdf_orig).min(),'+k')
    plt.savefig('pdf_' + name + '_flux.png')
    plt.close()

    # print main moments
    mom_len = 5
    moments = []
    print(value_spw.mean())
    #print(((value_spw*value_spw).mean() - value_spw.mean()*value_spw.mean())/1.)

    for n in range(mom_len):
        m = moment(value_spw, moment=n)
        print('{0}-th moment is: {1:.3f}'.format(n, m))
    
    return moments

###########################################

val_ev, file_names = profile_evol_load(attrib_name = ['flux'], file_code_name='gem')
val = profile_evol_plot(val_ev, file_names, name='tiflux')
val = np.array(val).squeeze()
#ti_flux = np.genfromtxt('gem_ti_flux.csv', delimiter =", ")
plot_coreprofval_dist(val, name='ti')
