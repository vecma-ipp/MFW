import pandas as pd
import numpy as np
import matplotlib.pylab as plt
import os
import sys

import itertools

from sklearn.neighbors import KernelDensity as KDE
from scipy.stats import moment

import statsmodels.api as sm

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

def profile_evol_load(rho=0.69, folder_name='../gem_data/cpo5/', prof_names=['ti_transp', 'te_transp'], attrib_names=['flux'], file_code_name='gem', name_postfix=''):
    
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
    #print('filenames'); print(file_names) ### DEBUG
    
    n = len(prof_names)
    m = len(attrib_names)
    value_s = [[] for _ in itertools.product(prof_names, attrib_names)]

    for file_name in file_names:
        coretransp = read(os.path.join(folder_name, file_name), 'coretransp')
        #print('coretransp'); print(coretransp) ### DEBUG
        for i, profname in enumerate(prof_names):
            #print('profname'); print(profname) ### DEBUG
            #print('values'); print(coretransp.values) ### DEBUG
            #print('values dict'); print(coretransp.values.__dict__) ### DEBUG          
            #print('values[0]'); print(coretransp.values[0]) ### DEBUG
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
        print('Last value (num. {1}) is: {0}'.format(value_s[i][-1], len(value_s[i])))        

    #return [value[0] for value in value_s], file_names
    return value_s, file_names

def profile_evol_plot(value_s, labels=[], file_names=[], name='gem_ti_flux'):
    
    #ts = np.arange(len(file_names))
    ts = np.arange(value_s[0].shape[0])

    ## rhos = np.arange(len(value_s))
    #for num, value in enumerate(value_s):
    ##     ax.plot(rhos, value, label=num)
    #     value_s_pointwise.append(value)
    ## plt.savefig(name + '.png')
    ## plt.close()

    ##print(value_s_pointwise)

    fig, ax = plt.subplots(figsize=(24.,8.))
    for value, lab in zip(value_s, labels):
        ax.plot(ts, value, '-', label=lab)
    plt.legend(loc='best')
    plt.savefig(name + '.png')
    plt.close()

    np.savetxt(name + '.csv', value_s, delimiter =", ", fmt ='% s')

    return value_s

def plot_coreprofval_dist(value_spw, name='ti', discr_level=64):

    # plot historgrams
    fig, ax = plt.subplots()
    ax.hist(value_spw, bins=len(value_spw)//discr_level)
    plt.savefig('hist_' + name + '.png')

    # get and plot KDE fit
    kde = KDE(kernel='gaussian', bandwidth=(value_spw.max()-value_spw.min())/discr_level).fit(value_spw[:, np.newaxis])

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
    print('mean value is: ' + str(value_spw.mean()))
    #print(((value_spw*value_spw).mean() - value_spw.mean()*value_spw.mean())/1.)
    
    with open('stats_' + name + '.txt', 'w') as wfile:
        for n in range(mom_len):
            if n == 1:
                m = value_spw.mean()
            else:
                m = moment(value_spw, moment=n)
            moments.append(m)
            line = '{0}-th moment is: {1:.3e}'.format(n, m)
            print(line)
            wfile.write(line+'\n')

    # check the normality of distribution function
    kld = compare_gaussian(np.exp(log_pdf), x, moments)
    print('KL-d is: ' + str(kld))

    return moments

def get_coreprof_ev_acf(value_ev, name='ti', lags=[1,2,3,4,5,6,7,8,9,10]):
    
    res = [1. if l==0 else np.corrcoef(value_ev[l:], value_ev[:-l])[0][1] for l in lags]
    #res = res[res.size//2:]
    res = np.array(res)
    line = 'ACF :' + str(res)
    print(line)
    with open(name+'_acf.txt', 'w') as wfile:
        wfile.write(line)

    return res

def filter_trend(values, method='hpf' ):
    """
    Applies filter to split every time series into stationary and non-stationary part
    
    returns two data structure of same dimension and type as input, one corresponds to trend, another to fluctuations
    """
    
    if method == 'hpf':
        lam = 0.5*1e9
        valpd = pd.DataFrame(values, columns=["ti_transp_flux"])
        val_cycle, val_trend = sm.tsa.filters.hpfilter(valpd.ti_transp_flux, lam)
    elif method == 'exp':
        alpha = 0.01
        valpd = pd.DataFrame(values, columns=["ti_transp_flux"])
        #smoothing_model = sm.tsa.ExponentialSmoothing(values).fit()
        smoothing_model = sm.tsa.SimpleExpSmoothing(valpd, initialization_method="heuristic").fit(smoothing_level=alpha, optimized=False)
        val_trend = smoothing_model.fittedvalues
        val_cycle = valpd - val_trend
        print('smoothing of value: the found alpha= {}'.format(smoothing_model.model.params["smoothing_level"]))
        print(val_trend)
    elif method == 'mean':
        val_trend = values.mean()*np.ones(values.shape)
        val_cycle = values - val_trend   
 
    return np.array(val_trend), np.array(val_cycle)

def compare_gaussian(pdf, domain, moments):
    # ***
    # takes pdf as f(x) e [0,1], x e {range of quantity values}
    # checks if central moments of order > 2 are neglegible
    # calculates KL of given pdf and pdf of a Gaussian with with given first two moments, E and sigma
    # ***

    for i, mom in enumerate(moments[2:]):
        if abs(mom) > 1e-2*np.power(moments[0], i+2):
            print('moment num '+str(i+2)+' is large')    

    delta = (domain[-1] - domain[0])/len(domain)

    x = 1. # np.arange(len(pdf)) # np.linspace(valmin, valmax, valres)
    x = domain

    pref = 1./(np.sqrt(2.*np.pi)*moments[1])
    determ = 2.*moments[1]*moments[1]
    
    pdf_gauss = pref*np.exp((moments[0]-x)*(moments[0]-x)/determ)

    kl_div = delta*np.multiply(pdf, np.log(np.divide(pdf, pdf_gauss))).sum()

    return kl_div

###########################################

def main(foldername='17', runforbatch=False):

    mainfoldernum = foldername

    workdir = os.path.join(os.getenv('SCRATCH'), 'MFW_runs')

    code_names = ['gem',
#                 'imp4dv',
           	 ]
    profiles = ['ti_transp', 
#                'te_transp',
#                'ni_transp',
#                'ne_transp'
               ]

    for code_name in code_names:
        if code_name == 'imp4dv':
       	    attributes = ['diff_eff','vconv_eff']
        if code_name == 'gem':
       	    attributes = ['flux']

        if not runforbatch:
            val_ev_s, file_names = profile_evol_load(prof_names=profiles, attrib_names=attributes, folder_name=os.path.join(workdir, 'cpo'+mainfoldernum), file_code_name=code_name, name_postfix='_'+mainfoldernum)
        val_ev_s = []

        for i,(p,a) in enumerate(itertools.product(profiles, attributes)):
            val_ev_s.append(np.genfromtxt(code_name+'_'+p+'_'+a+'_evol_'+mainfoldernum+'.csv', delimiter=", "))     
 
            profile_evol_plot([val_ev_s[i]], name=code_name+'_'+p+'_'+a+'_'+mainfoldernum)
            val = np.array(val_ev_s[i]).squeeze()
       
            ### print(val_ev_s[i].shape); print(val.shape) ### DEBUG 
            ##ti_flux = np.genfromtxt('gem_ti_flux.csv', delimiter =", ")
        
            get_coreprof_ev_acf(val, name=code_name+'_'+p+'_'+a+'stats', lags =[1,2,4,8,16,32,64,128])
            plot_coreprofval_dist(val, name=p+'_'+a+'_'+mainfoldernum)

            # plot different averagings
            alpha_wind = 0.33
            val_wind = val[:-int(alpha_wind*len(val))]            
            val_trend_avg, val_fluct_avg = filter_trend(val_wind, "mean")
            
            val_trend_hp, val_flux_exp = filter_trend(val, "hpf")
            val_trend_exp, val_fluct_exp = filter_trend(val, "exp")
            profile_evol_plot([val, val_trend_hp, val_trend_exp, np.ones(val.shape)*val_trend_avg[0]], 
                              labels=['original','hpf', 'exponential(alpha=0.01)', 'mean(of {:.2e} after {} steps)'.format(val_trend_avg[0], int(alpha_wind*len(val)))],
                              name='trend_'+p+'_'+a+'_'+mainfoldernum)
             
            # histogram for the last alpha_window values
            plot_coreprofval_dist(val_fluct_avg, name='wind'+code_name+p+'_'+a+'_'+mainfoldernum)

            #TODO get exponential average of the values: standard packaged optimize for alpha -- why it is so high? is composition of exponential avaraging is another exponential averagin -- if so, what is alpha_comp?
            #TODO get the trend as average after lamb=0.2-0.4 steps 
            #TODO plot three trends against the valeus, and the histograms of residuals for all averagings
            #TODO make FFT of series and split in Fourier space by thresholding

if __name__ == '__main__':

    if len(sys.argv) == 2:
        main(foldername=sys.argv[1])
    elif len(sys.argv) == 3:
        main(foldername=sys.argv[1], runforbatch=int(sys.argv[2]))
    else:
        main()

