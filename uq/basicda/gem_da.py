import pandas as pd
import numpy as np
import matplotlib.pylab as plt
import os
import sys

import itertools

from sklearn.neighbors import KernelDensity as KDE
from scipy.stats import moment

import statsmodels.api as sm
from statsmodels.tsa.api import acf, pacf, graphics
from statsmodels.tsa.api import SimpleExpSmoothing, ExponentialSmoothing  
from statsmodels.graphics.tsaplots import plot_acf, plot_pacf
from statsmodels.graphics.api import qqplot
from statsmodels.tsa.ar_model import AutoReg
from statsmodels.tsa.arima.model import ARIMA, ARIMAResults

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
    plt.close()

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

def profile_evol_load(rho=0.69, folder_name='../gem_data/cpo5/', prof_names=['ti_transp', 'te_transp'], attrib_names=['flux'], coord_len=1, file_code_name='gem', name_postfix=''):
    
    #prof_names = ['ti_transp', 'te_transp']
    file_base_name = 'gem_coretransp'
    file_ext = '.cpo'

    file_names = [f for f in os.listdir(folder_name) if 
                             os.path.isfile(os.path.join(folder_name, f)) and 
                             f.endswith(file_ext) and
                             f.startswith(file_code_name)
                ]

    file_names.sort()
    #print('filenames'); print(len(file_names)); #print(file_names) ### DEBUG

    n = len(prof_names)
    m = len(attrib_names)
    f = len(file_names)
    d = coord_len
    coords = np.arange(d)

    #value_s = [[] for _ in itertools.product(prof_names, attrib_names)]
    #value_s = [[[]]*d for _ in value_s]
    value_s = np.zeros((n*m,d,f))
    
    #print('len of value_s and value_s[0]'); print(len(value_s)); print(len(value_s[0]))#print(value_s); print(coords) ### DEBUG

    for k, file_name in enumerate(file_names):
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
                     val_reading = getattr(prof, attrib)
                     #print('val_reading'); print(val_reading) ### DEBUG
                     for coord in range(d):
                         #print('before v[a][c]'); print(coord); print(value_s[i*m+j][coord]); print(val_reading[coord][0]) ### DEBUG
                         #value_s[i*m+j][coord].append(val_reading[coord][0])
                         value_s[i*m+j][coord][k] = val_reading[coord][0]
                         #print('after v[a][c]'); print(value_s[i*m+j]) ### DEBUG
                elif profname[1] == 'e':
                     val_reading = getattr(prof, attrib)
                     for coord in range(d):
                         #print(coord) ###DEBUG
                         #value_s[i*m+j][coord].append(val_reading[coord])
                         value_s[i*m+j][coord][k] = val_reading[coord]
                else:
                     print('attributes have to belong either to ions or electrons')
                #print(len(value_s[i*m+j][0])) ### DEBUG   
 
    for i,(prof,attrib) in enumerate(itertools.product(prof_names, attrib_names)):
        value_arr = np.array(value_s[i])
        np.savetxt(file_code_name + '_' + prof + '_' + attrib  + '_evol' + name_postfix +'.csv', value_s[i], delimiter =", ", fmt ='% s')
        print('Last value (num. {1}) is: {0}'.format(value_s[i,:,-1], value_s.shape[2]))        

    #print('check size of read structure = {}'.format(value_s.shape)) ### DEBUG
    #return [value[0] for value in value_s], file_names
    return value_s, file_names

def profile_evol_plot(value_s, labels=['orig'], file_names=[], name='gem_ti_flux'):
    
    #ts = np.arange(len(file_names))
    n = max([v.shape[-1] for v in value_s])
    print('n = {}, len(v) = {}, v[0].shape = {}'.format(n, len(value_s), value_s[0].shape)) ### DEBUG

    ## rhos = np.arange(len(value_s))
    #for num, value in enumerate(value_s):
    ##     ax.plot(rhos, value, label=num)
    #     value_s_pointwise.append(value)
    ## plt.savefig(name + '.png')
    ## plt.close()

    #print('value_s'); print(value_s) ### DEBUG

    fig, ax = plt.subplots(figsize=(24.,8.))
    for value, lab in zip(value_s, labels):
        #print('value.shape = {}'.format(value.shape)) ###DEBUG
        ts = np.arange(n - value.shape[-1], n)
        for i in range(value.shape[0]):
             #print('value[{0},:]'.format(i)); print(value[i,:]) ### DEBUG
             ax.semilogy(ts, value[i,:], '-', label=lab+'_'+str(i))
    plt.legend(loc='best')
    plt.savefig(name + '.png')
    plt.close()

    np.savetxt(name + '.csv', np.squeeze(value_s, 0), delimiter =", ", fmt ='% s')

    return value_s

def plot_coreprofval_dist(value_spw, name='ti', discr_level=64):

    #print('value_spw'); print(value_spw) ### DEBUG
    # plot historgrams
    nft = value_spw.shape[0]

    fig, ax = plt.subplots()
    for i in range(nft):
        ax.hist(value_spw[i, :], bins=len(value_spw[i, :])//discr_level)
    plt.savefig('hist_' + name + '.png')
    plt.close()

    # get and plot KDE fit
    # TODO: KDE does not handle 3-D arrays
    x = np.linspace(0.9*value_spw.min(), 1.1*value_spw.max(), 100)[:, np.newaxis]
    fig, ax = plt.subplots()
    for i in range(nft):
        kde = KDE(kernel='gaussian', bandwidth=(value_spw[i].max() - value_spw[i].min()) / discr_level).fit(value_spw[i, :, np.newaxis])
        log_pdf = kde.score_samples(x)
        log_pdf_orig = kde.score_samples(value_spw[i, :, np.newaxis])

        ax.plot(x[:, 0], np.exp(log_pdf), label='density of core transport values')
        ax.plot(value_spw[i], (-0.01*np.random.rand(log_pdf_orig.shape[0])) * np.exp(log_pdf_orig).min(), '+k')
    
    plt.savefig('pdf_' + name + '.png')
    plt.close()

    # print main moments
    # TODO check moments for all flux tubes
    mom_len = 5
    moments = []
    print('mean value is: ' + str(value_spw[0].mean()))
    #print(((value_spw*value_spw).mean() - value_spw.mean()*value_spw.mean())/1.)
    
    with open('stats_' + name + '.txt', 'w') as wfile:
        for n in range(mom_len):
            if n == 1:
                m = value_spw[0].mean()
            else:
                m = moment(value_spw[0], moment=n)
            moments.append(m)
            line = '{0}-th moment is: {1:.3e}'.format(n, m)
            print(line)
            wfile.write(line+'\n')

    # check the normality of distribution function
    kld = compare_gaussian(np.exp(log_pdf[0]), x, moments)
    print('KL-d is: ' + str(kld))

    return moments

def get_coreprof_ev_acf(value_ev, name='ti', lags=[1,2,3,4,5,6,7,8,9,10]):
    
    #acf = [1. if l==0 else np.corrcoef(value_ev[l:], value_ev[:-l])[0][1] for l in lags]
    
    nl = 64    
    
    #TODO read mft data here
    r,q,p  = acf(value_ev[0], nlags=nl, fft=True, qstat=True) 
 
    acf_data = np.c_[np.arange(1, nl+1), r[1:], q, p]
  
    acf_data_pd = pd.DataFrame(acf_data, columns=['lags', 'AC', 'Q', 'P(>Q)']) 
    acf_data_pd.set_index('lags')
 
    #acf_res = np.array(acf_res)

    #line = 'ACF :' + str(acf_res)
    
    print(acf_data_pd)

    #with open(name+'_acf.txt', 'w') as wfile:
    #    wfile.write(line)
    
    val_df = pd.DataFrame(value_ev[0])
   
    """ 
    plot_acf(val_df, lags=lags)
    plt.savefig('acf.png')
    plt.close()
    
    plot_pacf(val_df, lags=lags)
    plt.savefig('pacf.png')
    plt.close()
    """
    # DEBUG AND PLOTTING
    """
    plt.plot( acf, '.', label='ACF')
    plt.xlabel('lags')
    plt.ylabel('ACF')
    plt.title('Autocorrelation function')
    plt.ylim([-1., 1.])
    plt.savefig('debug_acf.png')
    plt.close()
    """

    return acf

def apply_arma(values):
    
    val_pd = pd.Series(values)
    
    mod = ARIMA(val_pd, order=(1,0,1))
    
    res = mod.fit()

    print(res.summary())

    resid = res.resid

    fig = plt.figure()
    ax = fig.add_subplot(111)
    qqplot(resid, line='q', ax=ax, fit=True)
    ax.set_ylim([-5,5])
    plt.savefig('qq.png')
    plt.close()

    return values

def filter_trend(values, method='hpf' ):
    """
    Applies filter to split every time series into stationary and non-stationary part
    
    returns two data structure of same dimension and type as input, one corresponds to trend, another to fluctuations
    """
    
    if method =='fft':
        thr = 2**(-10)
        thr_frac = 0.5

        val_spectrum = np.fft.fft(values)
        freq = np.fft.fftfreq(values.shape[0], 1.)

        val_slow_spectrum = val_spectrum.copy()
        val_fast_spectrum = val_spectrum.copy()

        #val_slow_spectrum[int(thr_frac*val_spectrum.shape[0]):] = 0.
        #val_fast_spectrum[:int(thr_frac*val_spectrum.shape[0])] = 0.

        val_slow_spectrum[np.abs(freq) > thr] = 0.
        val_fast_spectrum[np.abs(freq) < thr] = 0.

        val_trend = np.abs(np.fft.ifft(val_slow_spectrum))
        val_cycle = np.abs(np.fft.ifft(val_fast_spectrum))

        en_ap_sl = (np.abs(val_slow_spectrum)**2).sum()
        en_ap_fs = (np.abs(val_fast_spectrum)**2).sum()
        en_ap_tot = en_ap_sl + en_ap_fs
        en_frac = en_ap_sl/en_ap_tot
        print("Approximation of spectra energy totally: {0:.4e} ; low frequencies: {1:.4e} ; high frequencies : {2:.4e} ; and fraction of it for low frequencies: {3:.4e}"
              .format(en_ap_tot, en_ap_sl, en_ap_fs, en_frac))

        # DEBUGING part of block
        # why the ifft is sclaed down around the average?
        # what is the median of frequencies?
        #plt.plot(np.arange(values.shape[0]), val_trend)
        #plt.savefig('debug_fft_trend.png')
        #plt.close()
        #plt.loglog(freq, np.abs(val_slow_spectrum)**2), '.'
        #plt.savefig('debug_fft_spec_s.png')
        #plt.close()
        plt.loglog(freq, np.abs(val_spectrum)**2,'')
        plt.axvline(thr, alpha=0.5, color='r', linestyle='--')
        plt.savefig('debug_fft_spec.png')
        plt.close()
        #print('which frequencies have high contribution')
        #print(np.argwhere(val_spectrum>10000.))
      
    elif method == 'hpf':
        lam = 0.5*1e9
        valpd = pd.DataFrame(values, columns=["ti_transp_flux"])
        val_cycle, val_trend = sm.tsa.filters.hpfilter(valpd.ti_transp_flux, lam)

    elif method == 'exp':
        alpha = 0.005
        valpd = pd.DataFrame(values, columns=["ti_transp_flux"])
        #smoothing_model = ExponentialSmoothing(values).fit()
        smoothing_model = SimpleExpSmoothing(valpd, initialization_method="heuristic").fit(smoothing_level=alpha, optimized=False)
        val_trend = smoothing_model.fittedvalues
        val_cycle = valpd - val_trend
        print('smoothing of value: the found alpha= {}'.format(smoothing_model.model.params["smoothing_level"]))

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

    for i, mom in enumerate(moments[3:]):
        if abs(mom) > 1e-2*np.power(moments[0], i+2):
            print('moment num '+str(i+3)+' is large')    

    delta = (domain[-1] - domain[0])/len(domain)

    x = 1. # np.arange(len(pdf)) # np.linspace(valmin, valmax, valres)
    x = domain

    pref = 1./(np.sqrt(2.*np.pi)*moments[2])
    determ = 2.*moments[2]*moments[2]
    
    pdf_gauss = pref*np.exp((moments[1]-x)*(moments[1]-x)/determ)

    kl_div = delta*np.multiply(pdf, np.log(np.divide(pdf, pdf_gauss))).sum()

    return kl_div

###########################################

def main(foldername=False, runforbatch=False):

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
            if not foldername:
                mainfoldernum = 'mft'
                val_ev_s, file_names = profile_evol_load(prof_names=profiles, attrib_names=attributes, coord_len=8, folder_name=os.path.join(workdir, 'mft/cpo'), file_code_name=code_name, name_postfix='_mft')
            else:
                mainfoldernum = foldername
                val_ev_s, file_names = profile_evol_load(prof_names=profiles, attrib_names=attributes, folder_name=os.path.join(workdir, 'cpo'+mainfoldernum), file_code_name=code_name, name_postfix='_'+mainfoldernum)
        #print(len(val_ev_s[0])); #print(val_ev_s) ### DEBUG
        val_ev_s = []

        for i,(p,a) in enumerate(itertools.product(profiles, attributes)):

            val_ev_s.append(np.atleast_2d(np.genfromtxt(code_name+'_'+p+'_'+a+'_evol_'+mainfoldernum+'.csv', delimiter=", ")))     
            #print('val_ev_s[{}]).shape={}'.format(i, val_ev_s[i].shape)); #print(val_ev_s) ###DEBUG
            
            profile_evol_plot([val_ev_s[i]], name=code_name+'_'+p+'_'+a+'_'+mainfoldernum)
            
            #print('before shape {}'.format(val_ev_s[i].shape)) ## DEBUG
            #val = np.array(val_ev_s[i]).squeeze()
            #print('after shape {}'.format(val.shape)) ### DEBUG            

            ##ti_flux = np.genfromtxt('gem_ti_flux.csv', delimiter =", ")
            
            get_coreprof_ev_acf(val_ev_s[i], name=code_name+'_'+p+'_'+a+'stats', lags =[1,2,4,8,16,32,64,128,256,512])
            
            plot_coreprofval_dist(val_ev_s[i], name=p+'_'+a+'_'+mainfoldernum, discr_level=32)
           
            # TODO create '*all23.csv' file with new format!!!
 
            # try ARMA model
            """
            apply_arma(val)
            """
            # plot different averagings
            """         
            alpha_wind = 0.4
            val_wind = val[:-int(alpha_wind*len(val))]            
            val_trend_avg, val_fluct_avg = filter_trend(val_wind, "mean")
            
            #val_trend_hp, val_fluct_exp = filter_trend(val, "hpf")
            
            val_trend_fft, val_fluct_fft = filter_trend(val, "fft")

            val_trend_exp, val_fluct_exp = filter_trend(val, "exp")
           
            profile_evol_plot([val, val_trend_fft, val_trend_exp, val_trend_avg], # np.ones(val.shape)*val_trend_avg[0]], 
                              labels=['original', 'fft(f<2^-10)', 'exponential(alpha=0.005)', 'mean(of {:.2e} after {} steps)'.
                                                                                               format(val_trend_avg[0], int(alpha_wind*len(val)))],
                              name='trend_'+p+'_'+a+'_'+mainfoldernum)
            """
            # histogram for the last alpha_window values
            """
            plot_coreprofval_dist(val_fluct_avg, name='wind'+code_name+p+'_'+a+'_'+mainfoldernum, discr_level=128)
            """

            #TODO get exponential average of the values: standard packaged optimize for alpha -- why it is so high? is composition of exponential avaraging is another exponential averagin -- if so, what is alpha_comp?


if __name__ == '__main__':

    if len(sys.argv) == 2:
        main(foldername=sys.argv[1])
    elif len(sys.argv) == 3:
        main(foldername=sys.argv[1], runforbatch=int(sys.argv[2]))
    else:
        main()

