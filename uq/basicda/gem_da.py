import pandas as pd
import numpy as np
import matplotlib.pylab as plt
import os
import sys

import itertools

from sklearn.neighbors import KernelDensity as KDE
from scipy.stats import moment, linregress

import statsmodels.api as sm
from statsmodels.tsa.api import acf, pacf, graphics
from statsmodels.tsa.api import SimpleExpSmoothing, ExponentialSmoothing  
from statsmodels.graphics.tsaplots import plot_acf, plot_pacf
from statsmodels.graphics.api import qqplot
from statsmodels.tsa.ar_model import AutoReg
from statsmodels.tsa.arima.model import ARIMA, ARIMAResults

import easyvvuq as uq

#from da_utils import *
from ascii_cpo import read
sys.path.append('..')
import base

# imports (and set-up) for debugging
import pprint
from IPython.core import ultratb
sys.excepthook = ultratb.FormattedTB(mode='Verbose', color_scheme='Linux', call_pdb=False)


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

def profile_evol_load(rho=0.69, folder_name='../gem_data/cpo5/', prof_names=['ti_transp', 'te_transp'], attrib_names=['flux'], 
                      coord_len=1, var_num=1, file_code_name='gem', name_postfix=''):
    """
    Loads the quantitiy values from all CPO files with a specific type of name in the folder,
          then saves in a CSV file
    
        Parameters:
             rho (double): radial coordinate rho of flux tube modeled in the code
             folder_name: folder location for the CPO files
             prof_names (list): list of strings with names of profile names in a CPO
             attib_names (list): list of strings with names of parameters, 
                                 which should be an attribute of a profile 
             coord_len (int): number of flux tubes considered
             var_num (int): number of variables to read (? number of file)
             file_code (str): name of the code for which postprocessing is, same as prefic for CPO file names
             name_postfix (str): (? postfix for CPO file names)
        Returns:
             nested list of values: profiles&attributes -> flux-tubes -> values(time) 
    """ 

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
    
    #print('len of value_s and value_s[0]'); print(len(value_s)); ###DEBUG
    #print(len(value_s[0]))#print(value_s); print(coords); ### DEBUG

    # Iterate over passed list of file names
    for k, file_name in enumerate(file_names):
        
        coretransp = read(os.path.join(folder_name, file_name), 'coretransp')
        #print('coretransp'); print(coretransp) ### DEBUG
       
        # Iterate over speicified profiles in each CPO file
        for i, profname in enumerate(prof_names):
            #print('profname'); print(profname) ### DEBUG
            #print('values'); print(coretransp.values) ### DEBUG
            #print('values dict'); print(coretransp.values.__dict__) ### DEBUG          
            #print('values[0]'); print(coretransp.values[0]) ### DEBUG
            prof = getattr(coretransp.values[0], profname)

            # Iterate over each chosen attibute of each profile
            for j, attrib in enumerate(attrib_names):

                # Chose profiles for ions, it is possible to have many species, so profiles are always are list
                if profname[1] == 'i':
                     val_reading = getattr(prof, attrib)
                     #print('val_reading'); print(val_reading) ### DEBUG
 
                     # Iterate over flux-tubes at different rho coordinates
                     for coord in range(d):
                         #print('before v[a][c]'); print(coord); print(value_s[i*m+j][coord]); ###DEBUG
                         #print(val_reading[coord][0]) ### DEBUG
                         
                         #value_s[i*m+j][coord].append(val_reading[coord][0])
                         value_s[i*m+j][coord][k] = val_reading[coord][0]
                         #print('after v[a][c]'); print(value_s[i*m+j]) ### DEBUG

                # Choose profiles for electrons, it is always a single species, 
                # and one fewer level of list nestedness than for ions
                elif profname[1] == 'e':
                     val_reading = getattr(prof, attrib)

                     # Iterate over flux-tubes at different rho coordinates
                     for coord in range(d):
                         #print(coord) ###DEBUG
                         #value_s[i*m+j][coord].append(val_reading[coord])
                         value_s[i*m+j][coord][k] = val_reading[coord]
                else:
                     print('Error: Attributes have to belong either to ions or electrons')
                #print(len(value_s[i*m+j][0])) ### DEBUG   
    
    # Iterate over the carthesian product of all profiles and their attributes, for each save a csv with the value list
    for i,(prof,attrib) in enumerate(itertools.product(prof_names, attrib_names)):
        value_arr = np.array(value_s[i])
        
        np.savetxt(file_code_name + '_' + prof + '_' + attrib  + '_evol' + name_postfix +'.csv', 
                   value_s[i].T, delimiter =", ", fmt ='% s')
       
        if len(value_s.shape) == 3 and value_s.shape[2] > 0: # fallable, better need to check for emptines etc
            print('Last value (num. {1}) is: {0}'.format(value_s[i,:,-1], value_s.shape[2]))        

    #print('check size of read structure = {}'.format(value_s.shape)) ### DEBUG
    #return [value[0] for value in value_s], file_names
    return value_s, file_names

def profile_evol_plot(value_s, labels=['orig'], file_names=[], name='gem_ti_flux', alignment='end'):
    """
    Saves a PNG of a Matplotlib plot for a quantity agains index/time; 
          reads values from a list of numpy arrays passed
        Parameters:
            values_s (list): nested list of profile values (agains time) to be plotted
            labels (list): strings of labels for each values, should be the same length as inner list of value_s
            file_names: list of original CPO files, obsolete
            name (str): prefix of file names for the saved graphs
            alignment: where to add the graph and how to create the abcissa scale,
                        if 'end' consider that the  different lists last readings end at the same time, 
                        if 'start' - that their firs reading are alligned 
    """

    #ts = np.arange(len(file_names))
    n = max([v.shape[-1] for v in value_s])
    #print('n = {}, len(v) = {}, v[0].shape = {}'.format(n, len(value_s), value_s[0].shape)) ### DEBUG

    ## rhos = np.arange(len(value_s))
    #for num, value in enumerate(value_s):
    ##     ax.plot(rhos, value, label=num)
    #     value_s_pointwise.append(value)
    ## plt.savefig(name + '.png')
    ## plt.close()

    #print('value_s'); print(value_s) ### DEBUG

    # Define a set of styles for different lists plotted
    color_list = ['b', 'g', 'r', 'y']
    line_list = ['-', '--', '-.', ':']
    marker_list = ['.', 'o', 'v', '^']
    style_lists = [line_list, color_list] # NB!: can be modified to include marker style
    fmt_list = ["".join(map(str, style)) for style in itertools.product(*style_lists)]

    fig, ax = plt.subplots(figsize=(24.,8.))

    for inum, (value, lab) in enumerate(zip(value_s, labels)):
        #print('value.shape = {}'.format(value.shape)) ###DEBUG

        if alignment == 'end':
            ts = np.arange(n - value.shape[-1], n)
        elif alignment == 'start':
            ts = np.arange(value.shape[-1])
        else:
            ts = np.arange(value.shape[-1])

        for i in range(value.shape[0]):
             #print('value[{0},:]'.format(i)); print(value[i,:]) ### DEBUG
             ## !!! TODO temporary changes !!!
             #ax.semilogy(ts, value[i,:], '-', label=lab+'_'+str(i))
             ax.plot(ts, value[i,:], fmt_list[inum], label=lab)

    ax.legend(loc='lower center',
             #bbox_to_anchor=(0.5, 0.0), 
              ncol=int(np.sqrt(len(labels))),
              prop={'size':8})
   
    #plt.legend(loc='best')
    plt.savefig(name + '.png')
    plt.close()

    #np.savetxt(name + '.csv', np.squeeze(value_s, 0), delimiter =", ", fmt ='% s')

    return value_s

def plot_coreprofval_dist(value_spw, labels=[], name='ti', discr_level=64):
    """
    """
    #print('value_spw'); print(value_spw) ### DEBUG
     
    # Number of flux tube or case
    nftc = value_spw.shape[0]

    # Initialise lables if nothing was passed
    if len(labels)==0:
        labels = [n for n in range(nftc)]

    # Define the means of each value sequence
    val_means = []
    for i in range(nftc):
        val_means.append(value_spw[i, :].mean())

    # Define a set of styles for different lists plotted
    color_list = ['b', 'g', 'r', 'y']
    line_list = ['-', '--', '-.', ':']
    marker_list = ['.', 'o', 'v', '^']
    style_lists = [line_list, color_list] # NB!: can be modified to include marker style
    fmt_list = ["".join(map(str, style)) for style in itertools.product(*style_lists)]

    # Plot histograms of values for every flux tube ot case
    fig, ax = plt.subplots()
    for i in range(nftc):
        ax.hist(value_spw[i, :], bins=len(value_spw[i, :])//discr_level, label=labels[i])
    plt.savefig('hist_' + name + '.png')
    plt.close()

    # Compute and plot KDE fit
    x = np.linspace(0.9*value_spw.min(), 1.1*value_spw.max(), 100)[:, np.newaxis]
    fig, ax = plt.subplots()
    for i in range(nftc):
        kde = KDE(kernel='gaussian', bandwidth=(value_spw[i].max() - value_spw[i].min()) / discr_level). \
                  fit(value_spw[i, :, np.newaxis])
        log_pdf = kde.score_samples(x)
        log_pdf_orig = kde.score_samples(value_spw[i, :, np.newaxis])

        ax.plot(x[:, 0], np.exp(log_pdf), fmt_list[i], label='density of core transport values')
        
        # Add 'pluses' underneath the plot for each point in the fitted sample
        #ax.plot(value_spw[i], (-0.01*np.random.rand(log_pdf_orig.shape[0])) * np.exp(log_pdf_orig).min(), '+k')

        # Add vertical lines for mean of the each sample
        ax.axvline(x=val_means[i], ymin=0., ymax=1., linestyle=fmt_list[i][:-1], color=fmt_list[i][-1:]) 
        #TODO: change the style specification 
    
    plt.savefig('pdf_' + name + '.png')
    plt.close()

    # Print main moments
    mom_len = 5
    moments = np.zeros((nftc, mom_len))
    
    for i in range(nftc):
        
        print('Considering flux tube #{0}'.format(i))
        
        print('mean value is: ' + str(value_spw[i].mean()))
        #print(((value_spw*value_spw).mean() - value_spw.mean()*value_spw.mean())/1.)
    
        with open('stats_' + name + '.txt', 'w') as wfile:
            for n in range(mom_len):
                if n == 1:
                    m = value_spw[i].mean()
                else:
                    m = moment(value_spw[i], moment=n)
                moments[i,n] = m
                line = '{0}-th moment is: {1:.3e}'.format(n, m)
                #print(line)
                wfile.write(line+'\n')

    # Check the normality of distribution function
    kld = compare_gaussian(np.exp(log_pdf[0]), x, moments)
    print('KL-d is: ' + str(kld))

    return moments

def get_coreprof_ev_acf(value_ev, name='ti', lags=[1,2,3,4,5,6,7,8,9,10]):
    """
    Calculate autocorrelation function of the sequence of profile values
    """ 
    #acf = [1. if l==0 else np.corrcoef(value_ev[l:], value_ev[:-l])[0][1] for l in lags]
    
    nl = 64    
    
    nftc = value_ev.shape[0]    

    print('>Calculating ACF')
    for i in range(nftc):
        
        print('Considering flux tube or case #{}'.format(i))

        r,q,p  = acf(value_ev[i], nlags=nl, fft=True, qstat=True) 
 
        acf_data = np.c_[np.arange(1, nl+1), r[1:], q, p]
  
        acf_data_pd = pd.DataFrame(acf_data, columns=['lags', 'AC', 'Q', 'P(>Q)']) 
        acf_data_pd.set_index('lags')
 
        #acf_res = np.array(acf_res)
        #line = 'ACF :' + str(acf_res)
    
        print(acf_data_pd)

        #with open(name+'_acf.txt', 'w') as wfile:
        #    wfile.write(line)
    
        val_df = pd.DataFrame(value_ev[i])
   
        plot_acf(val_df, lags=lags)
        plt.savefig(str(i)+'acf.png')
        plt.close()
    """
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
    """
    Calculate autoregressive moving average model for profile values
    """
    
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

def filter_trend(values, method='hpf'):
    """
    Applies filter to split every time series into stationary and non-stationary part
    
   
        Parameters:
            values: sample of series values, preferably a list of numpy arrays
            method: algorithm of how to choose the trend
                    Options:
                        hpf - 
                        fft - 
                        exp - 
                        mean - 
                        linear_regression -
                    
        Returns:
            Two data structure of same dimension and type as input, one corresponds for trend, 
                another for fluctuations
        TODO: at the moment, back to return np-arrays
    """
 
    nftc = values.shape[0]

    # Find Fast Fourier Transform of the series 
    if method =='fft':
        thr = 2**(-10)
        thr_frac = 0.5
        
        val_trend = []
        val_cycle = []
             
        for i in range(nftc):

            val_spectrum = np.fft.fft(values[i])
            freq = np.fft.fftfreq(values.shape[-1], 1.)

            #print('v.shape = {}'.format(values.shape)) ### DEBUG
            #print('val_spectrum = {}'.format(val_spectrum)) ### DEBUG

            val_slow_spectrum = val_spectrum.copy()
            val_fast_spectrum = val_spectrum.copy()

            #val_slow_spectrum[int(thr_frac*val_spectrum.shape[0]):] = 0.
            #val_fast_spectrum[:int(thr_frac*val_spectrum.shape[0])] = 0.

            val_slow_spectrum[np.abs(freq) > thr] = 0.
            val_fast_spectrum[np.abs(freq) < thr] = 0.

            val_trend.append(np.abs(np.fft.ifft(val_slow_spectrum)))
            val_cycle.append(np.abs(np.fft.ifft(val_fast_spectrum)))

            en_ap_sl = (np.abs(val_slow_spectrum)**2).sum()
            en_ap_fs = (np.abs(val_fast_spectrum)**2).sum()
            en_ap_tot = en_ap_sl + en_ap_fs
            en_frac = en_ap_sl/en_ap_tot
            print('''Approximation of spectra energy totally: {0:.4e} ; 
                   low frequencies: {1:.4e} ; high frequencies : {2:.4e} ;
                   and fraction of it for low frequencies: {3:.4e}'''.
                       format(en_ap_tot, en_ap_sl, en_ap_fs, en_frac))

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
            plt.savefig('fft_spec'+str(i)+'.png')
            plt.close()
        #print('which frequencies have high contribution')
        #print(np.argwhere(val_spectrum>10000.))
      
    elif method == 'hpf':
        lam = 0.5*1e9
        valpd = pd.DataFrame(values, columns=["ti_transp_flux"])
        val_cycle, val_trend = sm.tsa.filters.hpfilter(valpd.ti_transp_flux, lam)

    # Apply exponential averaging for the series
    elif method == 'exp':
        alpha = 0.005
 
        val_trend = []
        val_cycle = []

        for i in range(nftc):
 
            valpd = pd.DataFrame(values[i], columns=["ti_transp_flux"])
            #smoothing_model = ExponentialSmoothing(values).fit()
            smoothing_model = SimpleExpSmoothing(valpd, initialization_method="heuristic"). \
                                  fit(smoothing_level=alpha, optimized=False)
            val_trend.append(smoothing_model.fittedvalues)
            val_cycle.append(valpd - val_trend[i])
            print('smoothing of value: the found alpha= {}'.
                      format(smoothing_model.model.params["smoothing_level"]))

    # Find the mean of the sample Q_bar, return all means as trend and Q-Q_bar as deviation
    elif method == 'mean':
        val_trend = []
        val_cycle = []

        for i in range(nftc):
            val_trend.append(values[i].mean()*np.ones(values[i].shape))
            #val_cycle.append(values[i] - val_trend[i])   
            #TODO: mind the change: the only filtering method that does not return difference for 'cycle' part
            val_cycle.append(values[i].std()*np.ones(values[i].shape))

    # Find Linear Regression parameters (a,b) for Q=a*x+b
    elif method == 'linear_regression':
        # Options to choose sample for regresion
        # 1) All readings of the original sample: in normal equations matrix could be too large
        # 2) Choose samples for every window: window length to be defined from ACF
        # Options for algorithm:
        # 1) Normal equations for LSE
        # 2) Gradient Descent for SE
        
        val_trend = []
        val_cycle = []

        for i in range(nftc):
            #print('>array size of single sample: {}'.format(values[i].shape)) ###DEBUG
            
            n = values[i].shape[0]		   
 
            # (Possible) sample thinning (reduction/subsampling)
            delta = 1
            #wind_int = delta * acl
            #wind_int = 10
            #values = values[::wind_int]       
 
            #x_c = np.linspace(1, n, n).reshape(n, 1)
            x_c = np.arange(n).reshape(n, 1)
            i_c = np.ones_like(x_c)
            x_c_lr = np.append(x_c, i_c, axis=1)
            y_c_lr = np.array(values[i]).reshape(n, 1)
            
            """
            print('''
                  >size of sample matrix for LR: {} 
                           values[i]: {}
                           x_c: {}  
                           i_c: {} 
                           y vector: {}  
                  '''. format(x_c_lr.shape, values[i].shape, x_c.shape, i_c.shape, y_c_lr.shape)) ###DEBUG       
            """
 
            # Find LR parameters: for y=theta.T*X
            # (Currently considering 1D series separately)
            
            theta = np.dot( np.linalg.inv(np.dot(x_c_lr.T, x_c_lr)) , np.dot(x_c_lr.T, y_c_lr) )
            
            # original sample too large for this? - measure time or have an addtional sample length check
            b = theta[0]
            a = theta[1:]

            #print('Normal equation produces b = {0} ; a = {1}'.format(b, a)) ### DEBUG

            # Compare with (at least one) ready LR method from a package
            # This one may use iterative methods for solution, probably some options to regularize 
            res_lr_sp = linregress(x_c.reshape(-1), y_c_lr.reshape(-1))
            a_sp = res_lr_sp.slope
            b_sp = res_lr_sp.intercept
            #print(a_sp, b_sp) ###DEBUG
                
            # Find points (x,y) exactly satisfying y=ax+b; could also be done with a method from an LR package
            val_trend.append(np.dot(x_c_lr, theta))
            #val_trend.append(np.dot(a, x_c) + np.dot(b, i_c))
        
            val_cycle.append(values[i] - val_trend[i])
        
            # TODO Move check for approximate stationarity to a different function
            alpha_tol = 5e-2
            a_tol = alpha_tol * (np.max(y_c_lr) - np.min(y_c_lr)) / float(n)
            
            #print('NE give array shapes: {}'.format(val_trend[0].shape)) ###DEBUG 
            print('''Linear Regression: for case #{0} the slope parameter value is {1:.3f} 
                                        and it is {2} than {3:.3f} threshold'''.
                      format(i, a[0][0], 'SMALLER' if a[0][0] < a_tol else 'LARGER', a_tol))

    else:
        print('>Error in filtering: no such method')
   
    return np.array(val_trend), np.array(val_cycle)

def compare_gaussian(pdf, domain, moments):
    """
    Takes pdf as f(x) e [0,1], x e {range of quantity values}
    1) Checks if central moments of order > 2 are neglegible
    2) Calculates KL of given pdf and pdf of a Gaussian with with given first two moments, E and sigma
    """
    
    nftc = moments.shape[0]
     
    delta = (domain[-1] - domain[0])/len(domain)
    #x = 1. # np.arange(len(pdf)) # np.linspace(valmin, valmax, valres)
    x = domain

    kl_div = []

    print('> Performing comparison of KDE with a Gaussian of same mean and standard deviation')
    for j in range(nftc):
        print('Considering flux tube or case #{}'.format(j))
        for i, mom in enumerate(moments[j,3:]):
            if abs(mom) > 1e-2*np.power(moments[j,0], i+2):
                print('moment num '+str(i+3)+' is large')    

        pref = 1./(np.sqrt(2.*np.pi)*moments[j,2])
        determ = 2.*moments[j,2]*moments[j,2]
    
        pdf_gauss = pref*np.exp((moments[j,1]-x)*(moments[j,1]-x)/determ)

        kl_div.append(delta*np.multiply(pdf, np.log(np.divide(pdf, pdf_gauss))).sum())

    return kl_div

def read_run_uq(db_path):
    """
    Reads information on code runs that was recorded into an EasyVVUQ campaign DB
         Parameters:
             db_path: a full path to location of a DB inside an UQ campiang directory
         Returns:
             a list with dictionaries of run input values (consider keeping explicit run IDs)
             a list of stings with keys/names of input values 
   """
    
    my_campaign = uq.Campaign(name="campaign_1", db_location=db_path)

    runs = my_campaign.campaign_db.runs()
    input_values = [r[1]['params'] for r in runs]

    run1 = my_campaign.campaign_db.run("run_1")
    input_names = list(run1['params'].keys())
    print(">Names of params: ".format(input_names))

    return input_values, input_names

def plot_response_cuts(data, input_names, output_names):
    """
    Plot different cuts (fixing all input parameter values but one) of code response for given QoI
        Parameters:
            data: pandas dataframe with input and output values of code
                  note: pass means of the outputs
            input_names: list of stings with input names, corresponds to dataframe columns, 
                         also used for plot labels
            output_names: list of strings with output names, corresponds to dataframe columns,
                          also used for plot labels
    """
    
    n_inputs = len(input_names)
    n_points = len(data.index)
    n_points_perdim = int(n_points ** (1./n_inputs))
    n_fixvals = n_points_perdim ** (n_inputs - 1)
    n_plots = (n_inputs) * (n_fixvals)

    qoi_name = output_names[0] # TODO to make modifyable
    qoi_std_name = qoi_name + '_std'
    qoi_stem_name= qoi_name + '_stem'

    #print([n_inputs, n_points, n_points_perdim, n_fixvals, n_plots, qoi_name]) ###DEBUG
    
    # unique values for every input dimension
    input_vals_unique = np.array([np.unique(data[i]) for i in input_names])
    #print(input_vals_unique) ###DEBUG
 
    # a geneartor for numbers for input dimension numbers
    input_inds = range(n_inputs)

    fig, ax = plt.subplots(n_inputs, n_fixvals, 
                           figsize=(40, 20)
                          )

    # iterate over all the input dimensions, selecting single one as a running variable
    for i_ip in range(n_inputs):

        running_ip_name = input_names[i_ip]
        input_names_left = [n for n in input_names if n != running_ip_name]
        input_inds_left = [n for n in input_inds if n != i_ip]
        fixed_ip_names = ''.join([n+'&' for n in input_names_left])

        offset = 1

        input_fixvals_unique = list(itertools.product(
                                    *[input_vals_unique[i,:].tolist() for i in input_inds_left]))
        
        #print([i_ip, running_ip_name, input_names_left, 
        #      input_inds_left, fixed_ip_names, input_fixvals_unique]) ###DEBUG

        # Iterate over the all combinations of input parameters values to be kept const fro a cut
        for j_fixval in range(n_fixvals):
            #print([i_ip, j_fixval]) ###DEBUG
       
            #pivot_fixed_val = data[input_names_left][j_fixval+offset]
            #tot_ind = data[input_names_left].where() #TODO location where element is equal to pivot_fixed_val

            #tot_ind = np.arange(j_fixval*offset,
            #                    (j_fixval+n_points_perdim-1)*offset,
            #                    offset).tolist()
            # TODO how to save symbolic expression for an array cut?
            
            input_fixvals = input_fixvals_unique[j_fixval]
            fixval_query = ''.join([n+'=='+str(v)+' & ' for (n,v) in zip(input_names_left, input_fixvals)])[:-3]
            # TODO query using abs(x-x*)<e_tol           
 
            #print([tot_ind, input_fixvals, fixval_query]) ###DEBUG            

            data_slice = data.query(fixval_query) 
            #TODO: probably a very slow way to access a DataFrame - still think how to offset with 3-nested loop

            # TODO: choose one of three ways: 
            #                      1) offsets - has to be 3 nested loops
            #                      2) filtering - inside current tow loops, use np.unique() -> using
            #                      3) transform into ndarray of 4D, the iterate (look tuto notebook)
 
            #fixed_ip_vals      = data[input_names_left][tot_ind[0]] # TODO count tot_ind
            #fixed_ip_vals_str  = ''.join([str(v)+';' for v in fixed_ip_vals.to_list()]) 
            fixed_ip_val_str = ''.join([n+'='+str(round(v, 1))+';' for (n,v) in zip(input_names_left, input_fixvals)])

            x_io = data_slice[running_ip_name].to_numpy()
            y_qoi = data_slice[qoi_name].to_numpy()
            y_std = data_slice[qoi_std_name].to_numpy()
            y_stem= data_slice[qoi_stem_name].to_numpy()
 
            #ax[i_ip][j_fixval].plot(x_io, y_qoi, 'o-', 
            #                   label='Response for ({})->({}) for {}'.
            #                   format(running_ip_name, qoi_name, fixed_ip_val_str))         
             
            ax[i_ip][j_fixval].errorbar(x_io, y_qoi, yerr=y_stem,
                                        fmt='-o', uplims=False, lolims=False,
                                        label='Response for ({})->({}) for {}'.
                                        format(running_ip_name, qoi_name, fixed_ip_val_str))
       
            ax[i_ip][j_fixval].set_xlabel(r'{}'.format(running_ip_name))
            ax[i_ip][j_fixval].set_ylabel(r'{}'.format(qoi_name))
            ax[i_ip][j_fixval].set_title(r'{}'.format(fixed_ip_val_str), fontsize=9)
      
            ax[i_ip][j_fixval].set_ylim(1.5E+6, 3.8E+6) 
            #ax[i_ip][j_fixval].legend(loc='best') 
     
        offset *= 2
        # Filter on dataframe could be completely replaces by an offseting for different chosen parameter        
 
    fig.tight_layout()
    #fig.suptitle('GEM response in {} around profile values'.format(qoi_name)) #TODO: pass names as arguments    
    #fig.subplots_adjust(top=0.8)

    plt.savefig('scan_{}.png'.format(qoi_name))
    plt.close()

###########################################

def main(foldername=False, runforbatch=False, coordnum=1, runnum=1, mainfoldernum='false'):
    """
    Parameters:
      foldername: relative path to folder where to read cpo files from
      runforbatch: if False then first read values from cpo files, if True then look for a csv file
      coordnum: number of coordinate values (flux tubes) to consider
      runnum: number of different variants of run (e.g. profile shapes) to consider
      manfoldernum: for naming, if false then use cpo folder
    """

    if mainfoldernum == 'false':
        mainfoldernum = foldername # rather bad, fails if foldername is composed

    #workdir = os.path.join(os.getenv('SCRATCH'), 'MFW_runs')
    #workdir = os.path.join(os.getenv('SCRATCH'), 'VARY_1FT_GEM_NT_test')
    workdir = os.path.join(os.getenv('SCRATCH'),)

    runnum_list = [r+1 for r in range(runnum)]

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

        # 1) If runforbatch, then csv are already in the folder, otherwise have to read CPO-s
        if not runforbatch:
            # If asserets, then alway specify folders with original cpo files
            # Here are the possibilities where to look for the cpo containing folder:
            # mainfoldernum = 'mft4' # old default
            # 'mft/run4/cpo' # one option
            # 'cpo'+mainfoldernum # other option, if mainfoldernum=='20' etc.

            #mainfoldernum = foldername

            # Get list of directories for runs of profile variation; read them separetely in a loop 
            # (now there are different number of iterations), then load and pass to a new 
            # (TODO) plotting function with chosen profile/attribute/coordinate/etc 
            
            # 2) Iterate over all runs, meaning for same scenario but with a different transport profile variation
            for runn in runnum_list: 
                
                folder_name_curr = os.path.join(workdir, foldername+'/run_'+str(runn)) 
                # TODO make more flexible for different existing cases?
                
                print('Going over CPO-s in the folder: {}'.format(folder_name_curr))
                
                val_ev_s, file_names = profile_evol_load(prof_names=profiles, attrib_names=attributes, coord_len=coordnum, 
                                                         folder_name=folder_name_curr, 
                                                         file_code_name=code_name, 
                                                         name_postfix='_'+mainfoldernum+'_'+str(runn))
               
                #val_ev_s, file_names = profile_evol_load(prof_names=profiles, attrib_names=attributes, coord_len=coordnum, 
                #                                         folder_name=os.path.join(workdir, 'cpo'+mainfoldernum), 
                #                                         file_code_name=code_name, 
                #                                         name_postfix='_'+mainfoldernum       

        # 3) Getting the input profiles values, primarily for the plot labels
        db_id = 10002794
        camp_id = 'moj202gj'
        mmiter_num = 6 
        file_runs_db = "../gem_notransp_db_"+str(db_id)+".json" 
        #TODO: take the internal campaign folder ID as input, and then load the SLURM id -> 
        # -> probably for that it is better to import EasyVVUQ and intilalise the campaign
        # with the location of DB, 
        # then use the [my_]campaign.campaign_db.[func-s]() to read stuff about runs
        
        #runs_db_loc = "sqlite:///" + foldername + "/.."*7 + "/campaign.db"
        runs_db_loc = "sqlite:///" + "campaign_" + camp_id + "_" + str(mmiter_num) + ".db"
        runs_input_vals, runs_input_names = read_run_uq(runs_db_loc) # test function, at least manually    
           
        # 3') By default, create new list for readings and read them from file, 
        #     even if they are in programm memory already      
        val_ev_s = []

        # 4) Iterate over cartesian product of all profiles and their attributes
        for i,(p,a) in enumerate(itertools.product(profiles, attributes)):

            # 4.0) Assuming there are multiple ensembles of runs for this submission 
            #     (corresponding to a global iteration of UQ campaigns) read all the 'runs' of an ensemble 
            #     in a single list of arrays
            
            #csv_file_name = code_name+'_'+p+'_'+a+'_evol_'+mainfoldernum+'.csv'
            for runn in runnum_list:
                csv_file_name = code_name + '_' + p + '_' + a + '_evol_' + mainfoldernum + '_' + str(runn) + '.csv'
                val_ev_s.append(np.atleast_2d(np.genfromtxt(csv_file_name, delimiter=", ").T))     
            #print('val_ev_s[{}]).shape={}'.format(i, val_ev_s[i].shape)); #print(val_ev_s) ###DEBUG
            
            # 4.1) Plot the read values, pass a list of array
            
            #profile_evol_plot([val_ev_s[i]], name=code_name+'_'+p+'_'+a+'_'+mainfoldernum)
            # modification: list of arrays is for different profile variations
            labels = [str(r) for r in runnum_list]
            labels = ["".join([rin+'='+str(round(r[rin], 1))+"; " for rin in runs_input_names]) for r in runs_input_vals]
             
            profile_evol_plot(val_ev_s, labels=labels, name=code_name+'_'+p+'_'+a+'_'+mainfoldernum, alignment='start') 
            
            #print('passes to plot: {}'.format(val_ev_s[0].shape)) ###DEBUG
            #print('before shape {}'.format(val_ev_s[i].shape)) ###DEBUG
            #val = np.array(val_ev_s[i]).squeeze()
            #print('after shape {}'.format(val.shape)) ### DEBUG            
            #print('total number of different cases {}'.format(len(val_ev_s))) ###DEBUG
 
            ##ti_flux = np.genfromtxt('gem_ti_flux.csv', delimiter =", ")

            # 4.1') Define the window to discard intial ramp-up and overshooting phase
            alpha_wind = 0.3 # how much to discard
            val = val_ev_s[i] # singel series for a profile+attribute, shouldn't be used for now...
            #TODO: General series list now iterates with both prof+att and run case!

            val_wind_s = [val[:,int(alpha_wind*val.shape[-1]):] for val in val_ev_s]
            print('sizes before and after windowing: {} and {} '.format(val_ev_s[0].shape, val_wind_s[0].shape)) ###DEBUG
            #print('val_wind len and element shape are {} and {}'.format(len(val_wind), val_wind[0].shape)) ### DEBUG
   
            # 4.2) Calculate ACF for the values
            lags_list = [1,4,16,64,256,256,1024,2048,4096]            
            lags_list = [l for l in lags_list if l < val_wind_s[i].shape[-1]]
            
            #get_coreprof_ev_acf(val_ev_s[i], name=code_name+'_'+p+'_'+a+'stats'+'_'+str(runn), lags=lags_list)
            for runn in range(len(runnum_list)):
                """
                print('ACF for case #{0}'.format(runn))
                 
                get_coreprof_ev_acf(val_ev_s[runn], 
                                    name=code_name+'_'+p+'_'+a+'stats'+'_'+str(runn), 
                                    lags=lags_list) 
                #NB!: uncertainty of the ACF computation ~ Var(X)/sqrt(n) , where n=N_samples/N_lags
                """

            # 4.3) Plotting histograms and KDEs of the profile values evolution
            #plot_coreprofval_dist(val_ev_s[i], name=p+'_'+a+'_'+mainfoldernum, discr_level=32)
            for runn in range(len(runnum_list)):
                """
                print('KDE for case #{0}'.format(runn)) 
                         
                plot_coreprofval_dist(val_wind_s[runn],
                                      name=p+'_'+a+'_'+str(runn)+'_'+mainfoldernum, discr_level=32)
                """
                #TODO: pass number of case to save different files

            # 4.3.1) Plotting single plot with histograms, KDEs and distribution means
             
            plot_coreprofval_dist(np.vstack([np.squeeze(v, 0) for v in val_wind_s]), labels=labels, 
                                  name='tot_'+p+'_'+a+'_'+mainfoldernum, discr_level=32)
            

            # 4.4) Apply ARMA model
            """
            apply_arma(val)
            """

            # 4.5)  Apply different averaging methods and plot the results
            
            # 4.5.1) Calculating the mean of last *alpha* reads
            
            #print('input names : {}'.format(runs_input_names)) ###DEBUG
            #print('inputs values : {}'.format(runs_input_vals)) ###DEBUG            

            runs_input_names_new = [n.replace('.', '_') for n in runs_input_names]
            stats_df = pd.DataFrame(columns=['name', 'mean', 'std'])
            scan_df = pd.DataFrame(columns=['name'] 
                                         + runs_input_names_new
                                         + [p+'_'+a, p+'_'+a+'_std', p+'_'+a+'_stem'])

            val_trend_avg_s = []
            val_std_s = []

            for runn in runnum_list:

                val_trend_avg, val_fluct_avg = filter_trend(val_wind_s[runn-1], "mean")
                val_trend_avg_s.append(val_trend_avg)
                val_std_s.append(val_fluct_avg) 
                #TODO: look up Python ways to process tuple elements differently

                n_lensample = val_trend_avg.shape[-1]
               
                stats_df = stats_df.append(pd.Series(
                               data={'mean': val_trend_avg_s[runn-1][0][0],
                                     'std': val_std_s[runn-1][0][0]},
                               name=str(runn-1))) # probably a bad workaround
                
                scan_data = runs_input_vals[runn-1]
                scan_data[p+'_'+a] = val_trend_avg_s[runn-1][0][0]
                scan_data[p+'_'+a+'_std'] = val_std_s[runn-1][0][0]
                scan_data[p+'_'+a+'_stem'] = scan_data[p+'_'+a+'_std'] / float(n_lensample)

                scan_data_new = {}
                for k,v in scan_data.items():
                    scan_data_new[k.replace('.', '_')] = v
                
                scan_df = scan_df.append(pd.Series(
                              data=scan_data_new,
                              name=str(runn-1)))
            
            profile_evol_plot(val_trend_avg_s, labels=labels, name='means_'+p+'_'+a+'_'+mainfoldernum)
            stats_df.to_csv('stats_main_'+p+'_'+a+'_'+mainfoldernum+'.csv') 
            scan_df.to_csv('resuq_main_'+p+'_'+a+'_'+mainfoldernum+'.csv')    
            
            # 4.5.1'') Plot parameter dependency for single parameters
            print('plotting cuts starting')
            plot_response_cuts(scan_df, runs_input_names_new, [p+'_'+a])
            print('plotting cuts done')

            # 4.5.2) Calcualting linear regression against time fit for the last window:
            # Apply LR to get form of Q=a*t+b
            val_trend_lr_s = []
            for runn in runnum_list:
                """
                val_trend_lr, val_residue_lr = filter_trend(val_wind_s[runn-1], "linear_regression")
                val_trend_lr_s.append(val_trend_lr.reshape(val_wind_s[runn-1].shape)) # bad workaround
                
                #print('>shapes passed to plot: {} and {}'.
                #       format(val_wind_s[runn-1].shape, val_trend_lr_s[runn-1].shape)) ###DEBUG
                
                profile_evol_plot([val_wind_s[runn-1], val_trend_lr_s[runn-1]],
                                  labels=['original', 'linear regression with NE'],
                                  name='lr_'+p+'_'+a+'_'+str(runn-1)+'_'+mainfoldernum) 
                """

            # 4.5.3) Applying HP-filter    
            val_trend_hpf_s = []
            for runn in runnum_list:
                """
                val_trend_hpf, val_fluct_hpf = filter_trend(val, "hpf")
                val_trend_hpf_s.append(val_trend_hpf)
                """
         
            # 4.5.4) Applying Fast Fourier Transform
            val_trend_fft_s = []
            for runn in runnum_list:
                """
                val_trend_fft, val_fluct_fft = filter_trend(val_wind_s[runn-1], "fft")
                val_trend_fft_s.append(val_trend_fft)            
                """
                #TODO: pass number of case to save different files

            # 4.5.5) Applying Exponential Averaging:
            val_trend_exp_s = []
            for runn in runnum_list:
                #TODO: introduce 0-padding as an option to avoid lag effect of exp-avg
                """
                val_trend_exp, val_fluct_exp = filter_trend(val_wind_s[runn-1], "exp")
                val_trend_exp_s.append(val_trend_exp)
                """

            ##!!! TODO temorarily changes!!! --- mind what is plotted from continiuous_gem.sh           
            """
            profile_evol_plot([val, val_trend_fft, val_trend_exp, val_trend_avg], 
                            # np.ones(val.shape)*val_trend_avg[0]], 
                              labels=['original', 'fft(f<2^-10)', 'exponential(alpha=0.005)', 
                              'mean(of {:.2e} after {} steps)'.format(val_trend_avg[0,0], 
                                    int(alpha_wind*val.shape[-1]))],
                              name='trend_'+p+'_'+a+'_'+mainfoldernum)
            """

            #profile_evol_plot([val, val_trend_exp, val_trend_avg], # np.ones(val.shape)*val_trend_avg[0]], 
            #                  labels=['original', 'exponential(alpha=0.005)', 
            #                  'mean(of {:.2e} after {} steps)'.format(val_trend_avg[0,0], 
            #                        int(alpha_wind*val.shape[-1]))],
            #                  name='trend_'+p+'_'+a+'_'+mainfoldernum)
           
            # 4.6) Histogram for the last alpha_window values
            """
            plot_coreprofval_dist(val_fluct_avg, name='wind'+code_name+p+'_'+a+'_'+mainfoldernum, discr_level=128)
            """

            #TODO get exponential average of the values: standard packaged optimize for alpha -
            # - why it is so high? is composition of exponential avaraging is another exponential averagin -
            # - if so, what is alpha_comp?
            #TODO exponential averaging with a symmetric window -> apply padding

if __name__ == '__main__':

    if len(sys.argv) == 2:
        # Run main with default values for all files in the folder
        main(foldername=sys.argv[1])
    elif len(sys.argv) == 3:
        # Run main with default valuer for all files in the folder, but considering there might be a csv composed already
        main(foldername=sys.argv[1], runforbatch=int(sys.argv[2]))
    elif len(sys.argv) == 4:
        # Run main for all files in the folder, either read values from csv (runfirbatch), and for  multiple flux tubes (coord)
        main(foldername=sys.argv[1], runforbatch=int(sys.argv[2]), coordnum=int(sys.argv[3]))
    elif len(sys.argv) == 5:
        # Run main for all files in the folder, either read values from csv (runfirbatch), 
        # for possible multiple flux tubes (coord), and specifying how to save files
        main(foldername=sys.argv[1], runforbatch=int(sys.argv[2]), coordnum=int(sys.argv[3]), mainfoldernum=sys.argv[4])
    elif len(sys.argv) == 6:
        # Run main for all files in folder, possibly reading values from csv, possibly for multiple flux tubes, 
        # and for possibly many cases (e.g. different input profiles) + specify save folder
        main(foldername=sys.argv[1], runforbatch=int(sys.argv[2]), coordnum=int(sys.argv[3]), 
             runnum=int(sys.argv[4]), mainfoldernum=sys.argv[5])
    else:
        # Run main with all defaults
        main()

