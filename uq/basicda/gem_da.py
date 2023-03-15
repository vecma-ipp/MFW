"""
Provides functions to work on CPO outputs of turbulence code GEM
and a pipeline of post-processing
"""
import pandas as pd
import numpy as np
import matplotlib.pylab as plt
import os
import sys

import itertools
import time

from sklearn.neighbors import KernelDensity as KDE
from scipy.stats import moment, linregress

import statsmodels.api as sm
from statsmodels.tsa.api import SimpleExpSmoothing, ExponentialSmoothing  
from statsmodels.graphics.api import qqplot
from statsmodels.tsa.ar_model import AutoReg
from statsmodels.tsa.arima.model import ARIMA, ARIMAResults

import easyvvuq as uq
import easyvvuq.db.sql as db

from da_utils import walklevel, get_coreprof_ev_acf, produce_stats_dataframes, plot_response_cuts, plot_timetraces_act, time_traces_per_run

from ascii_cpo import read
sys.path.append('..')
import base

# Imports (and set-up) for debugging
#import matplotlib.font_manager as font_manager
import pprint
from IPython.core import ultratb
sys.excepthook = ultratb.FormattedTB(mode='Verbose', color_scheme='Linux', call_pdb=False)

###################################################
######### FUNCTIONS DEFINITIONS ###################
###################################################

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
    # Function to analyses the SVD decomposition of the Sobol indices matrix and find largest eigenvalues among
    # linear combinations of Sobol indices of different order.
    # Further should use the eigenvectors as "directions" for new resampling for UQ quadratures

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

def profile_evol_load(rho=0.7, folder_name='../gem_data/cpo5/', prof_names=['ti_transp', 'te_transp'], attrib_names=['flux'], 
                      coord_len=1, var_num=1, file_code_name='gem', name_postfix=''):
    """
    Loads the quantity values from all CPO files with a specific type of name in the folder,
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
    file_base_intermediate = 'coretransp'
    file_base_tocheck = file_code_name + '_' + file_base_intermediate

    file_ext = '.cpo'

    file_names = [f for f in os.listdir(folder_name) if 
                             os.path.isfile(os.path.join(folder_name, f)) and 
                             f.endswith(file_ext) and
                             f.startswith(file_base_tocheck)
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

        #print('Reading from {0}'.format(file_name)) ###DEBUG
        
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
                #   and one fewer level of list nestedness than for ions
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
        #value_arr = np.array(value_s[i])
        
        np.savetxt(file_code_name + '_' + prof + '_' + attrib  + '_evol' + name_postfix +'.csv', 
                   value_s[i].T, delimiter =", ", fmt ='% s')
       
        if len(value_s.shape) == 3 and value_s.shape[2] > 0: # fallable, better need to check for emptines etc
            print('Last value (num. {1}) is: {0}'.format(value_s[i,:,-1], value_s.shape[2]))        

    #print('check size of read structure = {}'.format(value_s.shape)) ### DEBUG
    #return [value[0] for value in value_s], file_names
    return value_s, file_names

def profile_evol_plot(value_s, labels=['orig'], file_names=[], name='gem_ti_flux', alignment='end', vertline=False):
    """
    Saves a PNG of a Matplotlib plot for a quantity against index/time; 
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
    color_list = ['b', 'g', 'r', 'y' , 'm', 'c', 'k']
    line_list = ['-', '--', '-.', ':']
    marker_list = ['', '.', 'o', 'v', '^', '<', '>']
    style_lists = [marker_list, line_list, color_list, ] # NB!: can be modified to include marker style
    #fmt_list = ["".join(map(str, style)) for style in itertools.product(*style_lists)]
    fmt_list = [style for style in itertools.product(*style_lists)]

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

             #ax.semilogy(ts, value[i,:], '-', label=lab+'_'+str(i))
             
             #ax.plot(ts, value[i,:], fmt_list[inum], label=lab)
             ax.plot(ts, value[i,:], color=fmt_list[inum][2], linestyle=fmt_list[inum][1], marker=fmt_list[inum][0], label=lab)

    ax.legend(loc='lower center',
             #bbox_to_anchor=(0.5, 0.0), 
              ncol=int(np.sqrt(len(labels))) if len(labels) < 25 else 4,
              prop={'size' : 10})

    ax.set_ylim(-5.E+5, 4.5E+6) #TODO either delete or make modifiable

    if vertline:
        ax.axvline(vertline, alpha=0.3, color='k', linestyle='--', label='left border of the window')
   
    plt.savefig(name + '.svg', dpi=1250)
    plt.close()

    #np.savetxt(name + '.csv', np.squeeze(value_s, 0), delimiter =", ", fmt ='% s')

    return value_s

def plot_coreprofval_dist(value_spw, labels=[], name='ti', discr_level=64, forplot=False):
    """
    """
    time_loc = time.time()
    #print('value_spw'); print(value_spw) ### DEBUG
     
    # Number of flux tube or case
    if isinstance(value_spw, np.ndarray):
        nftc = value_spw.shape[0]
    elif isinstance(value_spw, list):
        nftc = len(value_spw)
    else:
        nftc = 1

    # Initialise lables if nothing was passed
    if len(labels)==0:
        labels = [n for n in range(nftc)]

    # Define the means of each value sequence
    val_means = []
    for i in range(nftc):
        val_means.append(value_spw[i][:].mean())

    # Define a set of styles for different lists plotted
    color_list = ['b', 'g', 'r', 'y' , 'm', 'c', 'k']
    line_list = ['-', '--', '-.', ':']
    marker_list = ['', '.', 'o', 'v', '^', '<', '>']
    style_lists = [marker_list, line_list, color_list,] # NB!: can be modified to include marker style
    #fmt_list = ["".join(map(str, style)) for style in itertools.product(*style_lists)]
    fmt_list = [style for style in itertools.product(*style_lists)]

    # Plot histograms of values for every flux tube ot case
    fig, ax = plt.subplots()

    print('--time in distribution plotting: before histograms: {0}'.format(time.time()-time_loc))
    time_loc = time.time()
    for i in range(nftc):

        n = len(value_spw[i][:])
        ax.hist(value_spw[i][:], bins=n // discr_level, label=labels[i], alpha=0.7)

    plt.legend(loc='best' if len(labels) < 16 else (-0.2, -0.2),
               #ncol=int(np.sqrt(len(labels))) if len(labels) < 25 else 4,
               ncol=1,
               )
    plt.savefig('hist_' + name + '.svg', dpi=1200)
    plt.close()

    print('--time in distribution plotting: histograms before KDE: {0}'.format(time.time()-time_loc))
    
    # Compute and plot KDE fit
    """
    n_kde_binning = 256
    x_min = min([min(value_spw[i]) for i in range(nftc)])
    x_max = max([max(value_spw[i]) for i in range(nftc)])
    x = np.linspace(0.9*x_min, 1.1*x_max, n_kde_binning)[:, np.newaxis]

    fig, ax = plt.subplots()

    for i in range(nftc):

        time_loc = time.time()    

        kde = KDE(kernel='gaussian', bandwidth=(max(value_spw[i]) - min(value_spw[i])) / discr_level). \
                  fit(value_spw[i][:, np.newaxis])
        log_pdf = kde.score_samples(x)
        log_pdf_orig = kde.score_samples(value_spw[i][:, np.newaxis])

        ax.plot(x[:, 0], np.exp(log_pdf), color=fmt_list[i][2], linestyle=fmt_list[i][1], marker=fmt_list[i][0], label=labels[i]) #label='density of core transport values'
        
        # Add 'pluses' underneath the plot for each point in the fitted sample
        #ax.plot(value_spw[i], (-0.01*np.random.rand(log_pdf_orig.shape[0])) * np.exp(log_pdf_orig).min(), '+k')

        # Add vertical lines for mean of the each sample
        #ax.axvline(x=val_means[i], ymin=0., ymax=1., linestyle=fmt_list[i][:-2], color=fmt_list[i][-2], marker=fmt_list[i][-1]) # now fmt_list numbering is wrong
        ax.axvline(x=val_means[i], ymin=0., ymax=np.exp(log_pdf).max(), color=fmt_list[i][2], linestyle=fmt_list[i][1], marker=fmt_list[i][0]) 
        
        if forplot:
            ax.set_xlim([0., value_spw[i].max()])
            ax.invert_xaxis()
            #ax.set_xlim(left=0.)
            #ax.view_init(0, 90)

        print('--time in distribution plotting: KDE for {1}: {0}'.format(time.time()-time_loc, i))
    
    plt.legend(loc='best' if len(labels) < 16 else (-0.2, -0.2),
               ncol=int(np.sqrt(len(labels))) if len(labels) < 25 else 4,)
    plt.savefig('pdf_' + name + '.png', dpi=1200)
    plt.close()
    """

    # Print main moments
    mom_len = 5
    moments = np.zeros((nftc, mom_len))
    
    """
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
                wfile.write(line+'\n')

    # Check the normality of distribution function
    kld = compare_gaussian(np.exp(log_pdf[0]), x, moments)
    print('KL-d is: ' + str(kld))
    """
    
    return moments

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

def filter_trend(values, method='hpf', name=''):
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
        
        thr = values.shape[-1] * (2**(-14))
        thr_frac = 0.5
        
        val_trend = []
        val_cycle = []
             
        for i in range(nftc):

            val_spectrum = np.fft.rfft(values[i])
            freq = np.fft.rfftfreq(values.shape[-1], 1.)

            #print('v.shape = {}'.format(values.shape)) ### DEBUG
            #print('val_spectrum = {}'.format(val_spectrum)) ### DEBUG

            val_slow_spectrum = val_spectrum.copy()
            val_fast_spectrum = val_spectrum.copy()

            #val_slow_spectrum[int(thr_frac*val_spectrum.shape[0]):] = 0.
            #val_fast_spectrum[:int(thr_frac*val_spectrum.shape[0])] = 0.

            val_slow_spectrum[np.abs(freq) > thr] = 0.
            val_fast_spectrum[np.abs(freq) < thr] = 0.

            val_trend.append(np.abs(np.fft.irfft(val_slow_spectrum)))
            val_cycle.append(np.abs(np.fft.irfft(val_fast_spectrum)))

            en_ap_sl = (np.abs(val_slow_spectrum)**2).sum()
            en_ap_fs = (np.abs(val_fast_spectrum)**2).sum()
            en_ap_tot = en_ap_sl + en_ap_fs
            en_frac = en_ap_sl/en_ap_tot
            print('''Approximation of spectra energy totally: {0:.4e} ; 
                   low frequencies: {1:.4e} ; high frequencies : {2:.4e} ;
                   and fraction of it for low frequencies: {3:.4e}'''.
                       format(en_ap_tot, en_ap_sl, en_ap_fs, en_frac))

            # DEBUGING part of block
            # why the ifft is scaled down around the average?
            # what is the median of frequencies?

            #plt.plot(np.arange(values.shape[0]), val_trend)
            #plt.savefig('debug_fft_trend.png')
            #plt.close()
            #print('which frequencies have high contribution')
            #print(np.argwhere(val_spectrum>10000.))
            #print('freqs: {}'.format(freq))

            # Plotting Fourier spectrum of the windowed time series
            plot_fft(freq, val_spectrum, thr, -2, 'fft_spec_'+name+'_'+str(i))
      
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

def plot_fft(freq, vals, thr, slope=-2, name='fft'):
    """
    Plot spectrum of some univariate function, as well as
        vertical line for some power threshold
        slope of the spectrum
    """

    pivot_val = (vals[-1]**2) * np.power(freq[-1], -slope) 
    
    plt.loglog(freq, np.abs(vals)**2, color='b')
    
    plt.axvline(thr, alpha=0.5, color='r', linestyle='--')
    
    plt.loglog(freq, 
                #pivot_val*np.power(10., slope * np.log10(freq)), 
                pivot_val*np.power(freq, slope), 
                color='k',
                linestyle='--'
            )
    
    plt.savefig(name+'.svg')
    
    plt.close()

def deconvolve_expavg(vals, alpha=1./200.):
    """
    Deconvolve sequence of values produced by exponential averaging and get the original sequence

    Parameters:
    -----------
        vals: array_like
        Original sequence of values obtained after exponential averaging
        alpha: flot
        free memory-discounting parameter of exponential averaging
        a e [0.; 1.] : x[t+1] = a * vals[t+1] + (1.-a) * vals[t]

    Returns:
        array_like
        Original sequence of values before exponential averaging
    """

    xs = np.zeros(vals.shape)
    
    xs[1:] = (1./alpha) * vals[1:] - ((1.-alpha) / alpha) * vals[:-1]

    xs[0] = (1./alpha) * vals[0]

    return xs

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

def read_run_uq(db_path, wd_path='./'):
    """
    Reads information on code runs that was recorded into an EasyVVUQ campaign DB
         Parameters:
             db_path: a full path to location of a DB inside an UQ campiang directory
         Returns:
             a list with dictionaries of run input values (consider keeping explicit run IDs)
             a list of stings with keys/names of input values 
   """
    
    camp_db = db.CampaignDB(location=db_path)
    #camp_db.relocate(wd_path, 'campaign_1')
    #TODO: get the runs information from DB object only
     
    #my_campaign = uq.Campaign(name="campaign_1",
    #                          work_dir=wd_path,
    #                          db_location=db_path)

    #runs = my_campaign.campaign_db.runs()
    runs = camp_db.runs()
    input_values = [r[1]['params'] for r in runs]

    #run1 = my_campaign.campaign_db.run("run_1")
    run1 = camp_db.run("run_1", campaign=1)
    input_names = list(run1['params'].keys())
    print(">Names of params: ".format(input_names))

    return input_values, input_names

def discontinuity_check(vals, reltol=5E-2, abstol=10E4, disc_criterion='combined', n_thr=0):
    """
    Function to check for errors of reading different run cases and error in run numbering
    Checks if for a particular time series there is a time stamp for which next value changed more than by some tolerance factor.
    For each such discontinuity looks for another time series among the list which could fit as a continuation of the studied time series
    """
    
    n = len(vals)

    print('Using {0} as a criterion to find discontinuities in quantity evolution'.format(disc_criterion))
    
    for i in range(n):

        m = len(vals[i][0])
        
        # Numbering is from 0 to m-2, gradient is gX^f_t = X_t+1 - X_t
        diff = vals[i][0][1:] - vals[i][0][:-1]
        # Relative gradient is gXrel^f_t = (X_t+1 - X_t) / X_t
        grad = np.divide(diff, vals[i][0][:-1])

        # Array of second derivatives: interested in gX^f_t+1 - gX^b_t = X_t+2 - X_t+1 - X_t + X_t-1 
        # Numeration from 1 to m-3
        #second_der = vals[i][0][2:] + vals[i][0][:-2] - 2*vals[i][0][1:-1] # first-order Laplacian without prefactor
        second_diff = vals[i][0][3:] - vals[i][0][2:-1] - vals[i][0][1:-2] + vals[i][0][:-3]
        rel_second_diff = np.divide(second_diff, vals[i][0][1:-2])
        
        # Indices of array where gradient (or other studied quantity) is larger then tolerance
        if disc_criterion == 'gradient':
            ts = np.where(abs(grad) > reltol)[0].tolist() #TODO works badly when function growth quickly
        elif disc_criterion == 'absolute_diff':
            ts = np.where(abs(diff) > abstol)[0].tolist() #TODO make tolerance variable
        elif disc_criterion == 'rel_second_diff':
            ts = np.where(abs(rel_second_diff) > reltol)[0].tolist()
            ts = [t+1 for t in ts]
        else: # disc_criterion == 'combined':
            ts1 = np.where(abs(rel_second_diff) > reltol)[0].tolist()
            ts1 = [t+1 for t in ts1 if t+2 in ts1] # unnormalized 4-point second derivative indicates change at +/1 of sought position
            ts2 = np.where(abs(diff) > abstol)[0].tolist()
            
            #ts = [t for t in ts1 if t in ts2] # choosing an intersection of 'lists'
            ts = ts1 + [t for t in ts2 if t not in ts1] # choosing a union of 'lists'

        n_disc_s = len(ts)
        print("For run ind#{0} there are {1} discontinuities".format(i, n_disc_s))

        #print('vals of len {1}: {0}'.format(vals[i][0], n)) ###DEBUG
        #print('relgrad: {}'.format(relgrad)) ###DEBUG
        
        # Looking for such other time series, for which values at the found timestamps where close to the one in the current time series 
        for t in ts:
            cands = []

            for j in range(n):

                # Currently using gradient criterion to found potential mapping
                if np.divide(abs(vals[j][0][t+1] - vals[i][0][t]), vals[i][0][t]) < reltol/1. :
                    cands.append(j)

            print("For run ind#{0} there is a discontinuity at t={1} with original value of {3:.2f}. Most likely candidates for continuation have indices: {2}".
                format(i, t+n_thr, cands, vals[i][0][t]))

    # NB!: here considers only gradient condition
    cands_glob = [
            [
                [
                    np.where(np.divide(abs(vals[j][0][t+1] - vals[i][0][t]), vals[i][0][t]) < reltol)[0].tolist() for j in range(n)
                ] for t in np.where(np.divide(abs(vals[i][0][1:] - vals[i][0][:-1]), vals[i][0][1:]) > reltol)[0].tolist()
            ] for i in range(n)
        ]
    
    return cands_glob


#################################################
#### MAIN FUNCTION: PIPELINE OF PROCESSING ######
#################################################

def main(foldername=False, runforbatch=False, coordnum=1, runnumstart=1, runnum=1, mainfoldernum=None):
    """
    Parameters:
      foldername: relative path to folder where to read cpo files from
      runforbatch: if False then first read values from cpo files, if True then look for a csv file
      coordnum: number of coordinate values (flux tubes) to consider
      runnum: number of different variants of run (e.g. profile shapes) to consider
      manfoldernum: for naming, if None then use cpo folder
    """

    print('\n > Starting a new postprocessing sequence!!! \n')

    if mainfoldernum is None:
        mainfoldernum = foldername # rather bad, fails if foldername is composed

    workdir = os.path.join(os.getenv('SCRATCH'),)

    runnum_list = [r+1 for r in range(runnumstart-1, runnum)]

    # Get the list of folders in the SCRATCH to run over ad get CPOs from
    #   NB1: some runs might be missing
    #   NB2: order of runs in the DB and in the postprocessing script should be the same
    runfolder_list = [f[0] for f in os.walk(foldername) if not f[1]]
    runfolder_list.sort(key=lambda dir: int(dir[dir.rfind('_')+1:]))
    runfolder_list_numbers = [int(r[r.rfind('_')+1:]) for r in runfolder_list]

    #runfolder_list_filtered = [f for f in runfolder_list if (int(f[f.rfind('_')+1:]) if f.rfind('_')!=-1 else 0) in runnum_list]
    runfolder_list_filtered = [f for f in runfolder_list if int(f[f.rfind('_')+1:]) in runnum_list]
    runnum_list_filtered = [r for r in runnum_list if r in runfolder_list_numbers]

    print('The list of runs found from disk and the list of assumed ones are equal: {0}'.format(runnum_list==runnum_list_filtered)) ###DEBUG

    print('foldername={}'.format(foldername)) ###DEBUG
    print('runnum_list of len {1} is : {0}'.format(runnum_list, len(runnum_list))) ###DEBUG
    print('and modified of len {1}: {0}'.format(runnum_list_filtered, len(runnum_list_filtered))) ###DEBUG
    #print('folder list of len {1} of original runs: {0}'.format(runfolder_list, len(runfolder_list))) ###DEBUG
    #print('and modified of len {1}: {0}'.format(runfolder_list_filtered, len(runfolder_list_filtered))) ###DEBUG
    #TODO: check the order of folders and the order of runs!

    code_names = ['gem',
#                 'imp4dv',
           	 ]
    profiles = ['ti_transp', 
                #'te_transp',
                #'ni_transp',
                #'ne_transp'
               ]

    for code_name in code_names:
    
        if code_name == 'imp4dv':
            attributes = ['diff_eff','vconv_eff']
        elif code_name == 'gem':
            attributes = ['flux']
        else:
            print('>Error in start of postprocessing: no such code recognized')


        # 1) If runforbatch, then csv are already in the folder, otherwise have to read CPO-s
        time_start = time.time()
        if not runforbatch:
            # If asserets, then alway specify folders with original cpo files
            # Here are the possibilities where to look for the cpo containing folder:
            # mainfoldernum = 'mft4' # old default
            # 'mft/run4/cpo' # one option
            # 'cpo'+mainfoldernum # other option, if mainfoldernum=='20' etc.

            #mainfoldernum = foldername

            # Get list of directories for runs of profile variation; read them separetely in a loop 
            # (now there are different number of iterations), then load and pass to a new 
            
            # 2) Iterate over all runs, meaning for same scenario but with a different transport profile variation
            #for runn in runnum_list:
            for runn, fname in enumerate(runfolder_list_filtered): 
                
                print('Number of run folder and run file are equal: {0}; {1} and {2}'.
                    format(runn+1 == int(fname[fname.rfind('_')+1:]), runn+1, fname[fname.rfind('_')+1:])) ##DEBUG
                #TODO: check if csv files written in the right order

                #folder_name_curr = os.path.join(workdir, foldername+'/run_'+str(runn)) 
                # TODO make more flexible for different existing cases?
                folder_name_curr = os.path.join(workdir, fname) 
                
                print('Going over CPO-s in the folder: {}'.format(folder_name_curr))
                
                val_ev_s, file_names = profile_evol_load(prof_names=profiles, 
                                                         attrib_names=attributes, 
                                                         coord_len=coordnum, 
                                                         folder_name=folder_name_curr, 
                                                         file_code_name=code_name, 
                                                         name_postfix='_'+mainfoldernum+'_'+str(runn+1) # here runn is from enumerate, not from the list
                                                        )
               
                #val_ev_s, file_names = profile_evol_load(prof_names=profiles, attrib_names=attributes, coord_len=coordnum, 
                #                                         folder_name=os.path.join(workdir, 'cpo'+mainfoldernum), 
                #                                         file_code_name=code_name, 
                #                                         name_postfix='_'+mainfoldernum       

                #TODO: 1) +
                #      2) get rid of recurcive copy-ing in parent .sh file -> check if solution works
                #      3) make responce cuts flexible: cases when sometimes there is one value per cut

        print("time to load cpo files: {0} s".format(time.time()-time_start))
        # 3) Getting the input profiles values, primarily for the plot labels
        time_start = time.time()

        pos_str_cpo_num = mainfoldernum.rfind('_')+1
        cpo_num = int(mainfoldernum[pos_str_cpo_num:]) # if the leaf folder is named [a+]_[d+]
        #cpo_num = int(foldername[foldername.rfind('/')+1:])
        
        #print('cpo_num={0} and mainfoldernum={1}'.format(cpo_num, mainfoldernum)) ###DEBUG
        mmiter_num = cpo_num

        #db_id = 10002794 # where to get this among EasyVVUQ files?
        #file_runs_db = "../gem_notransp_db_"+str(db_id)+".json" 

        camp_id = mainfoldernum[mainfoldernum[:pos_str_cpo_num-1].rfind('_')+1:pos_str_cpo_num-1]

        workdir_camp_db = './' 

        #TODO: take the internal campaign folder ID as input, and then load the SLURM id -> 
        # -> probably for that it is better to import EasyVVUQ and intilalise the campaign
        #     with the location of DB, 
        #    then use the [my_]campaign.campaign_db.[func-s]() to read stuff about runs
        
        #runs_db_loc = "sqlite:///" + foldername + "/.."*7 + "/campaign.db"
        runs_db_loc = "sqlite:///" + "campaign_" + camp_id + "_" + str(mmiter_num) + ".db"
        runs_input_vals, runs_input_names = read_run_uq(runs_db_loc, 
                                                    workdir_camp_db) # test this function, at least manually  
        
        #print('runs_input_names={}'.format(runs_input_names)) ###DEBUG 
        #TODO: check which DB is copied - no run_name are in default copied ones
        #TODO: gem_ti_transp_flux_evol_*_*_80.csv is not created apparently
           
        # 3') By default, create new list for readings and read them from file, 
        #     even if they are in programm memory already      
        val_ev_s = []

        alpha_wind = 0.3 # how much to discard, in fractions of readings

        print("time to read and set-up basic values: {0} s".format(time.time()-time_start))
        # 4) Iterate over cartesian product of all profiles and their attributes

        for i,(p,a) in enumerate(itertools.product(profiles, attributes)):

            # After processing given profile/quantity we are not intersted in values
            val_ev_s = []
            print("Now running through {0} and {1}".format(p,a))

            # 4.0) Assuming there are multiple ensembles of runs for this submission 
            #     (corresponding to a global iteration of UQ campaigns) read all the 'runs' of an ensemble 
            #     in a single list of arrays
            time_start = time.time()
            
            #csv_file_name = code_name+'_'+p+'_'+a+'_evol_'+mainfoldernum+'.csv'
            for runn in runnum_list:
                csv_file_name = code_name + '_' + p + '_' + a + '_evol_' + mainfoldernum + '_' + str(runn) + '.csv'
                val_ev_s.append(np.atleast_2d(np.genfromtxt(csv_file_name, delimiter=", ").T))     
            
            #TODO for some reason the previous line fails for (foldername='/ptmp/yyudin//VARY_1FT_GEM_NT_n2qks5e7/runs//cpo/1', runforbatch=1, coordnum=1, runnum=64, mainfoldernum='new_n2qks5e7_1')
        
            print("time to read values from CSV files: {0} s".format(time.time()-time_start))

            # 4.1) Plot the read values, pass a list of array
            time_start = time.time()

            #profile_evol_plot([val_ev_s[i]], name=code_name+'_'+p+'_'+a+'_'+mainfoldernum)
            # modification: list of arrays is for different profile variations
            
            #labels = [str(r) for r in runnum_list]
            labels = ["".join([rin+'='+str(round(r[rin], 1))+"; " for rin in runs_input_names]) for r in runs_input_vals]
               
            profile_evol_plot(val_ev_s, labels=labels, name=code_name+'_'+p+'_'+a+'_'+mainfoldernum, alignment='start', vertline=alpha_wind*val_ev_s[0].shape[-1]) 

            #print('passes to plot: {}'.format(val_ev_s[0].shape)) ###DEBUG
            #print('before shape {}'.format(val_evname=code_name+'_'+p+'_'+a+'_'+mainfoldernum, alignment='start'_s[i].shape)) ###DEBUG
            #val = np.array(val_ev_s[i]).squeeze()
            #print('after shape {}'.format(val.shape)) ### DEBUG            
            #print('total number of different cases {}'.format(len(val_ev_s))) ###DEBUG
 
            ##ti_flux = np.genfromtxt('gem_ti_flux.csv', delimiter =", ")

            # 4.1'*) Compare flux values from turbulent code and the values that where used before exponential averaging
            
            """
            val_deconv = deconvolve_expavg(val_ev_s[0])
            
            profile_evol_plot([val_ev_s[0], val_deconv], labels=['original', 'deconvolved'],
                              name=code_name+'_'+p+'_'+a+'_'+mainfoldernum+'_deconv', 
                              alignment='start')
            """

            # 4.1') Define the window to discard intial ramp-up and overshooting phase
            val = val_ev_s[i] # single series for a profile+attribute, shouldn't be used for now...

            n_thrown_vals = int(alpha_wind*val.shape[-1])
            val_wind_s = [val[:,n_thrown_vals:] for val in val_ev_s]
            #print('sizes before and after windowing: {} and {} '.format(val_ev_s[0].shape, val_wind_s[0].shape)) ###DEBUG
            #print('val_wind len and element shape are {} and {}'.format(len(val_wind), val_wind[0].shape)) ### DEBUG
   
            print("time to discard values, set up labels etc.: {0} s".format(time.time()-time_start))
            
            # 4.1'') Check if there are any discontinuities in the evolution
            time_start = time.time()

            discontinuity_check(val_wind_s, disc_criterion='combined', n_thr=n_thrown_vals)

            print("time to go over time traces and look for discontinuities: {0} s".format(time.time()-time_start))
            
            # 4.2) Calculate ACF for the values
            time_start = time.time()
           
            #lags_list = [2**i for i in range(12) if 2**i < val_wind_s[i].shape[-1]] #add intermediate vals + sort
            lags_list = [1,2,4,8,16,32,48,64,96,128,160,256,512,1024,2048,4096] # [64,128,256]
            lags_list = [l for l in lags_list if l < val_wind_s[i].shape[-1]]
            
            #get_coreprof_ev_acf(val_ev_s[i], name=code_name+'_'+p+'_'+a+'stats'+'_'+str(runn), lags=lags_list)
            
            ac_len_s = []
            ac_num_s = []

            val_ev_acf_s = []
            
            #for runn in range(len(runnum_list)):
            for runn in runnum_list:
                
                print('ACF for case #{0}'.format(runn))
                 
                ac_len, ac_num = get_coreprof_ev_acf(val_wind_s[runn-runnumstart], 
                                    name=code_name+'_'+p+'_'+a+'_stats_'+mainfoldernum+'_'+str(runn), 
                                    lags=lags_list) 

                #NB!: uncertainty of the ACF computation ~ Var(X)/sqrt(n) , where n=N_samples/L_lags
                ac_len_s.append(ac_len)
                ac_num_s.append(ac_num)
                #TODO: acf function assumes multiple cases are passed and returns list - ..[0] is a workaround, change
                print('Approximate ACL: {0}; and effective number size is {1}'.format(ac_len[0], ac_num[0]))
                 
                # Populate new array with reading from the code-produced values taken per a ACL window
                # Take one reading for a miidle of ACL
                val_ev_acf = np.ones((1, ac_num[0]))
                # option 1: centres of the interval
                #val_ev_acf = val_wind_s[runn-1][0, int(ac_len[0]/2.):-1:int(ac_len[0])]
                # option 2: means of the interval
                val_ev_acf = np.array([val_wind_s[runn-runnumstart][0, i*int(ac_len[0]):(i+1)*int(ac_len[0])].mean() for i in range(int(ac_len[0]))])
                val_ev_acf_s.append(val_ev_acf)

                #print([ac_num[0], ac_len[0], val_wind_s[runn-1].shape, val_ev_acf.shape,]) ###DEBUG        

            print("time to calculate autocorrelation time and effective sample: {0} s".format(time.time()-time_start))
            # 4.3) Plotting histograms and KDEs of the profile values evolution
            time_start = time.time()
            #plot_coreprofval_dist(val_ev_s[i], name=p+'_'+a+'_'+mainfoldernum, discr_level=32)
            
            #for runn in range(len(runnum_list)):
            for runn in runnum_list:
                """
                print('KDE for case #{0}'.format(runn)) 
                
                plot_coreprofval_dist([np.squeeze(val_wind_s[runn-1],0)],
                                      name=p+'_'+a+'_'+str(runn)+'_'+mainfoldernum, 
                                      discr_level=32)
                """
                #TODO: pass number of case to save different files

            print("time to plot single histograms and KDEs: {0} s".format(time.time()-time_start))
            # 4.3.1) Plotting single plot with histograms, KDEs and distribution means
            time_start = time.time()

            """
            plot_coreprofval_dist(
                                  [np.squeeze(v, 0) for v in val_wind_s], 
                                  labels=labels, 
                                  name='tot_'+p+'_'+a+'_'+mainfoldernum, 
                                  discr_level=32,
                                  forplot=False,
                                  )
            """

            print("time to plot global histogram and KDE: {0} s".format(time.time()-time_start))
            # 4.3.2) Plotting single plot with histograms including data from MFW production runs
            time_start = time.time()

            mfw_input_names = ['dTi', 'dTe', 'Ti', 'Te']

            mfw_data_file = 'AUG_mix-lim_gem_inoutput.txt' # 'AUG_gem_inoutput.txt'
            mfw_ft_s = [5, 6, 7]

            val_mwf = pd.read_table('../data/'+mfw_data_file, delimiter='  *', engine='python') 
            print("-time to load MFW data and set-up stuff: {0} s".format(time.time()-time_start))
            time_start_tmp = time.time()
            
            if   p+'_'+a == 'ti_transp':
                val_mwf_s = [val_mwf['cp-flux-Ti-ft'+str(mfw_ft)].to_numpy().reshape(1,-1) for mfw_ft in mfw_ft_s]
            elif p+'_'+a == 'te_transp':
                val_mwf_s = [val_mwf['cp-flux-Te-ft'+str(mfw_ft)].to_numpy().reshape(1,-1) for mfw_ft in mfw_ft_s]
            else:
                #Fall-back option
                val_mwf_s = [val_mwf['cp-flux-Ti-ft'+str(mfw_ft)].to_numpy().reshape(1,-1) for mfw_ft in mfw_ft_s]

            mfw_input_refval_s = [[val_mwf[input_name+'-ft'+str(mfw_ft)].mean() for mfw_ft in mfw_ft_s] for input_name in mfw_input_names]
            print("-time to select right columns: {0} s".format(time.time()-time_start_tmp))
            time_start_tmp = time.time()
            
            #print(' Shapes of old and new arrays {0} {1}'.format(val_wind_s[0].shape, val_mwf.shape)) ### DEBUG
            #print(' MFW input values are dti={0} dte={1} ti={2} te={3}'.format(tiddrho_mwf_refval_s[0], teddrho_mwf_refval_s[0], ti_mwf_refval_s[0], te_mwf_refval_s[0])) ### DEBUG
            print('Mean of MFW ft5 QTi={0}'.format(val_mwf['cp-flux-Ti-ft5'].mean())) ###DEBUG

            # Mind that here 0 index is considered to be most important to compare: flux tube #5 from MFW run

            plot_coreprofval_dist(
                                    [np.squeeze(v,0) for v in [*val_wind_s, *val_mwf_s]],
                                    labels=[*labels, *['mfw_ft'+str(mfw_ft_s[i])+'; tiddrho={:.2f}'.format(mfw_input_refval_s[0][i]) for i in range(len(mfw_ft_s))]],
                                    name='tot_mwf_'+str(mfw_ft_s[0])+'_'+p+'_'+a+'_'+mainfoldernum,
                                    discr_level=32,
                                    forplot=False,
                                 )

            print("-time to plot the MFW histograms only: {0} s".format(time.time()-time_start_tmp))            
            val_mwf_mean = val_mwf_s[0].mean()
            val_mwf_min = val_mwf_s[0].min()
            val_mwf_max = val_mwf_s[0].max()

            print("time to compare with MFW production runs: {0} s".format(time.time()-time_start))
            # 4.4) Apply ARMA model
            """
            apply_arma(val)
            """

            # 4.5)  Apply different averaging methods and plot the results
            
            # 4.5.1) Calculating the mean of last *1-alpha* reads
            time_start = time.time()
            
            #print('input names : {}'.format(runs_input_names)) ###DEBUG
            #print('inputs values : {}'.format(runs_input_vals)) ###DEBUG            

            runs_input_names_new = [n.replace('.', '_') for n in runs_input_names]
            stats_df = pd.DataFrame(columns=['name', 'mean', 'std'])
            scan_df = pd.DataFrame(columns=['index', 'name'] 
                                         + runs_input_names_new
                                         + [p+'_'+a, p+'_'+a+'_std', p+'_'+a+'_stem'])

            val_trend_avg_s = []
            val_std_s = []

            val_trend_avg_acf_s = []
            val_std_acf_s = []

            for runn in runnum_list:

                val_trend_avg, val_fluct_avg = filter_trend(val_wind_s[runn-runnumstart], "mean")
                val_trend_avg_s.append(val_trend_avg)
                val_std_s.append(val_fluct_avg) 
                #TODO: look up Python ways to process tuple elements differently

                # Calculate mean and std using only single point per correlation length
                # TODO mind that currently all values for slices are taken from acf-thinned data
                val_trend_avg_acf, val_fluct_avg_acf = filter_trend(val_ev_acf_s[runn-runnumstart], "mean")
                val_trend_avg_acf_s.append(val_trend_avg_acf)
                val_std_acf_s.append(val_fluct_avg_acf)
            
                n_lensample = val_trend_avg_acf_s[runn-runnumstart].shape[-1]
                #print('acf-corrected sample length: {0}'.format(n_lensample)) ###DEBUG

                scan_df, stats_df = produce_stats_dataframes(runs_input_vals,
                                                             val_trend_avg_s,
                                                             val_std_s,
                                                             stats_df,
                                                             scan_df,
                                                             n_lensample, 
                                                             runn-runnumstart, 
                                                             p, a)

             
            profile_evol_plot(val_trend_avg_s, labels=labels, name='means_'+p+'_'+a+'_'+mainfoldernum)
            

            stats_df.to_csv('stats_main_'+p+'_'+a+'_'+mainfoldernum+'.csv') 
            scan_df.to_csv('resuq_main_'+p+'_'+a+'_'+mainfoldernum+'.csv')    
            
            print("time to calculate and save basic moments: {0} s".format(time.time()-time_start))
            # 4.5.1b) Plot parameter dependency for single parameters
            time_start = time.time()

            for param in runs_input_names_new:
                # For Pandas query: make sure input columns are floats
                scan_df[param] = scan_df[param].astype('float')

            compare_vals_mfw = (val_mwf_mean, val_mwf_min, val_mwf_max)

            print('plotting cuts starting')
            
            """
            plot_response_cuts(scan_df, 
                               runs_input_names_new, 
                               [p+'_'+a],
                               compare_vals=compare_vals_mfw, 
                               foldname=p+'_'+a+'_'+mainfoldernum,
                               traces=val_ev_s, #val_wind_s,
                               hists=True,
                              )
            """

            #4.5.1c) Plotting time traces for one case with its AVG, STD, SEM
            #runn_loc = 6
            runnum_list_loc = [29]
            for runn_loc in runnum_list_loc:
                #print('ACN here is {0} and total len is {1}'.format(scan_df.iloc[runn_loc-1]['ti_transp_flux_acn'], len(val_wind_s[runn_loc-1][0]))) ###DEBUG
                plot_timetraces_act(
                        val_ev_s[runn_loc-runnumstart][0][:],
                        avg=scan_df.iloc[runn_loc-runnumstart]['ti_transp_flux'],
                        std=scan_df.iloc[runn_loc-runnumstart]['ti_transp_flux_std'],
                        sem=scan_df.iloc[runn_loc-runnumstart]['ti_transp_flux_stem'],
                        foldname=p+'_'+a+'_'+mainfoldernum+'_'+str(runn_loc),
                        apha_discard=0.15,
                        act=int(len(val_wind_s[runn_loc-runnumstart][0])/scan_df.iloc[runn_loc-runnumstart]['ti_transp_flux_acn']),
                                    )
                
            #4.5.1d) Plotting time traces for one case with its AVG, STD, SEM, each taken per run
            #runn_loc = 6
                time_traces_per_run(
                        val_ev_s[runn_loc-runnumstart][0][:],
                        run_len=150,
                        foldname=p+'_'+a+'_'+mainfoldernum+'_'+str(runn_loc),
                        apha_discard=0.15,
                                    )

            print('plotting cuts done')

            print("time to plot scans for different parameters: {0} s".format(time.time()-time_start))
            # 4.5.2) Calcualting linear regression against time fit for the last window:
            # Apply LR to get form of Q=a*t+b
            val_trend_lr_s = []
            for runn in runnum_list:
                
                val_trend_lr, val_residue_lr = filter_trend(val_wind_s[runn-runnumstart], "linear_regression")
                val_trend_lr_s.append(val_trend_lr.reshape(val_wind_s[runn-runnumstart].shape)) # bad workaround
                
                #print('>shapes passed to plot: {} and {}'.
                #       format(val_wind_s[runn-1].shape, val_trend_lr_s[runn-1].shape)) ###DEBUG
                
                profile_evol_plot([val_wind_s[runn-runnumstart], val_trend_lr_s[runn-runnumstart]],
                                  labels=['original', 'linear regression with NE'],
                                  name='lr_'+p+'_'+a+'_'+str(runn-runnumstart)+'_'+mainfoldernum) 
                
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
                val_trend_fft, val_fluct_fft = filter_trend(val_wind_s[runn-1], "fft", 
                                                 name=p+'_'+a+'_'+mainfoldernum+'_'+str(runn))
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
            plot_coreprofval_dist(val_fluct_avg, name='wind_'+code_name+p+'_'+a+'_'+mainfoldernum, discr_level=128)
            """

            #TODO get exponential average of the values: standard packaged optimize for alpha -
            # - why it is so high? is composition of exponential avaraging is another exponential averagings -
            # - if so, what is alpha_comp?
            #TODO exponential averaging with a symmetric window -> apply padding

#################################
#### OPTIONS FOR MAIN() CALL ####
#################################

if __name__ == '__main__':

    if len(sys.argv) == 2:
        # Run main with default values for all files in the folder
        main(foldername=sys.argv[1])
    elif len(sys.argv) == 3:
        # Run main with default value for all files in the folder, but considering there might be a csv composed already
        main(foldername=sys.argv[1], runforbatch=int(sys.argv[2]))
    elif len(sys.argv) == 4:
        # Run main for all files in the folder, either read values from csv (runforbatch), and for multiple flux tubes (coord)
        main(foldername=sys.argv[1], runforbatch=int(sys.argv[2]), coordnum=int(sys.argv[3]))
    elif len(sys.argv) == 5:
        # Run main for all files in the folder, either read values from csv (runforbatch), 
        # for possible multiple flux tubes (coord), and specifying how to save files
        main(foldername=sys.argv[1], runforbatch=int(sys.argv[2]), coordnum=int(sys.argv[3]), mainfoldernum=sys.argv[4])
    elif len(sys.argv) > 5:
        # Run main for all files in folder, possibly reading values from csv, possibly for multiple flux tubes, 
        # and for possibly many cases (e.g. different input profiles) + specify save folder
        main(foldername=sys.argv[1], runforbatch=int(sys.argv[2]), coordnum=int(sys.argv[3]), runnumstart=int(sys.argv[4]),
             runnum=int(sys.argv[5]), mainfoldernum=sys.argv[6])
    else:
        # Run main with all defaults
        main()

