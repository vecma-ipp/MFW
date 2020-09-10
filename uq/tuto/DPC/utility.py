import numpy as np
import pandas as pd
import os
import matplotlib
if not os.getenv("DISPLAY"): matplotlib.use('Agg')
import matplotlib.pylab as plt
from IPython.display import set_matplotlib_formats
set_matplotlib_formats('png', 'pdf')

from numba import jit
from numba import float64
from numba import int64
@jit((float64[:], float64), nopython=True, nogil=True)
def _ewma(arr_in, alpha):
    r"""Exponentialy weighted moving average specified by a decay parameter

        y[t] = (x[t] + (1-a)*x[t-1] + (1-a)^2*x[t-2] + ... + (1-a)^n*x[t-n]) /
               (1 + (1-a) + (1-a)^2 + ... + (1-a)^n).

    Parameters
    ----------
    arr_in : np.ndarray, float64
        A single dimensional numpy array
    alpha : int64
        The decay parameter

    Returns
    -------
    np.ndarray
        The EWMA vector, same length / shape as ``arr_in``
    """
    n = arr_in.shape[0]
    ewma = np.empty(n, dtype=float64)
    w = 1
    ewma_old = arr_in[0]
    ewma[0] = ewma_old
    for i in range(1, n):
        w += (1-alpha)**i
        ewma_old = ewma_old*(1-alpha) + arr_in[i]
        ewma[i] = ewma_old / w
    return ewma

def plot_correlations(data, active_t):
    r"""Plot the correlations
    
    Parameters
    ----------
    data : pandas dataset 
        data from a multi-fluxtube workflow simulation
    active_t : array of arrays of strings 
        column names in data

    Returns
    -------
    nothing except the plots

    """
    for t in active_t:
        axes = pd.plotting.scatter_matrix(data[t], alpha=0.5, diagonal='hist', figsize=(7,4))
        corr = np.array(data[t].corr())
        for i, j in zip(*plt.np.triu_indices_from(axes, k=1)):
            axes[i, j].annotate("%.3f" %corr[i,j], (0.8, 0.8), xycoords='axes fraction', ha='center', va='center')
        plt.figure(figsize=(7,4))
        plt.imshow(corr, cmap='RdBu_r', vmin=-1, vmax=+1)
        plt.xticks(np.arange(corr.shape[0]), t, rotation=90); plt.yticks(np.arange(corr.shape[1]), t); plt.colorbar();
        
def plot_smoothed(data, what, logy=True, figsize=(24,8)):
    r"""Plot smoothed VECMA fusion WF data
    
    Parameters
    ----------
    data : array of dictionaries containing
        "data" : pandas dataset containing data from a multi-fluxtube workflow simulation
        "style" : text string containing the matplotlib line-style
    what : array of dictionaries containing
        "cols" : array of strings containing column names
        "label" : label for the y-axis
        "limit" : y-limits for the plot
    logy : boolean
        A flag to indicate whether to use a logaritmic scale for the y axis (Default True)

    Returns
    -------
    nothing except the plots
    """
    fig, ax = plt.subplots(1, len(what), figsize=figsize)
    ax = np.atleast_1d(ax)
    for i, W in enumerate(what):
        for D in data:
            for c in W["cols"]:
                M = _ewma(np.array(D["data"][c]), 1e-3)
                M2 = _ewma(np.array(D["data"][c])**2, 1e-3)
                S = np.sqrt(M2-M**2)
                base_line, = ax[i].plot(D["data"].time, M, D["style"], label=c)
                ax[i].fill_between(D["data"].time, M-S, M+S, facecolor=base_line.get_color(), alpha=0.1)
            ax[i].set_prop_cycle(None)
            if logy: ax[i].set_yscale('log')
        ax[i].legend(loc=0, ncol=4)
        ax[i].set_title('smoothing alpha = 1e-3')
        ax[i].set_ylim(W["limit"])
        ax[i].set_xlabel('time [s]')
        ax[i].set_ylabel(W["label"]);

def plot_unsmoothed(data, what, logy=True, figsize=(24,8)):
    r"""Plot unsmoothed VECMA fusion WF data
    
    Parameters
    ----------
    data : array of dictionaries containing
        "data" : pandas dataset containing data from a multi-fluxtube workflow simulation
        "style" : text string containing the matplotlib line-style
    what : array of dictionaries containing
        "cols" : array of strings containing column names
        "label" : label for the y-axis
        "limit" : y-limits for the plot
    logy : boolean
        A flag to indicate whether to use a logaritmic scale for the y axis (Default True)

    Returns
    -------
    nothing except the plots
    """
    fig, ax = plt.subplots(1, len(what), figsize=figsize)
    ax = np.atleast_1d(ax)
    for i, W in enumerate(what):
        for D in data:
            D["data"].plot(x='time', y=W["cols"], logy=logy, ax=ax[i], style=D["style"]);
            ax[i].set_prop_cycle(None)
    for i, W in enumerate(what):
        ax[i].legend(loc=0, ncol=4)
        ax[i].set_ylim(W["limit"])
        ax[i].set_xlabel('time [s]')
        ax[i].set_ylabel(W["label"]);

def analyze(wf_data, wf_columns, uq_data, uq_column, xlabel, row):
    r"""Analyze a column group
    
    Parameters
    ----------
    wf_data : pandas dataset 
        data from a multi-fluxtube workflow simulation
    wf_columns : array of strings 
        column names in wf_data
    uq_data : pandas dataset 
        data from a UQ campaign
    uq_column : string
        name of column in uq_data
    xlabel : string
        xlabel for the histograms
    row : integer
        for the second column, the starting point of the wf_data that hoipfully represents the start of the steady-state

    Returns
    -------
    nothing except the plots
    """
    fig, ax = plt.subplots(2,2, figsize=(16,8))
    wf_data.plot(x='time', y=wf_columns, ax=ax[0][0]);
    wf_data.iloc[row:].plot(x='time', y=wf_columns, ax=ax[0][1]);

    cols = wf_columns.copy()
    for c in cols:
        wf_data.hist(column=c, alpha=0.25, density=True, ax=ax[1][0])
    uq_data.hist(column=uq_column, alpha=0.75, density=True, ax=ax[1][0])
    cols.append(uq_column)
    ax[1][0].legend(cols, ncol=3)
    ax[1][0].set_xlabel(xlabel)
    ax[1][0].set_title("all rows");
    
    cols = wf_columns.copy()
    for c in cols:
        wf_data.iloc[row:].hist(column=c, alpha=0.25, density=True, ax=ax[1][1])
    uq_data.hist(column=uq_column, alpha=0.75, density=True, ax=ax[1][1])
    cols.append(uq_column)
    ax[1][1].legend(cols, ncol=3)
    ax[1][1].set_xlabel(xlabel)
    ax[1][1].set_title("from row %s onwards" % (row));