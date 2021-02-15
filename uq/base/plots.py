import matplotlib.pylab as plt
import matplotlib
import numpy as np


# TODO add mins and maxs
def plot_moments(mean, std, x=None, xlabel=None, ylabel=None,
                 ftitle=None, fname=None, dpi=128, fsize=11):
    matplotlib.rcParams.update({'font.size': fsize})

    plt.switch_backend('agg')
    fig, ax = plt.subplots()

    # qoi is a scalar: bar plot with error bars
    if x is None:
        x = np.arange(1, len(mean)+1)
        ax.bar(x, mean, yerr=std, align='center', alpha=0.5,
                ecolor='black', capsize=10)
        ax.grid(True, axis='y')
    # qoi is a vector: profile with mean +- std
    else:
        ax.plot(x, mean, 'r-', label='Mean')
        ax.plot(x, mean-std, 'b-', label=r'Mean $\pm$1 std')
        ax.plot(x, mean+std, 'b-')
        ax.fill_between(x, mean-std, mean+std, color='b', alpha=0.25)
        ax.grid()
        ax.legend()

    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)
    fig.suptitle(ftitle)

    if fname is None:
        fname = 'fig_moments.png'
    plt.savefig(fname, dpi=dpi)
    plt.close(fig)

# TODO add multiple subplots (for 5+ params for eg.)
def plot_sobols(sobols, params=None, x=None, xlabel=None, ylabel=None,
                ftitle=None, fname=None, dpi=128, fsize=11):
    matplotlib.rcParams.update({'font.size': fsize})

    plt.switch_backend('agg')
    fig, ax = plt.subplots()

    if params is None:
        params = list(sobols.keys())

    # qoi is a scalar: bar plots
    if x is None:
        for par in params:
            s = sobols[par]
            ax.bar(par, s, 0.4, label=par)

    # qoi is a vector: profile plots
    else:
        for par in params:
            s = sobols[par]
            ax.plot(x, s, label=par)
            ax.legend()

    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)
    ax.grid(True, axis='y')
    fig.suptitle(ftitle)

    if fname is None:
        fname = 'fig_sobols.png'
    plt.savefig(fname, dpi=dpi)
    plt.close(fig)
