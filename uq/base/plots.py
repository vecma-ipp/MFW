import matplotlib.pylab as plt
import matplotlib
import numpy as np


# TODO add mins and maxs instead of 1% and 99%
def plot_moments(mean, std, per=None, x=None, xlabel=None, ylabel=None,
                 ftitle=None, fname=None, dpi=200, fsize=11):
    matplotlib.rcParams.update({'font.size': fsize})

    plt.switch_backend('agg')
    fig, ax = plt.subplots()

    # qoi is a scalar: bar plot with error bars
    if x is None:
        x = np.arange(1, len(mean)+1)
        ax.bar(x, mean, yerr=std, width=0.8, align='center', alpha=0.5,
                ecolor='black', capsize=10)
        ax.grid(True, axis='y')
    # qoi is a vector: profile with mean +- std
    else:
        ax.plot(x, mean, 'b-', label='Mean')
        ax.plot(x, mean-std, 'b--', alpha=0.2, label=r'Mean $\pm$1 std')
        ax.plot(x, mean+std, 'b--', alpha=0.2)
        if per is not None:
            ax.plot(x, per[0], 'k--', alpha=0.4, label='1% and 99%')
            ax.plot(x, per[1], 'k--', alpha=0.4)
        ax.fill_between(x, mean-std, mean+std, color='blue', alpha=0.2)
        ax.grid()
        ax.legend()

    ax.set_xlabel(xlabel)
    ax.set_ylabel(ylabel)
    fig.suptitle(ftitle)

    if fname is None:
        fname = 'fig_moments.png'
    plt.savefig(fname, dpi=dpi)
    plt.close(fig)

def plot_sobols(sobols, params=None, x=None, xlabel=None, ylabel=None,
                ftitle=None, fname=None, dpi=200, fsize=11):
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

def plot_sobols_8(sobols, params=None, x=None, xlabel=None, ylabel=None,
                ftitle=None, fname=None, dpi=200, fsize=10):
    matplotlib.rcParams.update({'font.size': fsize})

    plt.switch_backend('agg')
    fig, axs = plt.subplots(nrows=2, ncols=4, sharex=True, sharey=True)

    if params is None:
        params = {p:p for p in sobols.keys()}

    for i, par in enumerate(sobols.keys()):
        s = sobols[par]
        ax = axs[i//4, i%4]
        ax.plot(x, s)
        ax.set_title(params[par])
        ax.grid()

    fig.suptitle(ftitle)

    if fname is None:
        fname = 'fig_sobols.png'
    plt.savefig(fname, dpi=dpi)
    plt.close(fig)
