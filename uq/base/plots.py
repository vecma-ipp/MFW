import matplotlib.pylab as plt
import matplotlib
import numpy as np


# Scaling coordianates
def format_exponent(ax, axis='y'):
    # Change the ticklabel format to scientific format
    ax.ticklabel_format(axis=axis, style='sci', scilimits=(-2, 2))

    # Get the appropriate axis
    if axis == 'y':
        ax_axis = ax.yaxis
        x_pos = 0.0
        y_pos = 1.0
        horizontalalignment='left'
        verticalalignment='bottom'
    else:
        ax_axis = ax.xaxis
        x_pos = 1.0
        y_pos = -0.05
        horizontalalignment='right'
        verticalalignment='top'


# Statistical Moments (mean +- std)
# TODO add min and max
def plot_moments(x, mean, std, xlabel, ylabel, ftitle, fname=None):
    matplotlib.rcParams.update({'font.size': 12})

    plt.switch_backend('agg')
    fig = plt.figure(figsize=(12,9))

    plt.plot(x, mean, 'r-', label='Mean')
    plt.plot(x, mean-std, 'b--', label=r'Mean $\pm$1 std')
    plt.plot(x, mean+std, 'b--')
    plt.fill_between(x, mean-std, mean+std, color='b', alpha=0.25)
    plt.xlabel(xlabel)
    plt.ylabel(ylabel)
    plt.grid()
    plt.legend()

    plt.title(ftitle)
    if filename is None:
        fig.show()
    else:
        fig.savefig(fname)
    plt.close(fig)

# Plot Sobols indices for 2, 4 or 6 params
def plot_sobols(x, sobols, params, ftitle, fname):
    plt.switch_backend('agg')
    npar = len(params)

    if npar==2:
        fig = plt.figure(figsize=(12,9))
        ax = fig.add_subplot(111)
        s = sobols[params[0]]
        ax.plot(x, s, label=params[0])
        s = sobols[params[1]]
        ax.plot(x, s, label=params[1])
        ax.legend()
        ax.grid()
        ax.set_title(ftitle)
        fig.savefig(fname)
        plt.close(fig)

    if npar==4:
        fig, axs = plt.subplots(nrows=2, ncols=2, sharex=True, sharey=True)
        for i in range(npar):
            ax = axs[i//2, i%2]
            s = sobols[params[i]]
            ax.plot(x, s)
            ax.grid()
            ax.set_title(params[i])
        fig.suptitle(ftitle)
        fig.savefig(fname)
        plt.close(fig)


    if npar==6:
        fig, axs = plt.subplots(nrows=2, ncols=3, sharex=True, sharey=True)
        for i in range(npar):
            ax = axs[i//3, i%3]
            s = sobols[params[i]]
            ax.plot(x, s)
            ax.grid()
            ax.set_title(params[i])

        fig.suptitle(ftitle)
        fig.savefig(fname)
        plt.close(fig)


# Plot Sobols indices (all in the same figure)
def plot_sobols_all(x, sobols, params, ftitle, fname):
    plt.switch_backend('agg')
    npar = len(params)

    plt.switch_backend('agg')
    fig = plt.figure(figsize=(12,9))
    ax = fig.add_subplot(111)

    for i in range(npar):
        ls = len(sobols[params[i]])
        for j in range(ls):
            s = sobols[params[i]][j]
            if ls==1:
                ax.plot(x, s, label=params[i])
            else:
                ax.plot(x, s, label=params[i]+str(j))

    ax.set_xlabel(r'$\rho_{tor} ~ [m]$')
    ax.set_ylabel('Sobol index')

    ax.set_title(ftitle)
    plt.legend()
    fig.savefig(fname)
    plt.close(fig)

