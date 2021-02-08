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
# TODO add mins and maxs
def plot_moments(x, mean, std, xlabel, ylabel, ftitle, fname):
    matplotlib.rcParams.update({'font.size': 12})

    plt.switch_backend('agg')
    fig = plt.figure(figsize=(12,9))

    plt.plot(x, mean, 'r-', label='Mean')
    plt.plot(x, mean-std, 'b-', label=r'Mean $\pm$1 std')
    plt.plot(x, mean+std, 'b-')
    plt.fill_between(x, mean-std, mean+std, color='b', alpha=0.25)
    plt.xlabel(xlabel)
    plt.ylabel(ylabel)
    plt.grid()
    plt.legend()

    plt.title(ftitle)
    fig.savefig(fname)
    plt.close(fig)

# Plot Sobols indices
def plot_sobols(x, xlabel, sobols, ylabel,  params, ftitle, fname, subplot=False):
    plt.switch_backend('agg')
    npar = len(params)

    # the subplots are supported for 5 to 9 params
    if subplot:
        if npar==5 or npar==6:
            fig, axs = plt.subplots(nrows=2, ncols=3, sharex=True, sharey=True)
        if npar==7 or npar==8 or npar==9:
            fig, axs = plt.subplots(nrows=3, ncols=3, sharex=True, sharey=True)

        for i in range(npar):
            ax = axs[i//3, i%3]
            s = sobols[params[i]]
            ax.plot(x, s)
            ax.grid()
            ax.set_title(params[i])
    else:
        fig = plt.figure(figsize=(12,9))
        ax = fig.add_subplot(111)
        for i in range(npar):
            s = sobols[params[i]]
            ax.plot(x, s, label=params[i])
            ax.legend()
            ax.grid()

    fig.suptitle(ftitle)
    plt.xlabel(xlabel)
    plt.ylabel(ylabel)
    plt.close(fig)
    fig.savefig(fname)
    plt.close(fig)
