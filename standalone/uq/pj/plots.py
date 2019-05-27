import matplotlib.pylab as plt
import numpy as np


# Util for scaling coordianates
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


# Statistical Moments (mean +- deviation and variance)
def plot_stats(x, stat, xlabel, ylabel, ftitle, fname):
    mean = np.array(stat["mean"])
    var  = stat["var"]
    std = np.array(stat['std'])

    plt.switch_backend('agg')
    fig = plt.figure(figsize=(12,9))

    ax1 = fig.add_subplot(111)
    ax1.plot(x, mean, 'g-', alpha=0.75, label='Mean')
    ax1.plot(x, mean-std, 'b-', alpha=0.25)
    ax1.plot(x, mean+std, 'b-', alpha=0.25)
    ax1.fill_between(x, mean-std, mean+std, alpha=0.25, label=r'Mean $\pm$ deviation')
    ax1.set_xlabel(xlabel)
    ax1.set_ylabel(ylabel, color='b')
    ax1.tick_params('y', colors='b')
    ax1.grid()
    ax1.legend()

    ax2 = ax1.twinx()
    ax2.plot(x, var, 'r-', alpha=0.5)
    ax2.set_ylabel('Variance', color='r')
    ax2.tick_params('y', colors='r')
    ax2 = format_exponent(ax2, axis='y')

    plt.title(ftitle)
    fig.savefig(fname)
    plt.close(fig)


# First Sobol indicies
def plot_sobols(x, sobols, params, ftitle, fname):
    plt.switch_backend('agg')
    npar = len(params)

    plt.switch_backend('agg')
    fig = plt.figure(figsize=(12,9))
    ax = fig.add_subplot(111)

    for i in range(npar):
        s = sobols[params[i]]
        ax.plot(x, s, label=params[i])

    ax.set_xlabel(r'$\rho_{tor} ~ [m]$')
    ax.set_ylabel(r'$1^{st} ~ Sobol$')

    ax.set_title(ftitle)
    plt.legend()
    fig.savefig(fname)
    plt.close(fig)
