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


# Statistical Moments (mean +- sdtv)
def plot_stats(x, stat, xlabel, ylabel, ftitle, fname):
    mean = np.array(stat["mean"])
    std = np.array(stat['std'])
    var  = stat["var"]

    plt.switch_backend('agg')
    fig = plt.figure(figsize=(12,9))

    ax1 = fig.add_subplot(111)
    ax1.plot(x, mean, 'b-', alpha=0.6, label='Mean')
    ax1.plot(x, mean-std, 'b-', alpha=0.25)
    ax1.plot(x, mean+std, 'b-', alpha=0.25)
    ax1.fill_between(x, mean-std, mean+std, alpha=0.2, label=r'Mean $\pm$ deviation')
    ax1.set_xlabel(xlabel)
    ax1.set_ylabel(ylabel, color='b')
    ax1.tick_params('y', colors='b')
    ax1.grid()
    ax1.legend()

    ax2 = ax1.twinx()
    ax2.plot(x, var, 'r-', alpha=0.6)
    ax2.set_ylabel('Variance', color='r')
    ax2.tick_params('y', colors='r')
    ax2 = format_exponent(ax2, axis='y')

    plt.title(ftitle)
    fig.savefig(fname)
    plt.close(fig)


# Statistical Moments (using 90% percentils)
def plot_stats_pctl(x, stat, pctl, xlabel, ylabel, ftitle, fname):
    mean = stat["mean"]
    std  = stat["std"]
    p10 = pctl['p10']
    p90 = pctl['p90']

    plt.switch_backend('agg')
    fig = plt.figure(figsize=(16,9))

    ax1 = fig.add_subplot(111)
    ax1.plot(x, mean, 'b-', alpha=0.6, label='Mean')
    ax1.plot(x, p10, 'b-', alpha=0.2)
    ax1.plot(x, p90, 'b-', alpha=0.2)
    ax1.fill_between(x, p10, p90, alpha=0.15, label='90% prediction interval')
    ax1.set_xlabel(xlabel)
    ax1.set_ylabel(ylabel, color='b')
    ax1.tick_params('y', colors='b')
    ax1.grid()
    ax1.legend()

    ax2 = ax1.twinx()
    ax2.plot(x, std, 'r-', alpha=0.6)
    ax2.set_ylabel('Standard deviation', color='r')
    ax2.tick_params('y', colors='r')
    ax2 = format_exponent(ax2, axis='y')

    plt.title(ftitle)
    fig.savefig(fname)
    plt.close(fig)

# Statistical Moments (mean +- sdtv) and p90, p10
def plot_stats_all(x, stat, perc, dist, xlabel, ylabel, ftitle, fname):
    matplotlib.rcParams.update({'font.size': 11})
    mean = np.array(stat["mean"])
    std = np.array(stat['std'])
    p10 = perc['p10']
    p90 = perc['p90']

    plt.switch_backend('agg')
    fig = plt.figure()#figsize=(12,9))

    plt.plot(x, mean, 'b-', label='Mean')
    plt.plot(x, mean-std, 'g-', alpha=0.6, label=r'Mean $\pm$1 std')
    plt.plot(x, mean+std, 'g-', alpha=0.6)
    plt.fill_between(x, mean-std, mean+std, color='g', alpha=0.2)
    plt.plot(x, p10, 'C1-', alpha=0.6, label='10 and 90 percentiles')
    plt.plot(x, p90, 'C1-', alpha=0.6)
    plt.fill_between(x, p10, p90, color='C1', alpha=0.1)
    plt.fill_between(x, [r.lower[0] for r in dist], [r.upper[0] for r in dist], color='b', alpha=0.05)
    plt.xlabel(xlabel)
    plt.ylabel(ylabel)
    plt.grid()
    plt.legend()

    plt.title(ftitle)
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
