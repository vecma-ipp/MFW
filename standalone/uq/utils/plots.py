import matplotlib.pylab as plt
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

# Statistical Moments
def plot_stats_bis(x, stat, pctl, xlabel, ylabel, ftitle, fname=None):
    mean = stat["mean"]
    var  = stat["var"]
    p10 = pctl['p10']
    p90 = pctl['p90']

    plt.switch_backend('agg')
    fig = plt.figure(figsize=(12,9))

    ax1 = fig.add_subplot(111)
    ax1.plot(x, mean, 'g-', alpha=0.75, label='Mean')
    ax1.plot(x, p10, 'b-', alpha=0.25)
    ax1.plot(x, p90, 'b-', alpha=0.25)
    ax1.fill_between(x, p10, p90, alpha=0.25, label='90% prediction interval')
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

    if fig is None:
        plt.show()
    else:
        fig.savefig(fname)
        plt.close(fig)

    #plt.close()

# Statistical Moments (+- deviation)
def plot_stats(x, stat, xlabel, ylabel, ftitle, fname=None):
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

    if fig is None:
        plt.show()
    else:
        fig.savefig(fname)
        plt.close(fig)

    #plt.close()

# TODO: generic plots (here for 4 params)
def plot_sobols_4(x, sobols, params):
    plt.switch_backend('agg')

    s1 = sobols[params[0]]
    s2 = sobols[params[1]]
    s3 = sobols[params[2]]
    s4 = sobols[params[3]]

    fig, axs = plt.subplots(nrows=2, ncols=2, sharex=True)

    ax = axs[0,0]
    ax.plot(x, s1)
    ax.set_title('D1')

    #ax.locator_params(nbins=4)

    ax = axs[0,1]
    ax.plot(x, s2)
    ax.set_title('D2')

    ax = axs[1,0]
    ax.plot(x, s3)
    ax.set_title('D3')

    ax = axs[1,1]
    ax.plot(x, s4)
    ax.set_title('D4')

    fig.suptitle('First-Order Sobol indices')
    fig.savefig('sobols.png')
    plt.close(fig)

def plot_sobols(x, sobols, params):
    plt.switch_backend('agg')

    s1 = sobols[params[0]]
    s2 = sobols[params[1]]

    fig, axs = plt.subplots(nrows=1, ncols=2, sharex=True)

    ax = axs[0]
    ax.plot(x, s1)
    ax.set_title(r'$T_e(\||\rho_{tor}\||=0.)$')

    #ax.locator_params(nbins=4)

    ax = axs[1]
    ax.plot(x, s2)
    ax.set_title(r'$T_e(\||\rho_{tor}\||=1.)$')

    fig.suptitle('First-Order Sobol indices')
    fig.savefig('loop_sobols.png')
    plt.close(fig)

    #plt.show()

## Correlation matrix
#if __corr:
#    fig3 = plt.figure()
#    ax3  = fig3.add_subplot(111)
#    ax3.imshow(corr, cmap=plt.cm.jet)
#    ax3.colorbar()
#    ax3.title('Corrolation matrix)')

# QoI distribution, in the index grid i
def plot_dist(i, dist, rho, mean, std):
    r  = rho[i]
    m = mean[i]
    sd = std[i]

    samples = np.linspace(m-3*std, m+3*std, 200)
    plt.plot(samples, dist[i].pdf(samples), 'b-')

    plt.axvline(x=m, color= 'C1', linestyle='-')
    plt.axvline(x=m-sd, color= 'C1', linestyle='--')
    plt.axvline(x=m+sd, color= 'C1', linestyle='--')

    plt.title(r'Output distribution: pressure in $\rho = $'+str(r))
    plt.xlabel('p')
    plt.ylabel('p_dist')
    plt.grid()
    plt.show()
