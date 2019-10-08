from memory_profiler import profile
import chaospy as cp
import numpy   as np


Cor = [[ 1.,          0.99793468, -0.94606383,  0.77531377, -0.79596346]
     [ 0.99793468,  1.,         -0.96217442,  0.80346325, -0.8166447 ]
     [-0.94606383, -0.96217442,  1.,         -0.9180122,   0.91320696]
     [ 0.77531377,  0.80346325, -0.9180122,   1.,         -0.93349271]
     [-0.79596346, -0.8166447,   0.91320696, -0.93349271,  1.        ]]

Cov = [[18798.72960368, 16766.71123094, -9197.07042708,  3104.4031156, -1121.03305667]
       [16766.71123094, 15016.30349987, -8359.88201176,  2875.30483846, -1027.95878331]
       [-9197.07042708, -8359.88201176,  5027.24026345, -1900.85781434, 665.11243905]
       [3104.4031156,   2875.30483846, -1900.85781434,   852.85013173,   -280.03243112]
       [-1121.03305667 -1027.95878331,   665.11243905,  -280.03243112,  105.51691259]]



# QoI distribution, in the index grid i
def plot_dist(dist, stat):
    plt.switch_backend('agg')
    m = np.array(stat["mean"])
    sd = np.array(stat['std'])

    fig, axs = plt.subplots(nrows=2, ncols=2, sharex=True, figsize=(12,9))

    #fig = plt.figure(figsize=(12,9))
    #ax1 = fig.add_subplot(111)
    j = 0
    for i in range(4):
        s = np.linspace(m[j]-3*sd[j], m[j]+3*sd[j], 100)
        d = dist[j].pdf(s)

        ax = axs[i//2, i%2]
        ax.plot(s, d, 'b-')
        ax.axvline(x=m[j], color= 'C1', linestyle='-')
        ax.axvline(x=m[j]-sd[j], color= 'C1', linestyle='--')
        ax.axvline(x=m[j]+sd[j], color= 'C1', linestyle='--')
        ax.set_title(r'dist in: $C_'+str(j)+'$')
        ax.grid()
        j = i+2

    fig.suptitle('Output distiburions')
    fig.savefig('dist_out.png')
    plt.close(fig)

#def plot_dist01(dist, stat):
#    plt.switch_backend('agg')
#    m = np.array(stat["mean"])
#    sd = np.array(stat['std'])
#
#    fig = plt.figure(figsize=(12,9))
#    ax = fig.add_subplot(111)
#
#    s0 = np.linspace(m[0]-3*sd[0], m[0]+3*sd[0], 100)
#    d0 = dist[0].pdf(s0)
#    ax.plot(s0, d0, label=r'$C_0$')
#
#    s1 = np.linspace(m[1]-3*sd[1], m[1]+3*sd[1], 100)
#    d1 = dist[1].pdf(s1)
#    ax.plot(s1, d1, label=r'$C_1$')
#
#    ax.set_title(r'dist in: $C_0$ and $C_1$')
#    ax.grid()
#
#    ax.legend()
#
#    fig.savefig('dist_out01.png')
#    plt.close(fig)
