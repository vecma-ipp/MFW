import numpy as np
import matplotlib.pylab as plt
from numba import jit

@jit(nopython=True) # Set "nopython" mode for best performance, equivalent to @njit
def randomize(N1, N2, N3, l1, l2, l3, s2, s3, d2, d3, x1=None, x2=None, alpha=None):

    """
The routine attempts to mimic the output from a turbulence code with
an overshoot period followed by a relaxation to a saturated turbulence
state.

Inputs
N1	determines the length of the overshoot region
N2 	determines the length of the standard region
N3 	determines the start of the averaging region
l1 	is the initial value
l2 	is the height of the overshoot
l3 	is the height of the standard region
s2 	determines the size of the Brownian perturbations in the overshoot
	region
s3 	determines the size of the Brownian perturbations in the standard
	region
d2 	is the denominator of the relaxation to the overshoot value
d3 	is the denominator of the relaxation to the standard value
x1 	is the current running mean of the value
x2 	is the current running mean of the value squared
alpha 	is the mixing value of the new value

Outputs
x	a sequence of values meant to mimic the output from a turbulence code
mean 	the mean of x starting from the N3 value
std	the standard deviation of x starting from the N3 value

range 	a pair of integers (as an array) characterising the range where
	the mean and standard deviation was calculated
x1 	is the updated running mean of the value
x2 	is the updated running mean of the value squared
    
"""

    x = np.zeros((N1+N2+1))
    x[0] = l1
    x[1] = l2+s2*(np.random.random()*2-1)
    for i in range(2,N1+1):
      x[i] = x[i-1]+s2*(np.random.random()*2-1)-(x[i-1]-l2)/d2
    for i in range(N1+1,N1+N2+1):
      x[i] = x[i-1]+s3*(np.random.random()*2-1)-(x[i-1]-l3)/d3
    x_mean = x[N3:].mean()
    x_std  = x[N3:].std()
    x_range = np.array([N3,N1+N2])
    if alpha is not None:
        if x1 is not None:
            for xv in x:
                x1 = (1 - alpha) * x1 + alpha * xv
        if x2 is not None:
            for xv in x:
                x2 = (1 - alpha) * x2 + alpha * xv**2
    return x, x_mean, x_std, x_range, x1, x2

def series (alpha, N1, N2, N3, s2, s3, d2, d3):
    """
This routine is used to demonstrate the role of alpha in using ramdomize.

It calculates 1000 blocks determined by N1,N2, N3 ..., plots the results of all of the calls and returns the exponentially averaged values and their square using the specified alpha

Inputs
alpha 	is the mixing value of the new value
N1	determines the length of the overshoot region
N2 	determines the length of the standard region
N3 	determines the start of the averaging region
s2 	determines the size of the Brownian perturbations in the overshoot
	region
s3 	determines the size of the Brownian perturbations in the standard
	region
d2 	is the denominator of the relaxation to the overshoot value
d3 	is the denominator of the relaxation to the standard value

Outputs
x1 	list of the exponentially averaged outputs
x2 	list of the exponentially averaged outputs squared
    
"""
    l1 = np.log10(1)
    l3 = np.log10(1e10)
    l2 = np.log10((10**l3 - 10**l1) * 0.1 + 10**l3)
    x1 = l1
    x2 = l1 ** 2

    N0 = 0

    x1_list = []
    x2_list = []
    plt.clf()
    x, mean, std, xrange, x1, x2 = randomize(N1, N2, N3, l1, l2, l3, s2, s3, d2, d3, x1, x2, alpha)
    fxrange = np.arange(0,len(x))
    plt.plot(N0+fxrange, 10**x,'k', alpha=0.25)
    plt.plot(N0+xrange,[10**(mean-std),10**(mean-std)],'r')
    plt.plot(N0+xrange,[10**mean,10**mean],'b')
    plt.plot(N0+xrange,[10**(mean+std),10**(mean+std)],'g')
    plt.plot(N0+fxrange, x*0+10**x1,'c')

    x1_list.append(x1)
    x2_list.append(x2)

    for i in range(1000):

        N0 += len(x)
        l1 = x1
        l2 = np.log10((10**l3 - 10**l1) * 0.1 + 10**l3)
        x, mean, std, xrange, x1, x2 = randomize(N1, N2, N3, l1, l2, l3, s2, s3, d2, d3, x1, x2, alpha)
        plt.plot(N0+fxrange, 10**x,'k', alpha=0.25)
        plt.plot(N0+xrange,[10**(mean-std),10**(mean-std)],'r')
        plt.plot(N0+xrange,[10**mean,10**mean],'b')
        plt.plot(N0+xrange,[10**(mean+std),10**(mean+std)],'g')
        plt.plot(N0+fxrange, x*0+10**x1,'c')
        plt.plot(N0+fxrange, x*0+10**(x1+np.sqrt(x2-x1**2)),'c')
        plt.plot(N0+fxrange, x*0+10**(x1-np.sqrt(x2-x1**2)),'c')
        
        x1_list.append(x1)
        x2_list.append(x2)

    plt.title('alpha=%s, N1=%s, N2=%s, N3=%s, s2=%s, s3=%s, d2=%s, d3=%s' % (alpha, N1, N2, N3, s2, s3, d2, d3))
    plt.yscale('log')
    return x1_list, x2_list
