import pytest
import numpy as np
import math as m

from da_utils import get_coreprof_ev_acf

#TODO: when running with "pytest ..." da_utils is not found as a package
@pytest.mark.parametrize("func_name,func_params", [
                ('cos', {"omega":2*np.pi/32.}),
        ])
def test_acf(func_name, func_params):

    if func_name == 'cos':
        func = lambda x, w: np.cos(x*w)
        theta = func_params['omega']
        act_ref = 1.*np.pi/theta
        xs = np.linspace(0, 99, 100)
        # ACF computation in case of cos: 
        #   1) normalisation constant decreases with lag
        #   but for lag equal to period convolution is of maximum value
        #   2) the envelope over ACF values does not decrease
        #   3) summation over lags result depends on the phase of finish 

    ys = func(xs, theta)
    # Lags are in integers, dx=1.
    lags_list = [1,2,4,8,16,32,48,64,96,128,160,256,512,1024,2048,4096]
    lags_list = [l for l in lags_list if l < xs[-1]]

    act, acn = get_coreprof_ev_acf(ys.reshape(1,-1), name='test_'+func_name+'_acf', lags=lags_list)
    
    act_check = act[0]
    print('ACT={0} for {1}'.format(act_check, act_ref))
    assert m.isclose(act_check, act_ref)

test_acf('cos', {'omega':2*np.pi/32.})
