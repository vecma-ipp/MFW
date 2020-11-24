import sys
import os
#TODO: make a new package + install / or get relative paths consistent
sys.path.append(os.path.abspath("../../standalone/src/custom_codes/gem0"))
import importlib.util
spec = importlib.util.spec_from_file_location("gem0_singleton", os.path.abspath("../../standalone/src/custom_codes/gem0/gem0_singleton.py"))
gem0_singleton = importlib.util.module_from_spec(spec)
spec.loader.exec_module(gem0_singleton)
from gem0_singleton import GEM0Singleton

class ExtCodeHelper():

    def __init__(self):
        self.gem0obj = GEM0Singleton()

    def gem0_call_tefltevltegrad(x): # TODO np.vectorize?
        """
        :param x: x is a 1D array; x[0] is Te, x[1] is gradTe 
        """
        return gem0obj.gem0_call({'te.value': x[0], 'te.ddrho': x[1]})[0]

    def gem0_call_teflteval_array(x):
        res = []
        for el in x: 
            res.append([gem0obj.gem0_call({'te.value': el[0]})[0]])
        return np.array(res)

    def gem0_call_teflteval_log_array(x): 
        res = []
        for el in x: 
            res.append([math.log(gem0obj.gem0_call({'te.value': el[0]})[0])]) #TODO actually negative values
        return np.array(res)

    def gem0_call_tefltegrad_array(self, x):
        res = []
        for el in x: 
            res.append([self.gem0obj.gem0_call({'te.ddrho': el[0]})[0]])
        return np.array(res)

    def gem0_call_tefltevltegrad_array(self, x): # TODO np.vectorize?
        """
        calls the gem0 code for desired te.valus and te.ddrho
        :param x: x[0] is desired tevalue, x[1] is desired tegrad
        """
        res = []
        for el in x:
            res.append(self.gem0obj.gem0_call({'te.value': el[0], 'te.ddrho': el[1]})[0])
        return res

    def gem0_call_tefltevltivl_array(self, x):
        """
        calls the gem0 code for desired te.valus and te.ddrho
        :param x: x[0] is desired tevalue, x[1] is desired tegrad
        """
        res = []
        for el in x:
            res.append(self.gem0obj.gem0_call({'te.value': el[0], 'ti.value': el[1]})[0])
        return res