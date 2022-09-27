import sys
import os
import numpy as np
#TODO: make a new package + install / or get relative paths consistent
sys.path.append(os.path.join(os.getcwd(), "standalone/src/custom_codes/gem0"))
import importlib.util  # TODO windows path does not understand '..' notation
spec = importlib.util.spec_from_file_location("gem0_singleton", os.path.abspath("../../../MFW/standalone/src/custom_codes/gem0/gem0_singleton.py"))
#spec = importlib.util.spec_from_file_location("gem0_singleton", os.path.abspath("C:/Users/user/Documents/UNI/MPIPP/PHD/code/MFW/standalone/src/custom_codes/gem0/gem0_singleton.py"))
gem0_singleton = importlib.util.module_from_spec(spec)
spec.loader.exec_module(gem0_singleton)
from gem0_singleton import GEM0Singleton

class ExtCodeHelper():

    def __init__(self, option=2):
        if option not in [1, 2]:
            option = 2
        self.gem0obj = GEM0Singleton(option)

    def gem0_call_tefltevltegrad(self, x): # TODO np.vectorize?
        """
        :param x: x is a 1D array; x[0] is Te, x[1] is gradTe 
        """
        return gem0obj.gem0_call({'te.value': x[0], 'te.ddrho': x[1]})[0]

    def gem0_call_teflteval_array(self, x):
        res = []
        for el in x: 
            res.append([self.gem0obj.gem0_call({'te.value': el[0]})[0]])
        return np.array(res)

    def gem0_call_teflteval_log_array(self, x):
        res = []
        for el in x: 
            res.append([np.log(self.gem0obj.gem0_call({'te.value': el[0]})[0])]) #TODO actually negative values
        return np.array(res)

    def gem0_call_tefltegrad_array(self, x):
        res = []
        for el in x:
            res.append([self.gem0obj.gem0_call({'te.ddrho': el})[0]])
        return np.array(res)

    def gem0_call_tefltigrad_array(self, x):
        res = []
        for el in x:
            res.append([self.gem0obj.gem0_call({'ti.ddrho': el})[0]])
        return np.array(res)

    def gem0_call_tifltigrad_array(self, x):
        res = []
        for el in x:
            res.append([self.gem0obj.gem0_call({'ti.ddrho': el})[0]])
        return np.array(res)

    def gem0_call_tefltevltegrad_array(self, x):  # TODO np.vectorize?
        """
        calls the gem0 code for desired te.valus and te.ddrho
        :param x: x[0] is desired tevalue, x[1] is desired tegrad
        """
        res = []
        for el in x:
            res.append([self.gem0obj.gem0_call({'te.value': el[0], 'te.ddrho': el[1]})[0]])
        return res

    def gem0_call_tefltevltivl_array(self, x):
        """
        calls the gem0 code for desired te.valus and te.ddrho
        :param x: x[0] is desired tevalue, x[1] is desired tegrad
        """
        res = []
        for el in x:
            res.append([self.gem0obj.gem0_call({'te.value': el[0], 'ti.value': el[1]})[0]])
        return res

    def gem0_call_tifltigrad_array(self, x):
        """
        calls the gem0 code for desired ti.ddrho
        :param x: x[0] is ti.ddrho
        """
        res = []
        for el in x:
            res.append([self.gem0obj.gem0_call({'ti.ddrho': el})[1]])  #question: why tiddrho is different from teddrho in cpo? is tiddrho is converted to array by acii api?
        return np.array(res)

    def gem0_call_tefltegradtigrad_array(self, x):
        """
        calls the gem0 code for desired te.ddrho and ti.ddrho
        :param x: x[0] is te.ddrho, x[1] is ti.ddrho
        """
        res = []
        for el in x:
            res.append([self.gem0obj.gem0_call({'te.ddrho': el[0], 'ti.ddrho': el[1]})[0]])
        return np.array(res)

    def gem0_call_tifltegradtigrad_array(self, x):
        """
        alls the gem0 code for desired te.ddrho and ti.ddrho
        :param x: x[0] is te.ddrho, x[1] is ti.ddrho
        """
        res = []
        for el in x:
            res.append([self.gem0obj.gem0_call({'te.ddrho': el[0], 'ti.ddrho': el[1]})[1]])  #question: why tiddrho is different from teddrho in cpo? is tiddrho is converted to array by acii api?
        return np.array(res)

    def gem0_call_tifltivltigrad_array(self, x):
        """
        calls the gem0 code for desired ti.val and ti.ddrho
        :param x: x[0] is ti.value x[1] is ti.ddrho
        """
        res = []
        for el in x:
            res.append([self.gem0obj.gem0_call({'ti.value': el[0], 'ti.ddrho': el[1]})[1]])
        return np.array(res)


    def gem0_call_4param2target_array(self, x):
        """
        calls the gem0 code for desired ti.ddrho
        :param x: x[0] is te.value
        """
        res = []
        for el in x:
            res.append([self.gem0obj.gem0_call({'te.value': el[0], 'ti.value': el[1],
                                                'te.ddrho': el[2], 'ti.ddrho': el[3]})[0:2]])
        return np.array(res)

    def gem0_call_tefl4params_array(self, x):
        """
        calls the gem0 code for desired ti.ddrho
        :param x: x[0] is te.value
        """
        res = []
        for el in x:
            res.append([self.gem0obj.gem0_call({'te.value': el[0], 'ti.value': el[1],
                                                'te.ddrho': el[2], 'ti.ddrho': el[3]})[0]])
        return np.array(res)



