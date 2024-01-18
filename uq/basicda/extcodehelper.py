import sys
import os
import numpy as np

import importlib.util  # TODO windows path does not understand '..' notation

#TODO: make a new package + install / or get relative paths consistent
sys.path.append(os.path.join(os.getcwd(), "standalone/src/custom_codes/gem0"))
#gem0path = "C:/Users/user/Documents/UNI/MPIPP/PHD/code/MFW/standalone/src/custom_codes/gem0/gem0_singleton.py"
gem0path = os.path.abspath("../standalone/src/custom_codes/gem0/gem0_singleton.py")
spec = importlib.util.spec_from_file_location("gem0_singleton", os.path.abspath(gem0path))
gem0_singleton = importlib.util.module_from_spec(spec)
spec.loader.exec_module(gem0_singleton)

from gem0_singleton import GEM0Singleton

class ExtCodeHelper():

    def __init__(self, option=2., **kwargs):
        
        if option not in [1, 2]:
            option = 2
        
        xml_file = kwargs['xml_file'] if 'xml_file' in kwargs else 'gem0.xml'

        self.gem0obj = GEM0Singleton(option, xml_file=xml_file)

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


    def gem0_call_tefl4params_array(self, x, rho_inds=[69]):
        """
        calls the gem0 code for desired 4 parameters
        :param x: x[0] is te.value
        """
        res = []
        for el in x:
            res.append([self.gem0obj.gem0_call({'te.value': el[0], 'ti.value': el[1],
                                                'te.ddrho': el[2], 'ti.ddrho': el[3]},
                                        rho_inds=rho_inds)[0]])
        return np.array(res)


    def gem0_call_4param2target_array(self, x, rho_inds=[69], rho=None):
        """
        calls the gem0 code for desired 4 parameters
        :param x: x[0] is te.value
        """
        res = []
        inputs_new = []
        for el in x:

            fluxes, input = self.gem0obj.gem0_call({'te.value': el[0], 'ti.value': el[1],
                                                'te.ddrho': el[2], 'ti.ddrho': el[3]},
                                        rho_inds=rho_inds, rho=rho)[0:2]
            res.append([fluxes],)
            inputs_new.append([input],)

        #print(f"from gem0_call_4param2target_array we are returning:{np.array(res)}, {np.array(inputs_new)}") ###DEBUG

        return np.array(res), np.array(inputs_new)

    def gem0_call_4param2target_cpo(self, equilibrium, coreprof, coretransp, params=None):
        """
        Takes equilibrium, coreprof, coretransp CPO datastructures, XML params and 
        returns np.array(2,#flux-tubes) with te_transp and ti_transp fluxes 
        """

        res, input = self.gem0obj.gem0_call_cpo(equilibrium=equilibrium, coreprof=coreprof, coretransp=coretransp, params=params)

        res = np.array(res)
        input = np.array(input)

        return res, input
    