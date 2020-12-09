#import importlib.util
#spec = importlib.util.spec_from_file_location("gem0", "/marconi/home/userexternal/yyudin00/code/MFW/standalone/src/custom_codes/gem0/gem0.py")
#gem0 = importlib.util.module_from_spec(spec)
#spec.loader.exec_module(gem0)
from gem0 import gem

from ascii_cpo import read, write

from ual.coreprof import coreprof
from ual.coretransp import coretransp
from ual.equilibrium import equilibrium

from assign_turb_parameters import assign_turb_parameters

import xml.etree.ElementTree as ET
#from easymfw.templates.xml_element import XMLElement
from easymfw.templates.cpo_element import CPOElement

def get_code_params(xml_file_name='gem0.xml'): #TODO check if fill_param() does exactly the same as parsing

    print ("> Get code params")
    # fill_param(code_parameters, xml_file_name, '', 'gem0.xsd')
    #code_parameters = XMLElement(xml_file_name)
    code_parameters, _ = assign_turb_parameters(xml_file_name)

    return code_parameters

def get_code_ios(equil_file_in="gem0_equilibrium_in.cpo", corep_file_in="gem0_coreprof_in.cpo", coret_file_out="gem0_coretransp_out.cpo"):
    #coret = {}
    ios = 0

    # Read CPO file and write structures
    equil = read(equil_file_in, "equilibrium")
    #corep = read(corep_file_in, "coreprof")
    #coret = read(coret_file_out, "coretransp")
    coret = coretransp()

    corep_elem = CPOElement(corep_file_in, "coreprof")

    return equil, corep_elem, coret

class GEM0Singleton():

    def __init__(self, option=4):
        self.equil_file_in = "gem0_equilibrium_in.cpo"
        self.corep_file_in ="gem0_coreprof_in.cpo"
        self.coret_file_out = "gem0_coretransp_out.cpo"

        self.code_parameters = get_code_params()
        self.code_parameters['chigb_option'] = option
        self.equil, self.corep_elem, self.coret = get_code_ios(self.equil_file_in, self.corep_file_in, self.coret_file_out)
        
        self.code_parameters["grid.nion"] = 1 #TODO: workaround

    def get_curr_params(self, ft=69):

        return self.corep_elem.get_value('te.value')[ft], self.corep_elem.get_value('ti.value')[ft], \
               self.corep_elem.get_value('te.ddrho')[ft], self.corep_elem.get_value('ti.ddrho')[ft]

    def modify_code_ios(self, attrib, new_value, ft=[69]):
        """
        Modifies a gem0 coreprof cpo object for a single parameter
        """
        old_value = self.corep_elem.get_value(attrib)  

        values = []
        indices = []
        ind = ft[0] # TODO: workaround for single flux tube
        if attrib in ['te.value', 'te.ddrho', 'ti.value', 'ti.ddrho']:
            rho = self.corep_elem.get_value('rho_tor_norm')
            if attrib == 'te.ddrho':
                t = self.corep_elem.get_value('te.value')
                t_i  = t[ind]
                dt_i = new_value
                self.corep_elem.set_value('te.ddrho', [new_value], [ind])
            if attrib == 'te.value':
                dt = self.corep_elem.get_value('te.ddrho')
                t_i  = new_value
                dt_i = dt[ind]
                values.append(new_value)
                indices.append(ind)
            if attrib == 'ti.ddrho':
                t = self.corep_elem.get_value('te.value')
                if self.code_parameters["grid.nion"] == 1:
                    t_i  = t[ind]
                else:
                    t_i  = t[ind][0]
                dt_i = new_value
                self.corep_elem.set_value('ti.ddrho', [new_value], [ind])
            if attrib == 'ti.value':
                dt = self.corep_elem.get_value('ti.ddrho')
                t_i  = new_value
                if self.code_parameters["grid.nion"] == 1:
                    dt_i = dt[ind]
                else:
                    dt_i = dt[ind][0]
                values.append(new_value)
                indices.append(ind)

            # if profile: neighbors to update
            values.append(dt_i*(rho[ind-2] - rho[ind]) + t_i)
            values.append(dt_i*(rho[ind-1] - rho[ind]) + t_i)
            values.append(dt_i*(rho[ind+1] - rho[ind]) + t_i)
            values.append(dt_i*(rho[ind+2] - rho[ind]) + t_i)
            indices += [ind-2, ind-1, ind+1, ind+2]
            if attrib in ['te.value', 'te.ddrho']:
                self.corep_elem.set_value('te.value', values, indices)
            if attrib in ['ti.value', 'ti.ddrho']:
                self.corep_elem.set_value('ti.value', values, indices)

        #self.corep_elem.set_value(attrib, [new_value], ft)

        # TODO: for gradients: read the coreprof cpo-s, get the gradient by interpolation
        # for new inputs re-wrtie gradients at cpo, always write the new gradients?
        # move to ONLY modifying gradients i.e. temaparature has to be interpolated?
        
        #return self.corep_elem

    def modify_code_params(self, attrib, value):
        self.code_parameters[attrib] = value
        #return self.code_parameters

    def gem0_call(self, param):
        for k, v in param.items():
            self.modify_code_ios(k, v)
        coret, tefl, tifl, tedr, tidr = gem(self.equil, self.corep_elem.core, self.coret, self.code_parameters)
        return tefl, tifl, tedr, tidr

    def gem0_fit_call(xs, thresh, beta_reduction, etae_pinch, chi_d, chiratio_phi):

        # change the (free model) parameters
        params_new = {'beta_reduction': beta_reduction, 'etae_pinch': etae_pinch, 
                    'chi_d': etae_pinch, 'chiratio_phi': chiratio_phi}
        for k, v in params_new.items():
            self.code_parameters = modify_code_params(code_parameters, k, v)

        #change the values at spo (inputs)
        Xlabels = ['ti.value', 'te.value', 'ti.ddrho', 'ti.ddrho']
        for xl in Xlabels:
            self.corep_elem = modify_code_ios(corep_elem, xl, xs)

        coret, tefl, tifl, tedr, tidr = gem(equil, corep_elem.core, coret, code_parameters)
        return [tefl, tifl, tedr, tidr]

    def gem0_test(self,):
        """
        runs a gem0 python replacement as a standalone program
        perfroms inputs, outputs and code run
        """
        #corep_elem = modify_code_ios(corep_elem, 'ti.value', 0)

        print ("> Run gem0 routine")
        coret, tefl, tifl, tedr, tidr = gem(self.equil, self.corep_elem.core, self.coret, self.code_parameters)
        print('ti_transp_flux is: {}'.format(tifl))

        # Transfer CPO to buffer / write file
        print("> Writing transport cpo")
        write(coret, self.coret_file_out, 'coretransp')

