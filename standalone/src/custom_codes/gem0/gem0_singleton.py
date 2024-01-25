import os, sys
import importlib.util

import bisect

# -- load pyGEM0 code files - via importlib
# #gem0path="/marconi/home/userexternal/yyudin00/code/MFW/standalone/src/custom_codes/gem0/gem0.py"
# #gem0path="C:/Users/user/Documents/UNI/MPIPP/PHD/code/MFW/standalone/src/custom_codes/gem0/gem0.py"
# gem0path="../standalone/src/custom_codes/gem0/gem0.py"
gem0path = "/u/yyudin/code/MFW/standalone/src/custom_codes/gem0/gem0.py"
spec = importlib.util.spec_from_file_location("gem0", os.path.abspath(gem0path))
gem0 = importlib.util.module_from_spec(spec)
spec.loader.exec_module(gem0)

# -- load pyGEM0 code files - via sys
#sys.path.append('c:\\Users\\user\\Documents\\UNI\\MPIPP\\PHD\\code\\MFW\\ual\\')
sys.path.append(gem0path)

from gem0 import gem

from assign_turb_parameters import assign_turb_parameters

from ascii_cpo import read, write

from ual.coreprof import coreprof
from ual.coretransp import coretransp
from ual.equilibrium import equilibrium

import xml.etree.ElementTree as ET

from base.cpo_element import CPOElement
#from easymfw.templates.xml_element import XMLElement
#from easymfw.templates.cpo_element import CPOElement

def get_code_params(xml_file_name='gem0.xml'): #TODO check if fill_param() does exactly the same as parsing

    #print("> Get code params")

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

    def __init__(self, option=4., **kwargs):
        
        self.equil_file_in = kwargs['equilibrium'] if 'equilibrium' in kwargs else "gem0_equilibrium_in.cpo"
        self.corep_file_in = kwargs['coreprof'] if 'coreprof' in kwargs else "gem0_coreprof_in.cpo"
        self.coret_file_out = kwargs['coretransp'] if 'coretransp' in kwargs else "gem0_coretransp_out.cpo"

        self.xml_file = kwargs.get('xml_file') if 'xml_file' in kwargs else 'gem0.xml'

        self.code_parameters = get_code_params(xml_file_name=self.xml_file)
        self.code_parameters['chigb_option'] = option
        self.equil, self.corep_elem, self.coret = get_code_ios(self.equil_file_in, self.corep_file_in, self.coret_file_out)
        
        self.code_parameters["grid.nion"] = 1 #TODO: workaround

    def get_curr_params(self, ft=69):

        return self.corep_elem.get_value('te.value')[ft], self.corep_elem.get_value('ti.value')[ft], \
               self.corep_elem.get_value('te.ddrho')[ft], self.corep_elem.get_value('ti.ddrho')[ft]

    def update_gradients(self, name, value, i_ft=69):
        rho = self.corep_elem.get_value('rho_tor_norm')
        i = i_ft

        values = []
        indices = []
        if name == 'te.value':
            dt = self.corep_elem.get_value('te.ddrho')
            t_i = value
            dt_i = dt[i]
            values.append(value)
            indices.append(i)
        if name == 'te.ddrho':
            t = self.corep_elem.get_value('te.value')
            t_i = t[i]
            dt_i = value
            self.corep_elem.set_value('te.ddrho', [value], [i])
        if name == 'ti.value':
            dt = self.corep_elem.get_value('ti.ddrho')
            t_i = value
            if self.code_parameters['grid.nion'] == 1:
                dt_i = dt[i]
            else:
                dt_i = dt[i][0]
            values.append(value)
            indices.append(i)
        if name == 'ti.ddrho':
            t = self.corep_elem.get_value('ti.value')
            if self.code_parameters['grid.nion'] == 1:
                t_i = t[i]
            else:
                t_i = t[i][0]
            dt_i = value
            self.corep_elem.set_value('ti.ddrho', [value], [i])

        # neighbors to update
        values.append(dt_i*(rho[i-2] - rho[i]) + t_i)
        values.append(dt_i*(rho[i-1] - rho[i]) + t_i)
        values.append(dt_i*(rho[i+1] - rho[i]) + t_i)
        values.append(dt_i*(rho[i+2] - rho[i]) + t_i)
        indices += [i-2, i-1, i+1, i+2]
        if name[0:2] == 'te':
            self.corep_elem.set_value('te.value', values, indices)
        if name[0:2] == 'ti':
            self.corep_elem.set_value('ti.value', values, indices)

    def update_values_old(self, attrib, new_value, ft=[69]):
        """
              Modifies a gem0 coreprof cpo object for a single parameter
              """
        old_value = self.corep_elem.get_value(attrib)

        values = []
        indices = []
        ind = ft[0]  # TODO: workaround for single flux tube
        if attrib in ['te.value', 'te.ddrho', 'ti.value', 'ti.ddrho']:
            rho = self.corep_elem.get_value('rho_tor_norm')
            if attrib == 'te.ddrho':
                t = self.corep_elem.get_value('te.value')
                t_i = t[ind]
                dt_i = new_value
                self.corep_elem.set_value('te.ddrho', [new_value], [ind])
            if attrib == 'te.value':
                dt = self.corep_elem.get_value('te.ddrho')
                t_i = new_value
                dt_i = dt[ind]
                values.append(new_value)
                indices.append(ind)
            if attrib == 'ti.ddrho':
                t = self.corep_elem.get_value('te.value')
                if self.code_parameters["grid.nion"] == 1:
                    t_i = t[ind]
                else:
                    t_i = t[ind][0]
                dt_i = new_value
                self.corep_elem.set_value('ti.ddrho', [new_value], [ind])
            if attrib == 'ti.value':
                dt = self.corep_elem.get_value('ti.ddrho')
                t_i = new_value
                if self.code_parameters["grid.nion"] == 1:
                    dt_i = dt[ind]
                else:
                    dt_i = dt[ind][0]
                values.append(new_value)
                indices.append(ind)

            # if profile: neighbors to update
            values.append(dt_i * (rho[ind - 2] - rho[ind]) + t_i)
            values.append(dt_i * (rho[ind - 1] - rho[ind]) + t_i)
            values.append(dt_i * (rho[ind + 1] - rho[ind]) + t_i)
            values.append(dt_i * (rho[ind + 2] - rho[ind]) + t_i)
            indices += [ind - 2, ind - 1, ind + 1, ind + 2]
            if attrib in ['te.value', 'te.ddrho']:
                self.corep_elem.set_value('te.value', values, indices)
            if attrib in ['ti.value', 'ti.ddrho']:
                self.corep_elem.set_value('ti.value', values, indices)

        # self.corep_elem.set_value(attrib, [new_value], ft)

        # TODO: for gradients: read the coreprof cpo-s, get the gradient by interpolation
        # for new inputs re-write gradients at cpo, always write the new gradients?
        # move to ONLY modifying gradients i.e. temperature has to be interpolated?

        # return self.corep_elem

    def update_values_coretransp(self, value_dict, rho_tor_norm):
        """
        - Gets a dictionary of values of profile at a point at rho_tor_norm value (at coretransp grid)
        - A profile and its gradient should always be passed together
        - Finds the closest six points at original (coreprof) grid (three from each side) and substitutes with linear profile 
            such that values and gradient at passed point are exact
        """

        rho_grid_corep = self.corep_elem.get_value('rho_tor_norm')

        quantities = ['t']
        speciess = ['i', 'e']
        attributes = ['value', 'ddrho']

        # indices around the pivot index to extrapolate to
        delta_ind = [-3,-2,-1,0,1,2]
   
        # Find indices of neibouring points ar coreprof grid
        #  - The first point at coreprof grid outer than rho_tor_norm
        i_coreprof = 0
        
        # for i in range(len(rho_grid_corep)):
        #     if rho_grid_corep[i] >= rho_tor_norm:
        #         i_coreprof = i
        #         break

        # Use bicection #TODO test
        i_coreprof = bisect.bisect_right(rho_grid_corep, rho_tor_norm)

        #print(f"> pivot index at coreprof gird: {i_coreprof}") ###DEBUG
        # Extrapolate the values
        for quantity in quantities:
            for species in speciess:
                if f"{quantity}{species}_value" in value_dict:

                    q_value = value_dict[f"{quantity}{species}_value"]
                    q_ddrho = value_dict[f"{quantity}{species}_ddrho"]

                    # fill in the values and indices around the pivot index
                    values = []
                    for d_i in delta_ind:
                        # linear extrapolation using value and gradient at the point, and distance to the new point
                        values.append(q_ddrho*(rho_grid_corep[i_coreprof+d_i] - rho_tor_norm) + q_value)
                    
                    indices = [i_coreprof+d_i for d_i in delta_ind]

                    self.corep_elem.set_value(f"{quantity}{species}.value", values, indices)
                    self.corep_elem.set_value(f"{quantity}{species}.ddrho", [q_ddrho]*len(indices), indices)

        return 0

    def modify_code_ios(self, attrib, new_value, ft=[69]):
        self.update_gradients(attrib, new_value, i_ft=ft[0])

    def modify_code_params(self, attrib, value):
        if attrib == 'ra0':
            self.code_parameters['grid.ra0'] = value
        else:
            self.code_parameters['physical.' + attrib] = value
        # TODO: make dictionary of parameter categories
        #return self.code_parameters

    def gem0_call(self, param, rho_inds=[69], rho=None):
        for k, v in param.items():
            # the modification of profile itself does no account for rho value
            self.modify_code_ios(k, v, ft=rho_inds)
        if rho is not None:
            self.modify_code_params('ra0', rho)
        else:
            self.modify_code_params('ra0', (rho_inds[0]+0)/100.0) # not accurate?    

        coret, tefl, tifl, tedr, tidr, te, ti, rho_new = gem(self.equil, self.corep_elem.core, self.coret, self.code_parameters)
        return [tefl, tifl], [te, ti, tedr, tidr]

    def gem0_call_profile(self, coreprof):
        """
        calls GEM0 when four core profile are known
            coreprof: dictionary, each element is array of length nrho (usually 100)
            returns: triplet of (fluxes, inputs, coretransp) where fluxes is list of Qe/i, inputs is list of Te/i,gradTe/i, and coretransp is CPO object
        """

        for k,prof in coreprof.items():

            # if k == 'te_value':
            #     self.corep_elem.te.value = prof
            # if k == 'ti_value':
            #     self.corep_elem.ti.value = prof
            # if k == 'te_ddrho':
            #     self.corep_elem.te.ddrho = prof
            # if k == 'ti_ddrho':
            #     self.corep_elem.ti.ddrho = prof
            # else:
            
            nrho = len(prof)
            inds = [i for i in range(nrho)]
            profname = k.split('_')[0]
            attribname = k.split('_')[-1]
            self.corep_elem.set_value(f"{profname}.{attribname}", prof.tolist(), inds)

            #print(f"> core profile of {k} is not supported!")

        coret, tefl, tifl, tedr, tidr, te, ti, rho_new = gem(self.equil, self.corep_elem.core, self.coret, self.code_parameters)

        te_transp_flux = coret.values[0].te_transp.flux[:]
        ti_transp_flux = coret.values[0].ti_transp.flux[:,0]

        return [te_transp_flux, ti_transp_flux], [te, ti, tedr, tidr], coret

    def gem0_call_coretransp(self, coreprof, rho_tor_norm):
        """
        - Gets a dictionary of values of profile P at a point R at rho_tor_norm value (at coretransp grid)
        - Retruns Qe/i(<P_i(R)>) : flux values as a function of profiles at the given rho_tor_norm value
        """

        self.update_values_coretransp(value_dict=coreprof, rho_tor_norm=rho_tor_norm)

        coret, tefl, tifl, tedr, tidr, te, ti, rho_new = gem(self.equil, self.corep_elem.core, self.coret, self.code_parameters)

        te_transp_flux = coret.values[0].te_transp.flux[:]
        ti_transp_flux = coret.values[0].ti_transp.flux[:,0]

        return [te_transp_flux, ti_transp_flux], [te, ti, tedr, tidr], coret

    def gem0_fit_call(self,
                      xs,
                      params,
                      ):

        # change the (free model) parameters
        for k, v in params.items():
            self.modify_code_params(k, v)

        # change the values at cpo (inputs)
        Xlabels = ['te.value', 'te.ddrho', 'ti.ddrho', 'ti.value']
        y_res = []

        for x in xs:
            x_dict = x
            for feat in [feat for feat in Xlabels if feat in x_dict.keys()]:
            #for k, v in x_dict.items():
                #self.modify_code_ios(k, v)
                self.modify_code_ios(feat, x_dict[feat])

            coret, tefl, tifl, tedr, tidr, te, ti, rho_new = gem(self.equil, self.corep_elem.core, self.coret, self.code_parameters)
            y_res.append({'te.transp.flux': tefl, 'ti.transp.flux': tifl})

        return y_res

    def gem0_test(self,):
        """
        runs a gem0 python replacement as a standalone program
        perfroms inputs, outputs and code run
        """
        #corep_elem = modify_code_ios(corep_elem, 'ti.value', 0)

        print("> Run gem0 routine")
        coret, tefl, tifl, tedr, tidr, te, ti, rho_new = gem(self.equil, self.corep_elem.core, self.coret, self.code_parameters)
        print('ti_transp_flux is: {}'.format(tifl))

        # Transfer CPO to buffer / write file
        print("> Writing transport cpo")
        write(coret, self.coret_file_out, 'coretransp')

    def gem0_call_cpo(self, equilibrium=None, coreprof=None, coretransp=None, params=None):
        """
        calls the gem0 code for desired te.value, te.ddrho, ti.value, ti.ddrho
        :params: CPO datastructures for equilibrium, coreprof, coretransp
        :returns: list of arrays of te_transp_flux, ti_transp_flux, elements are values for each flux tube
        """
        if params is None:
            params = self.code_parameters

        if equilibrium is None:
            equilibrium = self.equil

        if coreprof is None:    
            coreprof = self.corep_elem.core

        if coretransp is None:
            coretransp = self.coret
        
        res_coretransp, te_transp_flux, ti_transp_flux, teddrho, tiddrho, tevalue, tivalue, rho_new = gem(equilibrium, coreprof, coretransp, params)

        te_transp_flux_arr = res_coretransp.values[0].te_transp.flux[:]
        ti_transp_flux_arr = res_coretransp.values[0].ti_transp.flux[:,0] # last dimension is species

        return [te_transp_flux_arr, ti_transp_flux_arr], [tevalue, tivalue, teddrho, tiddrho], res_coretransp
    