import os
import logging
import numpy as np
import scipy.interpolate
from easyvvuq import OutputType
#from easyvvuq.encoders.base import BaseEncoder
#from easyvvuq.encoders.generic_template import GenericEncoder
from ascii_cpo import read, write
from .cpo_element import CPOElement


# Specific Encoder for CPO files
#class CPOEncoder(BaseEncoder, encoder_name="cpo_encoder"):
#class CPOEncoder(GenericEncoder, encoder_name="cpo_encoder"):
class CPOEncoder:

    def __init__(self, cpo_filename, cpo_name, input_dir,
                 input_params=None, target_filename=None, ftube_index=None):

        # Check that user has specified the object to use as cpo
        if cpo_filename is None:
            msg = ("CPOEncoder must be given 'cpo_filename': a CPO filename.")
            logging.error(msg)
            raise RuntimeError(msg)

        # Check the cpo_name
        cpo_namelist = ['coreprof', 'coretransp','equilibrium','coresource', 'coreimpur', 'toroidfield']
        if not cpo_name not in cpo_namelist:
            msg = ("CPOEncoder: wrong cpo_name")
            logging.error(msg)

        self.cpo_filename = cpo_filename
        self.cpo_name = cpo_name
        self.input_dir = input_dir
        self.input_params = input_params

        if target_filename is None:
            self.target_filename = cpo_filename
        else:
            self.target_filename = target_filename

        # TODO: put back ftube_index in input_params or keep it there (or remove it)?
        self.ftube_index = ftube_index

        # The cpo object
        self.cpo = CPOElement(cpo_filename, cpo_name, input_dir)

        self.nion = 1
        if cpo_name in ["coreprof", 'coretransp', 'coresource']:
            self.nion = len(self.cpo.get_value('compositions.ions'))


    # Create simulation input files
    def encode(self, params={}, target_dir=''):
        if not target_dir:
            raise RuntimeError('No target directory specified to encoder')

        if self.input_params is None:
            self.input_params = params

        for name, attr in self.input_params.items():
            value = params[name]
            
            if not isinstance(value, list):
                value = float(value) # For values read from a CSV .This should be double check for some parameters
            else:
                value = [float(v) for v in value]

            index = self.ftube_index

            if name in ['te.value', 'te.ddrho', 'ti.value', 'ti.ddrho']:
                # TODO verify if self.ftube_index is not None
                self._update_gradients(name, value, index=index)
            else:
                self.cpo.set_value(name, value, index=index)

        # Do a symbolic link to other files (cpo, xml and xsd)
        os.system("ln -s " + self.input_dir + "*.xml " + target_dir + " 2>/dev/null")
        os.system("ln -s " + self.input_dir + "*.xsd " + target_dir + " 2>/dev/null")
        os.system("ln -s " + self.input_dir + "*.cpo " + target_dir + " 2>/dev/null")
        # Copy restart data if given
        for filename in os.listdir(self.input_dir):
            if filename.endswith(".dat"):
                os.system("cp " + self.input_dir + filename + " " + target_dir + " 2>/dev/null")

        # Write target input CPO file
        target_file_path = os.path.join(target_dir, self.target_filename)
        if(os.path.isfile(target_file_path)):
            os.system("rm " + target_file_path)
        self.cpo.save_file(target_file_path)


    def _update_gradients(self, name, value, index=0):
        """
        Params:
            name
            value: can be float or list<float>
            index
        """
        rho = self.cpo.get_value('rho_tor_norm')

        if isinstance(index, int):
            index = [index]

        if not isinstance(value, list):
            value = [value]
        
        print('>DEBUG: i is of type: {0}'.format(type(index)))

        for i, v in zip(index, value):

            values = []
            indices = []
            if name == 'te.value':
                dt = self.cpo.get_value('te.ddrho')
                t_i  = v
                dt_i = dt[i]
                values.append(v)
                indices.append(i)
            if name == 'te.ddrho':
                t = self.cpo.get_value('te.value')
                t_i  = t[i]
                dt_i = v
                self.cpo.set_value('te.ddrho', [v], [i])
            if name == 'ti.value':
                dt = self.cpo.get_value('ti.ddrho')
                t_i  = v
                if self.nion == 1:
                    dt_i = dt[i]
                else:
                    dt_i = dt[i][0]
                values.append(v)
                indices.append(i)
            if name == 'ti.ddrho':
                t = self.cpo.get_value('ti.value')
                if self.nion == 1:
                    t_i  = t[i]
                else:
                    t_i  = t[i][0]
                dt_i = v
                self.cpo.set_value('ti.ddrho', [v], [i])

            # neighbors to update
            values.append(dt_i*(rho[i-2] - rho[i]) + t_i)
            values.append(dt_i*(rho[i-1] - rho[i]) + t_i)
            values.append(dt_i*(rho[i+1] - rho[i]) + t_i)
            values.append(dt_i*(rho[i+2] - rho[i]) + t_i)
            indices += [i-2, i-1, i+1, i+2]
            if name[0:2] =='te':
                self.cpo.set_value('te.value', values, indices)
            if name[0:2] == 'ti':
                self.cpo.set_value('ti.value', values, indices)


    def get_restart_dict(self):
        return {"cpo_filename": self.cpo_filename,
                "cpo_name": self.cpo_name,
                "input_dir": self.input_dir,
                "input_params": self.input_params,
                "target_filename": self.target_filename,
                "ftube_index": self.ftube_index
                }

    def element_version(self):
        return "0.51"
