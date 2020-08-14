import os
import logging
import numpy as np
import scipy.interpolate
from easyvvuq import OutputType
from easyvvuq.encoders.base import BaseEncoder
from ascii_cpo import read, write
from .cpo_element import CPOElement


# Specific Encoder for CPO files
class CPOEncoder(BaseEncoder, encoder_name="cpo_encoder"):

    def __init__(self, template_filename, target_filename,
                 input_cponame, input_params, common_dir):

        # Check that user has specified the object to use as template
        if template_filename is None:
            msg = ("CPOEncoder must be given 'template_filename': a CPO file.")
            logging.error(msg)
            raise RuntimeError(msg)

        self.template_filename = template_filename
        self.target_filename = target_filename
        self.input_cponame = input_cponame
        self.input_params = input_params
        self.common_dir = common_dir

        # The cpo object
        input_cpofile = os.path.join(common_dir, template_filename)
        self.cpo = CPOElement(input_cpofile, input_cponame)

    # Create simulation input files
    def encode(self, params={}, target_dir=''):
        if not target_dir:
            raise RuntimeError('No target directory specified to encoder')

        if self.input_params is None:
            self.input_params = params

        for name, attr in self.input_params.items():
            value = params[name]
            indices = None
            if "idx" in attr.keys():
                indices = attr["idx"]
                if len(indices)==1:
                    value = [value]
            if "bsp_x" in attr.keys():
                x = attr["bsp_x"]
                y = value
                rho = attr["rho"]
                S = scipy.interpolate.make_interp_spline(x, y, bc_type=([(1, 0.0)], [(2, 0.0)]))
                value = S(rho).tolist()
            self.cpo.set_value(name, value, indices)
            # particular case: te.value and ti.value
            # update neighbors +/-2 rho_tor grid points according to
            # the slops given by te.ddrho and ti.ddrho
            if name == 'te.value' or name == 'te.ddrho':
                rho = self.cpo.get_value('rho_tor_norm')
                neighb_values = []
                if name == 'te.value':
                    dte = self.cpo.get_value('te.ddrho')
                    j = 0
                    for i in indices:
                        rho_i = rho[i]
                        te_i  = value[j]
                        dte_i = dte[i]
                        neighb_values.append(dte_i*(rho[i-2] - rho_i) + te_i)
                        neighb_values.append(dte_i*(rho[i-1] - rho_i) + te_i)
                        neighb_values.append(dte_i*(rho[i+1] - rho_i) + te_i)
                        neighb_values.append(dte_i*(rho[i+2] - rho_i) + te_i)
                        j+=1
                if name == 'te.ddrho':
                    te = self.cpo.get_value('te.value')
                    j = 0
                    for i in indices:
                        rho_i = rho[i]
                        te_i  = te[i]
                        dte_i = value[j]
                        neighb_values.append(dte_i*(rho[i-2] - rho_i) + te_i)
                        neighb_values.append(dte_i*(rho[i-1] - rho_i) + te_i)
                        neighb_values.append(dte_i*(rho[i+1] - rho_i) + te_i)
                        neighb_values.append(dte_i*(rho[i+2] - rho_i) + te_i)
                        j+=1
                j = 0
                neighb_indices = []
                for i in indices:
                    neighb_indices.append(i-2)
                    neighb_indices.append(i-1)
                    neighb_indices.append(i+1)
                    neighb_indices.append(i+2)
                    j+=1
                self.cpo.set_value('te.value', neighb_values, neighb_indices)

        # Do a symbolic link to other files (cpo, xml and restart data)
        os.system("ln -s " + self.common_dir + "*.xml " + target_dir + " 2>/dev/null")
        os.system("ln -s " + self.common_dir + "*.xsd " + target_dir + " 2>/dev/null")
        os.system("ln -s " + self.common_dir + "*.cpo " + target_dir + " 2>/dev/null")
        #os.system("ln -s " + self.common_dir + "*.dat " + target_dir + " 2>/dev/null")

        # Write target input CPO file
        target_file_path = os.path.join(target_dir, self.target_filename)
        if(os.path.isfile(target_file_path)):
            os.system("rm " + target_file_path)
        self.cpo.save(target_file_path)

    def get_restart_dict(self):
        return {"template_filename": self.template_filename,
                "target_filename": self.target_filename,
                "input_cponame": self.input_cponame,
                "input_params": self.input_params,
                "common_dir": self.common_dir,
                }

    def element_version(self):
        return "0.3"
