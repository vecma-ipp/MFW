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
        self.nion = 1
        if input_cponame in ["coreprof", 'coretransp', 'coresource']:
            self.nion = len(self.cpo.get_value('compositions.ions'))


    # Create simulation input files
    def encode(self, params={}, target_dir=''):
        if not target_dir:
            raise RuntimeError('No target directory specified to encoder')

        if self.input_params is None:
            self.input_params = params

        for name, attr in self.input_params.items():
            value = params[name]

            # Particular case: pofile as uncertain paramert: reduce the
            # size using bspline approximation. TODO new Specific Encoder
            if "bsp_x" in attr.keys():
                x = attr["bsp_x"]
                y = value
                rho = attr["rho"]
                S = scipy.interpolate.make_interp_spline(x, y, bc_type=([(1, 0.0)], [(2, 0.0)]))
                value = S(rho).tolist()

            # Particular case: uncertain parmater is te.value or
            # ti.value in a given flux tube. Get the Flux tube position
            # and update neighbors (+/-2 grid points) according to the
            # slops given by te.ddrho and ti.ddrho
            # TODO add more parameters and use external routine
            if "ft_index" in attr.keys():
                i = attr["ft_index"]
                values = []
                indices = []
                if name in ['te.value', 'te.ddrho', 'ti.value', 'ti.ddrho']:
                    rho = self.cpo.get_value('rho_tor_norm')
                    if name == 'te.value':
                        dt = self.cpo.get_value('te.ddrho')
                        t_i  = value
                        dt_i = dt[i]
                        values.append(value)
                        indices.append(i)
                    if name == 'te.ddrho':
                        t = self.cpo.get_value('te.value')
                        t_i  = t[i]
                        dt_i = value
                        self.cpo.set_value('te.ddrho', [value], [i])
                    if name == 'ti.value':
                        dt = self.cpo.get_value('ti.ddrho')
                        t_i  = value
                        if self.nion == 1:
                            dt_i = dt[i]
                        else:
                            dt_i = dt[i][0]
                        values.append(value)
                        indices.append(i)
                    if name == 'ti.ddrho':
                        t = self.cpo.get_value('te.value')
                        if self.nion == 1:
                            t_i  = t[i]
                        else:
                            t_i  = t[i][0]
                        dt_i = value
                        self.cpo.set_value('ti.ddrho', [value], [i])

                    # neighbors to update
                    values.append(dt_i*(rho[i-2] - rho[i]) + t_i)
                    values.append(dt_i*(rho[i-1] - rho[i]) + t_i)
                    values.append(dt_i*(rho[i+1] - rho[i]) + t_i)
                    values.append(dt_i*(rho[i+2] - rho[i]) + t_i)
                    indices += [i-2, i-1, i+1, i+2]
                    if name in ['te.value', 'te.ddrho']:
                        self.cpo.set_value('te.value', values, indices)
                    if name in ['ti.value', 'ti.ddrho']:
                        self.cpo.set_value('ti.value', values, indices)

            else:
                self.cpo.set_value(name, value)

        # Do a symbolic link to other files (cpo and xml)
        os.system("ln -s " + self.common_dir + "*.xml " + target_dir)
        os.system("ln -s " + self.common_dir + "*.xsd " + target_dir)
        os.system("ln -s " + self.common_dir + "*.cpo " + target_dir)
        # Copy restart data if given
        for fname in os.listdir(self.common_dir):
            if fname.endswith(".dat"):
                os.system("cp " + self.common_dir + fname + " " + target_dir)
        if os.path.isdir(self.common_dir + "/surrogate"):
            os.system("ln -s " + self.common_dir + "/surrogate/" + "*.joblib " + target_dir)
            os.system("cp " + self.common_dir + "/surrogate/" + "*.cpo " + target_dir)

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
        return "0.4"
