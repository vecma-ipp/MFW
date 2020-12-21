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

    def __init__(self, template_filename, input_cponame, input_cpodir,
                 ftube_index=None, target_filename=None):

        # Check that user has specified the object to use as template
        if template_filename is None:
            msg = ("CPOEncoder must be given 'template_filename': a CPO file name.")
            logging.error(msg)
            raise RuntimeError(msg)

        self.template_filename = template_filename

        if target_filename is None:
            self.target_filename = template_filename
        else:
            self.target_filename = target_filename

        self.input_cponame = input_cponame
        self.input_cpodir = input_cpodir
        self.ftube_index = ftube_index

        # The cpo object
        input_cpofile = os.path.join(input_cpodir, template_filename)
        self.cpo = CPOElement(input_cpofile, input_cponame)
        self.nion = 1
        if input_cponame in ["coreprof", 'coretransp', 'coresource']:
            self.nion = len(self.cpo.get_value('compositions.ions'))


    # Create simulation input files
    def encode(self, params={}, target_dir=''):
        if not target_dir:
            raise RuntimeError('No target directory specified to encoder')

        for name, attr in params.items():
            value = params[name]

            if name in ['te.value', 'te.ddrho', 'ti.value', 'ti.ddrho']:
                # TODO verify if self.ftube_index is not None
                self._update_gradients(name)
            else:
                self.cpo.set_value(name, value)

        # Do a symbolic link to other files (cpo, xml and xsd)
        os.system("ln -s " + self.input_cpodir + "*.xml " + target_dir)
        os.system("ln -s " + self.input_cpodir + "*.xsd " + target_dir)
        os.system("ln -s " + self.input_cpodir + "*.cpo " + target_dir)
        # Copy restart data if given
        for filename in os.listdir(self.input_cpodir):
            if filename.endswith(".dat"):
                os.system("cp " + self.input_cpodir + filename + " " + target_dir)

        # Write target input CPO file
        target_file_path = os.path.join(target_dir, self.target_filename)
        if(os.path.isfile(target_file_path)):
            os.system("rm " + target_file_path)
        self.cpo.save(target_file_path)


    def _update_gradients(self, param_name):
        rho = self.cpo.get_value('rho_tor_norm')
        i = self.ftube_index

        values = []
        indices = []
        if param_name == 'te.value':
            dt = self.cpo.get_value('te.ddrho')
            t_i  = value
            dt_i = dt[i]
            values.append(value)
            indices.append(i)
        if param_name == 'te.ddrho':
            t = self.cpo.get_value('te.value')
            t_i  = t[i]
            dt_i = value
            self.cpo.set_value('te.ddrho', [value], [i])
        if param_name == 'ti.value':
            dt = self.cpo.get_value('ti.ddrho')
            t_i  = value
            if self.nion == 1:
                dt_i = dt[i]
            else:
                dt_i = dt[i][0]
            values.append(value)
            indices.append(i)
        if param_name == 'ti.ddrho':
            t = self.cpo.get_value('ti.value')
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
        if param_name[0:2] =='te':
            self.cpo.set_value('te.value', values, indices)
        if param_name[0:2] == 'ti':
            self.cpo.set_value('ti.value', values, indices)


    def get_restart_dict(self):
        return {"template_filename": self.template_filename,
                "input_cponame": self.input_cponame,
                "input_cpodir": self.input_cpodir,
                "ftube_index": self.ftube_index,
                "target_filename": self.target_filename
                }


    def element_version(self):
        return "0.5"
