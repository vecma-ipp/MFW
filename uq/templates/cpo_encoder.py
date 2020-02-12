import os
import logging
from easyvvuq import OutputType
from easyvvuq.encoders.base import BaseEncoder
from ascii_cpo import read, write
from utils import cpo_io


# Specific Encoder for CPO files
class CPOEncoder(BaseEncoder, encoder_name="cpo_encoder"):

    def __init__(self, template_filename, target_filename,
                 common_dir, params_names=None, flux_index=0):
        # Check that user has specified the object to use as template
        if template_filename is None:
            msg = ("CPOEncoder must be given 'template_filename': a CPO file.")
            logging.error(msg)
            raise RuntimeError(msg)

        self.template_filename = template_filename
        self.target_filename = target_filename
        self.common_dir = common_dir
        self.params_names = params_names
        self.flux_index = flux_index

        # The cpo object
        cpo_filename = os.path.join(common_dir, template_filename)
        cpo_name = cpo_io.get_cponame(template_filename)
        self.cpo_core = read(cpo_filename, cpo_name)

    # Create simulation input files
    def encode(self, params={}, target_dir=''):
        if not target_dir:
            raise RuntimeError('No target directory specified to encoder')

        if self.params_names is None:
            self.params_names = list(params)

        for key in self.params_names:
            value = params[key]
            cpo_io.set_parameters(self.cpo_core, key, value, self.flux_index)
            # Todo Update Te and Ti

        # Write target input CPO file
        target_file_path = os.path.join(target_dir, self.target_filename)
        if(os.path.isfile(target_file_path)):
            os.system("rm " + target_file_path)
        write(self.cpo_core, target_file_path)

        # Do a symbolic link to other files (cpo, xml and restart data)
        os.system("ln -s " + self.common_dir + "* " + target_dir + " >/dev/null 2>&1")

    def get_restart_dict(self):
        return {"template_filename": self.template_filename,
                "target_filename": self.target_filename,
                "common_dir": self.common_dir,
                "params_names": self.params_names,
                "flux_index": self.flux_index}

    def element_version(self):
        return "0.2"
