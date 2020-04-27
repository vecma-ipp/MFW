import os
import logging
from easyvvuq import OutputType
from easyvvuq.encoders.base import BaseEncoder
from ascii_cpo import read, write
from .cpo_element import CPOElement


# Specific Encoder for CPO files
class CPOEncoder(BaseEncoder, encoder_name="cpo_encoder"):

    def __init__(self, template_filename, target_filename,
                 input_cponame, common_dir, uncertain_params=None):
        # Check that user has specified the object to use as template
        if template_filename is None:
            msg = ("CPOEncoder must be given 'template_filename': a CPO file.")
            logging.error(msg)
            raise RuntimeError(msg)

        self.template_filename = template_filename
        self.target_filename = target_filename
        self.input_cponame = input_cponame
        self.common_dir = common_dir
        self.uncertain_params = uncertain_params

        # The cpo object
        input_cpofile = os.path.join(common_dir, template_filename)
        self.cpo = CPOElement(input_cpofile, input_cponame)

    # Create simulation input files
    def encode(self, params={}, target_dir=''):
        if not target_dir:
            raise RuntimeError('No target directory specified to encoder')

        if self.uncertain_params is None:
            self.uncertain_params = list(params)

        for name, attr in self.uncertain_params.items():
            value = params[name]
            index = None
            if "ids" in attr.keys():
                index = [attr["ids"]]
            self.cpo.set_value(name, value, index)

        # Write target input CPO file
        target_file_path = os.path.join(target_dir, self.target_filename)
        if(os.path.isfile(target_file_path)):
            os.system("rm " + target_file_path)
        self.cpo.save(target_file_path)

        # Do a symbolic link to other files (cpo, xml and restart data)
        os.system("ln -s " + self.common_dir + "* " + target_dir + " >/dev/null 2>&1")

    def get_restart_dict(self):
        return {"template_filename": self.template_filename,
                "target_filename": self.target_filename,
                "common_dir": self.common_dir,
                "input_cponame": self.input_cponame,
                "uncertain_params": self.uncertain_params}

    def element_version(self):
        return "0.3"
