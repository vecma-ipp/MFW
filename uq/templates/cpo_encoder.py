import os
import logging
from easyvvuq import OutputType
from easyvvuq.encoders.base import BaseEncoder
from ascii_cpo import read, write
from utils import cpo_io, cpo_tools


# Specific Encoder for CPO files
class CPOEncoder(BaseEncoder, encoder_name="cpo_encoder"):

    def __init__(self,
                 template_filename, target_filename,
                 common_dir, cpo_name,
                 link_xmlfiles=False, link_data=False):

        # Check that user has specified the objests to use as template
        if template_filename is None:
            msg = ("CPOEncoder must be given 'template_filename': a CPO file.")
            logging.error(msg)
            raise RuntimeError(msg)

        self.template_filename = template_filename
        self.target_filename = target_filename
        self.common_dir = common_dir
        self.cpo_name = cpo_name
        self.link_xmlfiles = link_xmlfiles
        self.link_data = link_data

        # The CPO object (use cpo_io?)
        cpo_filename = os.path.join(common_dir, template_filename)
        self.cpo_core = read(cpo_filename, cpo_name)

    # Create simulation input files
    def encode(self, params={}, target_dir=''):

        if not target_dir:
            raise RuntimeError('No target directory specified to encoder')

        for key, value in params.keys():
            cpo_io.set_parameters(self.cpo_core, key, value)
            # Todo Update Te and Ti

        # Do a symbolic link to other CPO, XML files and restart data
        os.system("ln -s " + self.common_dir + "*.cpo " + target_dir)
        if self.link_xmlfiles:
            os.system("ln -s " + self.common_dir + "*.xml " + target_dir)
            os.system("ln -s " + self.common_dir + "*.xsd " + target_dir)
        if self.link_data:
            os.system("ln -s " + self.common_dir + "*.dat " + target_dir)

        # Write target input CPO file
        target_file_path = os.path.join(target_dir, self.target_filename)
        if(os.path.isfile(target_file_path)):
            os.system("rm -rf " + target_file_path)

        write(self.cpo_core, target_file_path)

    def get_restart_dict(self):
        return {"template_filename": self.template_filename,
                "target_filename": self.target_filename,
                "common_dir": self.common_dir,
                "cpo_name": self.cpo_name,
                "link_xmlfiles": self.link_xmlfiles,
                "link_data": self.link_data}

    def element_version(self):
        return "0.2"
