import os
from string import Template
from easyvvuq.encoders.base import BaseEncoder
from ascii_cpo import read, write
import logging
import json


class CPOEncoder(BaseEncoder, encoder_name="cpo_template"):

    def __init__(self, template_filename, target_filename, cpo_type):

        self.template_filename = template_filename
        self.target_filename = target_filename
        self.cpo_type = cpo_type
        self.fixture_support = True

        # Check that user has specified the object to use as template
        if template_filename is None:
            msg = ("CPOEncoder must be given 'template_filename': a CPO object.")
            logging.error(msg)
            raise RuntimeError(msg)

        self.core_cpo = read(template_filename, cpo_type)


    def encode(self, params={}, target_dir='', fixtures=None):

        if fixtures is not None:
            local_params = self.substitute_fixtures_params(params, fixtures,
                                                           target_dir)
        else:
            local_params = params

        if not target_dir:
            raise RuntimeError('No target directory specified to encoder')

        for key, value in local_params.items():
            if(key=='Te_boundary'):
                self.core_cpo.te.boundary.value[0] = value
            if(key=='Ti_boundary'):
                self.core_cpo.ti.boundary.value[0][0] = value

        # Write target input file
        target_file_path = os.path.join(target_dir, self.target_filename)
        write(self.core_cpo, target_file_path)

    def _log_substitution_failure(self, exception):
        reasoning = (f"\nFailed substituting into template "
                     f"{self.template_filename}.\n"
                     f"KeyError: {str(exception)}.\n")
        logging.error(reasoning)

        raise KeyError(reasoning)

    def get_restart_dict(self):
        return {"target_filename": self.target_filename,
                "template_filename": self.template_filename,
                "cpo_type": self.cpo_type}

    def element_version(self):
        return "0.1"
