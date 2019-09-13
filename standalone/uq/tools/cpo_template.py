import os
import logging
import chaospy as cp
import pandas as pd
from string import Template
from easyvvuq import OutputType
from easyvvuq.encoders.base import BaseEncoder
from easyvvuq.decoders.base import BaseDecoder
from ascii_cpo import read, write


# Specific Encoder
class CPOEncoder(BaseEncoder, encoder_name="cpo_encoder"):

    def __init__(self, template_filename, template_cponame, cpos_directory, target_filename, uncertain_params):

        self.template_filename = template_filename
        self.template_cponame = template_cponame
        self.cpos_directory = cpos_directory
        self.target_filename = target_filename
        self.uncertain_params = uncertain_params
        self.fixture_support = True

        # Check that user has specified the objests to use as template
        if template_filename is None:
            msg = ("CPOEncoder must be given 'template_filename': a CPO file.")
            logging.error(msg)
            raise RuntimeError(msg)

        # The CPO object
        self.core_cpo = read(cpos_directory + template_filename, template_cponame)

    # Return param dict for Campagn and list of distribitions for Sampler
    def draw_app_params(self):

        params = {}
        vary = {}

        for key in self.uncertain_params.keys():
            # Get initial values
            if(key=='Te_boundary'):
                val = self.core_cpo.te.boundary.value[0]
            if(key=='Ti_boundary'):
                val = self.core_cpo.ti.boundary.value[0][0]

            dist_name = self.uncertain_params[key]["distribution"]
            margin_error = self.uncertain_params[key]["margin_error"]

            # Build distributions
            if dist_name == "Normal":
                dist = cp.Normal(val, margin_error*val)
            if dist_name == "Uniform":
                dist = cp.Uniform((1. - margin_error)*val, (1. - margin_error)*val)

            # Update output dict
            params.update({key: {"type": "float", "default": val}})
            vary.update({key: dist})

        return params, vary

    # Create simulation input files
    def encode(self, params={}, target_dir='', fixtures=None):

        if fixtures is not None:
            local_params = self.substitute_fixtures_params(params, fixtures, target_dir)
        else:
            local_params = params

        if not target_dir:
            raise RuntimeError('No target directory specified to encoder')

        for key, value in local_params.items():
            if(key=='Te_boundary'):
                self.core_cpo.te.boundary.value[0] = value
            if(key=='Ti_boundary'):
                self.core_cpo.ti.boundary.value[0][0] = value

        # Do a symbolic link to other CPO files
        os.system("ln -s " + self.cpos_directory + "*.cpo " + target_dir)

        # Write target input CPO file
        target_file_path = os.path.join(target_dir, self.target_filename)
        if(os.path.isfile(target_file_path)):
            os.system("rm -rf " + target_file_path)

        write(self.core_cpo, target_file_path)

    def get_restart_dict(self):
        return {"template_filename": self.template_filename,
                "template_cponame": self.template_cponame,
                "cpos_directory": self.cpos_directory,
                "target_filename": self.target_filename,
                "uncertain_params": self.uncertain_params}

    def element_version(self):
        return "0.1"

# Specific Decoder
class CPODecoder(BaseDecoder, decoder_name="cpo_decoder"):

    def __init__(self, target_filename, target_cponame, output_columns):

        if target_filename is None:
            msg = (f"target_filename must be set for CPODecoder. This should be"
                   f"the name of the output file this decoder acts on.")
            logging.error(msg)
            raise Exception(msg)

        if output_columns is None:
            msg = (f"output_columns must be specified for CPODecoder.")
            logging.error(msg)
            raise Exception(msg)

        if target_cponame is None:
            msg = (f"target_cponame must be specified for CPODecoder.")
            logging.error(msg)
            raise Exception(msg)

        if len(output_columns) == 0:
            msg = "output_columns cannot be empty."
            logger.error(msg)
            raise Exception(msg)

        self.target_filename = target_filename
        self.target_cponame = target_cponame
        self.output_columns = output_columns

        self.output_type = OutputType('sample')

    @staticmethod
    def _get_output_path(run_info=None, outfile=None):

        run_path = run_info['run_dir']

        if not os.path.isdir(run_path):
            raise RuntimeError(f"Run directory does not exist: {run_path}")

        return os.path.join(run_path, outfile)

    def sim_complete(self, run_info=None):

        out_path = self._get_output_path(run_info, self.target_filename)

        if not os.path.isfile(out_path):
            return False
        else:
            return True

    def parse_sim_output(self, run_info={}):

        out_path = self._get_output_path(run_info, self.target_filename)

        # The CPO object
        core_cpo = read(out_path, self.target_cponame)

        # TODO check and add all possibilities
        quoi_dict = {}
        if self.target_cponame == "coreprof":
            for qoi in self.output_columns:
                if qoi == "Te":
                    quoi_dict.update({qoi: core_cpo.te.value})
                if qoi == "Ti":
                    quoi_dict.update({qoi: core_cpo.ti.value[:,0]})

        data = pd.DataFrame(quoi_dict)

        return data

    def get_restart_dict(self):
        return {"target_filename": self.target_filename,
                "output_columns": self.output_columns,
                "target_cponame": self.target_cponame}

    def element_version(self):
        return "0.1"
