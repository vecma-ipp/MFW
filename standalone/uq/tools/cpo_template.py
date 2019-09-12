import os
import logging
import chaospy as cp
from string import Template
from easyvvuq.encoders.base import BaseEncoder
from easyvvuq.decoders.base import BaseDecoder
from ascii_cpo import read, write


# Specific Encoder
class CPOEncoder(BaseEncoder, encoder_name="cpo_encoder"):

    #
    def __init__(self, template_filename, template_directory, target_filename, uncertain_params):

        self.template_filename = template_filename
        self.template_directory = template_directory
        self.target_filename = target_filename
        self.uncertain_params = uncertain_params
        self.fixture_support = True

        # Check that user has specified the object to use as template
        if template_filename is None:
            msg = ("CPOEncoder must be given 'template_filename': a CPO file.")
            logging.error(msg)
            raise RuntimeError(msg)

        # Get the corresponding CPO object
        cponame = uncertain_params["structure"]
        if cponame in template_filename:
            self.core_cpo = read(template_directory + template_filename, cponame)
        else:
            msg = ("The structure filed of 'uncertain_params' must much with 'template_filename'")
            logging.error(msg)
            raise RuntimeError(msg)

    # Return param dict for Campagn and list of distribitions for Sampler
    def draw_app_params(self):

        params = {}
        vary = {}

        for key in self.uncertain_params.keys() - {"structure"}:
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

        # Do a symbolic link to other CPO files
        os.system("ln -s " + self.template_directory + "*.cpo " + target_dir)

        # Write target input CPO file
        target_file_path = os.path.join(target_dir, self.target_filename)
        if(os.path.isfile(target_file_path)):
            os.system("rm -rf " + target_file_path)

        write(self.core_cpo, target_file_path)

    def get_restart_dict(self):
        return {"target_filename": self.target_filename,
                "template_filename": self.template_filename,
                "template_directory": self.template_directory,
                "uncertain_params": self.uncertain_params}

    def element_version(self):
        return "0.1"

# Specific Decoder
class CPODecoder(BaseDecoder, decoder_name="cpo_decoder"):

    def __init__(self, output_columns=None):

        if output_columns is None:
            msg = (
                f"output_columns must be specified for CPODecoder. This should"
                f"be the names of the output columns this decoder extracts"
                f"from the target csv file."
            )
            logging.error(msg)
            raise Exception(msg)

        if len(output_columns) == 0:
            msg = "output_columns cannot be empty."
            logger.error(msg)
            raise Exception(msg)

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

        data = pd.read_csv(
            out_path,
            names=self.output_columns,
            header=self.header)

        return data

    def get_restart_dict(self):
        return {"target_filename": self.target_filename,
                "output_columns": self.output_columns,
                "header": self.header}

    def element_version(self):
        return "0.1"
