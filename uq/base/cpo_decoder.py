import os
import logging
import numpy as np
from easyvvuq import OutputType
#from easyvvuq.decoders.base import BaseDecoder
from .cpo_element import CPOElement


# Specific Decoder for CPO files
#class CPODecoder(BaseDecoder, decoder_name="cpo_decoder"):
class CPODecoder:

    def __init__(self, cpo_filename, cpo_name, output_columns):
        if cpo_filename is None:
            msg = (f"cpo_filename must be set for CPODecoder. This should be"
                   f"the name of the output file this decoder acts on.")
            logging.error(msg)
            raise Exception(msg)

        if cpo_name is None:
            msg = (f"cpo_name must be specified for CPODecoder.")
            logging.error(msg)
            raise Exception(msg)

        if output_columns is None:
            msg = (f"output_columns must be specified for CPODecoder.")
            logging.error(msg)
            raise Exception(msg)

        if len(output_columns) == 0:
            msg = "output_columns cannot be empty."
            logger.error(msg)
            raise Exception(msg)

        self.cpo_filename = cpo_filename
        self.cpo_name = cpo_name
        self.output_columns = output_columns

        self.output_type = OutputType('sample')

        self.target_filename = cpo_filename

    @staticmethod
    def _get_output_path(run_info=None, outfile=None):
        run_path = run_info['run_dir']

        if not os.path.isdir(run_path):
            raise RuntimeError(f"Run directory does not exist: {run_path}")

        return os.path.join(run_path, outfile)

    def sim_complete(self, run_info=None):

        out_path = self._get_output_path(run_info, self.cpo_filename)

        if not os.path.isfile(out_path):
            return False
        else:
            return True

    def parse_sim_output(self, run_info={}):
        out_path = self._get_output_path(run_info, self.cpo_filename)

        # The CPO object
        cpo = CPOElement(out_path, self.cpo_name)

        # Results
        qoi_values = {}
        for qoi in self.output_columns:
            value = cpo.get_value(qoi)
            n = len(np.shape(value))
            # One or two ion spices
            if n == 1:
                qoi_values.update({qoi: value})
            else:
                for i in range(n):
                    qoi_values.update({(qoi, i): value[i]})

        #print(qoi_values) ###DEBUG

        # Outputs
        return qoi_values

    def get_restart_dict(self):
        return {"cpo_filename": self.cpo_filename,
                "cpo_name": self.cpo_name,
                "output_columns": self.output_columns,
                "target_filename": self.target_filename,}

    def element_version(self):
        return "0.5"
