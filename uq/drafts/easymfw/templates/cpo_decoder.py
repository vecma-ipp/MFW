import os
import logging
import numpy as np
from easyvvuq import OutputType
from easyvvuq.decoders.base import BaseDecoder
from .cpo_element import CPOElement


# Specific Decoder for CPO files
class CPODecoder(BaseDecoder, decoder_name="cpo_decoder"):

    def __init__(self, target_filename, output_columns, output_cponame):
        if target_filename is None:
            msg = (f"target_filename must be set for CPODecoder. This should be"
                   f"the name of the output file this decoder acts on.")
            logging.error(msg)
            raise Exception(msg)

        if output_columns is None:
            msg = (f"output_columns must be specified for CPODecoder.")
            logging.error(msg)
            raise Exception(msg)

        if output_cponame is None:
            msg = (f"output_cponame must be specified for CPODecoder.")
            logging.error(msg)
            raise Exception(msg)

        if len(output_columns) == 0:
            msg = "output_columns cannot be empty."
            logger.error(msg)
            raise Exception(msg)

        self.target_filename = target_filename
        self.output_columns = output_columns
        self.output_cponame = output_cponame

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
        cpo = CPOElement(out_path, self.output_cponame)

        # Results
        qoi_values = {}
        for qoi in self.output_columns:
            value = cpo.get_value(qoi)
            n = len(np.shape(value))
            # one or two spices
            if n == 1:
                qoi_values.update({qoi: value})
            else:
                for i in range(n):
                    qoi_values.update({(qoi, i): value[i]})

        # Output
        return qoi_values

    def get_restart_dict(self):
        return {"target_filename": self.target_filename,
                "output_columns": self.output_columns,
                "output_cponame": self.output_cponame}

    def element_version(self):
        return "0.3"
