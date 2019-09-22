# -*- coding: UTF-8 -*-
import os
import logging
import chaospy as cp
import pandas as pd
from easyvvuq import OutputType
from easyvvuq.decoders.base import BaseDecoder
from ascii_cpo import read, write
from utils import statistics


# Specific Decoder for CPO files
class CPODecoder(BaseDecoder, decoder_name="cpo_decoder"):

    def __init__(self, target_filename, cpo_name, output_columns):

        if target_filename is None:
            msg = (f"target_filename must be set for CPODecoder. This should be"
                   f"the name of the output file this decoder acts on.")
            logging.error(msg)
            raise Exception(msg)

        if output_columns is None:
            msg = (f"output_columns must be specified for CPODecoder.")
            logging.error(msg)
            raise Exception(msg)

        if cpo_name is None:
            msg = (f"cpo_name must be specified for CPODecoder.")
            logging.error(msg)
            raise Exception(msg)

        if len(output_columns) == 0:
            msg = "output_columns cannot be empty."
            logger.error(msg)
            raise Exception(msg)

        self.target_filename = target_filename
        self.cpo_name = cpo_name
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
        cpo_core = read(out_path, self.cpo_name)

        # TODO check and add all possibilitieus => user switcher
        quoi_dict = {}
        if self.cpo_name == "coreprof":
            for qoi in self.output_columns:
                if qoi == "Te":
                    quoi_dict.update({qoi: cpo_core.te.value})
                if qoi == "Ti":
                    quoi_dict.update({qoi: cpo_core.ti.value[:,0]})

        data = pd.DataFrame(quoi_dict)

        return data

    def get_restart_dict(self):
        return {"target_filename": self.target_filename,
                "cpo_name": self.cpo_name,
                "output_columns": self.output_columns}

    def element_version(self):
        return "0.1"
