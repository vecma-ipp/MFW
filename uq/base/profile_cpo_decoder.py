import os
import logging
import numpy as np
import pandas as pd

from easyvvuq import OutputType

from .cpo_element import CPOElement

class ProfileCPODecoder:

    def __init__(self, cpo_filename, cpo_path='', cpo_name='coreprof', output_columns=["te_value_0", "ti_value_0"]):

        if cpo_filename is None:
            msg = (f"cpo_filename must be set for ProfileCPODecoder. This should be"
                   f"the name of the output file this decoder acts on.")
            logging.error(msg)
            raise Exception(msg)

        if cpo_name is None:
            msg = (f"cpo_name must be specified for ProfileCPODecoder.")
            logging.error(msg)
            raise Exception(msg)

        if output_columns is None:
            msg = (f"output_columns must be specified for CPODecoder.")
            logging.error(msg)
            raise Exception(msg)

        if len(output_columns) == 0:
            msg = "output_columns cannot be empty."
            logging.error(msg)
            raise Exception(msg)

        self.cpo_filename = cpo_filename
        self.cpo_path = cpo_path
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

        self.cpo_file_full_path = os.path.join(self.cpo_path, self.cpo_filename)

        out_path = self._get_output_path(run_info, self.cpo_file_full_path)

        cpo = CPOElement(out_path, self.cpo_name)
        #TODO old CPOElement does no consider interpolation on arbitrary grids!

        qoi_values = {}

        for qoi in self.output_columns:

            qoi_quantity = qoi.split('_')[0]
            qoi_name = qoi.split('_')[1]
            qoi_coord = qoi.split('_')[2]

            qoi_short = f"{qoi_quantity}_{qoi_name}"
            coord = int(qoi_coord)

            value = cpo.get_value(f"{qoi_quantity}.{qoi_name}")
            qoi_values[f"{qoi}"] = value[coord]

            #print(f"{qoi}={value[coord]}") ###DEBUG
        
        #print(qoi_values) ###DEBUG

        return qoi_values

    def get_restart_dict(self):
        return {"cpo_filename": self.cpo_filename,
                "cpo_name": self.cpo_name,
                "output_columns": self.output_columns,
                "target_filename": self.target_filename,}

    def element_version(self):
        return "0.01"