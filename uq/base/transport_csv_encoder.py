import os
import logging
import numpy as np
import pandas as pd

from easyvvuq import OutputType


class TransportCSVEncoder:

    def __init__(self, csv_filename, input_dir,
                 input_params=None, target_filename=None):

        if csv_filename is None:
            msg = ("TransportCSVEncoder must be given 'csv_filename': a CPO filename.")
            logging.error(msg)
            raise RuntimeError(msg)

        self.data_filename = csv_filename
        self.input_dir = input_dir
        self.target_filename = target_filename if target_filename is not None else csv_filename

    def encode(self, params={}, target_dir=''):
        
        if not target_dir:
            raise RuntimeError('No target directory specified to encoder')
        
        if self.input_params is None:
            self.input_params = params

        data_file_path = os.path.join(self.input_dir, self.data_filename)

        self.data = pd.read_csv(data_file_path, sep=',')

        # TODO: treat all flux tubes
        # TODO: make sample for all cases, then substitute every reading!
        # TODO: choose between absolute value and relative perturbation - later seems more resonable
        for quantity, value_pert in self.input_params.items():

            quantity_name = quantity.split('_')[0]
            quantity_ft = quantity.split('_')[1]
            quantity_species = quantity_name[-1]

            mask = self.data['ft']==int(quantity_ft)
            column_name = f"t{quantity_species}_transp_flux"

            for row_i in self.data[mask].index:
                value_old = self.data.at[row_i, column_name]
                self.data.at[row_i, column_name] = value_old * (1 + value_pert)

        #new_data_filename = self.data_filename
        #TODO restart data?

        # Copy the new input file to the target directory
        target_file_path = os.path.join(target_dir, self.target_filename)
        self.data.to_csv(target_file_path, index=False)
        #os.system(f"cp {new_data_filename} {target_dir}/")

        return None

    def get_restart_dict(self):
        return {"data_filename": self.data_filename,
                "target_filename": self.target_filename,
                }
    
    def element_version(self):
        return "0.01"