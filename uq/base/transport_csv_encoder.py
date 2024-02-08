import os
import logging
import numpy as np
import pandas as pd

from easyvvuq import OutputType


class TransportCSVEncoder:

    def __init__(self, data_filename) -> None:

        if data_filename is None:
            msg = ("TransportCSVEncoder must be given 'data_filename': a CPO filename.")
            logging.error(msg)
            raise RuntimeError(msg)

        self.data_filename = data_filename

    def encode(self, params={}, target_dir=''):
        
        if not target_dir:
            raise RuntimeError('No target directory specified to encoder')
        
        if self.input_params is None:
            self.input_params = params

        self.data = pd.read_csv(self.data_filename, sep=',')

        # TODO: treat all flux tubes
        for quantity, value in self.input_params.items():
            self.data[quantity] = value

        new_data_filename = self.data_filename

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