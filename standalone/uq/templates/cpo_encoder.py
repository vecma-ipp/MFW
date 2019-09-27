# -*- coding: UTF-8 -*-
import os
import logging
import chaospy as cp
import pandas as pd
from easyvvuq import OutputType
from easyvvuq.encoders.base import BaseEncoder
from ascii_cpo import read, write
from utils import statistics


# Specific Encoder for CPO files
class CPOEncoder(BaseEncoder, encoder_name="cpo_encoder"):

    def __init__(self,
                 template_filename, target_filename,
                 cpo_name, common_dir, uncertain_params):

        # Check that user has specified the objests to use as template
        if template_filename is None:
            msg = ("CPOEncoder must be given 'template_filename': a CPO file.")
            logging.error(msg)
            raise RuntimeError(msg)

        self.template_filename = template_filename
        self.target_filename = target_filename
        self.cpo_name = cpo_name
        self.common_dir = common_dir
        self.uncertain_params = uncertain_params

        self.fixture_support = True

        # The CPO object
        cpo_filename = os.path.join(common_dir, template_filename)
        self.cpo_core = read(cpo_filename, cpo_name)

        # Mapping with cpo file
        # TODO verify that uncertain_params key included in switcher keys
        # TODO move to switcher  tools routine
        self.mapper = {
            "Te_boundary" : self.cpo_core.te.boundary.value[0],
            "Ti_boundary" : self.cpo_core.ti.boundary.value[0][0]
        }

    # Returns dict (params) for Campaign and a list (vary) of distribitions for Sampler
    def draw_app_params(self):

        params = {}
        vary = {}

        for k, d in self.uncertain_params.items():
            # Get initial values
            val = self.mapper[k]
            typ = d["type"]

            # Build the probability distribution
            dist_name = d["distribution"]
            margin_error = d["margin_error"]
            dist = statistics.get_dist(dist_name, val, margin_error)

            # Update output dict
            params.update({k: {"type": typ, "default": val}})
            vary.update({k: dist})

        return params, vary

    # Create simulation input files
    def encode(self, params={}, target_dir='', fixtures=None):

        if fixtures is not None:
            local_params = self.substitute_fixtures_params(params, fixtures, target_dir)
        else:
            local_params = params

        if not target_dir:
            raise RuntimeError('No target directory specified to encoder')

        for k, v in local_params.items():
            if k=="Te_boundary" :
                self.cpo_core.te.boundary.value[0] = v
            else:
                self.cpo_core.ti.boundary.value[0][0] = v
            #self.mapper[k] = v

        # Do a symbolic link to other CPO and XML files
        os.system("ln -s " + self.common_dir + "* " + target_dir)

        # Write target input CPO file
        target_file_path = os.path.join(target_dir, self.target_filename)
        if(os.path.isfile(target_file_path)):
            os.system("rm -rf " + target_file_path)

        write(self.cpo_core, target_file_path)

    def get_restart_dict(self):
        return {"template_filename": self.template_filename,
                "target_filename": self.target_filename,
                "cpo_name": self.cpo_name,
                "common_dir": self.common_dir,
                "uncertain_params": self.uncertain_params}

    def element_version(self):
        return "0.1"

