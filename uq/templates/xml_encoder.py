# -*- coding: UTF-8 -*-
import os
import logging
import chaospy as cp
import pandas as pd
import xml.etree.ElementTree as et
from easyvvuq import OutputType
from easyvvuq.encoders.base import BaseEncoder
from utils import statistics


# Specific Encoder
class XMLEncoder(BaseEncoder, encoder_name="xml_encoder"):

    def __init__(self, template_filename, target_filename,
                 common_dir, uncertain_params, link_cpofiles=False):

        # Check that user has specified the objests to use as template
        if template_filename is None:
            msg = ("XMLEncoder must be given 'template_filename': an XML file.")
            logging.error(msg)
            raise RuntimeError(msg)

        self.template_filename = template_filename
        self.target_filename = template_filename
        self.common_dir = common_dir
        self.uncertain_params = uncertain_params
        self.link_cpofiles = link_cpofiles

        self.fixture_support = True

        # To parse the XML file
        xml_file = os.path.join(common_dir, template_filename)
        self.tree = et.parse(xml_file)

        # Mapping with xml file
        # TODO verify that uncertain_params key included in switcher keys
        # TODO move to switcher in tools routine
        self.mapper = {
            "amplitude_el": "./electrons/heating_el/WTOT_el",
            "position_el" : "./electrons/heating_el/RHEAT_el",
            "width_el"    : "./electrons/heating_el/FWHEAT_el",
            "amplitude_ion" : "./ions/heating/WTOT",
            "position_ion"  : "./ions/heating/RHEAT",
            "width_ion"     : "./ions/heating/FWHEAT"
        }

    # Return param dict for Campagn and list of distribitions for Sampler
    def draw_app_params(self):

        params = {}
        vary = {}
        root = self.tree.getroot()

        for k, d in self.uncertain_params.items():
            # Get initial values
            elem = root.find(self.mapper[k])
            val = float(elem.text)
            typ = d["type"]
            dist_name = d["distribution"]
            margin_error = d["margin_error"]

            # get the probability distribution
            dist = statistics.get_dist(dist_name, val, margin_error)

            # Update output dict
            params.update({k: {"type": typ, "default": val}})
            vary.update({k: dist})

        return params, vary

    # Creates simulation input files
    def encode(self, params={}, target_dir='', fixtures=None):

        root = self.tree.getroot()
        if fixtures is not None:
            local_params = self.substitute_fixtures_params(params, fixtures, target_dir)
        else:
            local_params = params

        if not target_dir:
            raise RuntimeError('No target directory specified to encoder')

        for k in self.uncertain_params.keys():
            v = local_params[k]
            elem = root.find(self.mapper[k])
            elem.text = str(v)

        # Do a symbolic link to other CPO and XML files
        os.system("ln -s " + self.common_dir + "*.xml " + target_dir)
        os.system("ln -s " + self.common_dir + "*.xsd " + target_dir)
        if self.link_cpofiles:
            os.system("ln -s " + self.common_dir + "*.cpo " + target_dir)

        # Write target input (XML file)
        target_file_path = os.path.join(target_dir, self.target_filename)
        if(os.path.isfile(target_file_path)):
            os.system("rm -rf " + target_file_path)

        self.tree.write(target_file_path)

    def get_restart_dict(self):
        return {"template_filename": self.template_filename,
                "target_filename": self.target_filename,
                "common_dir": self.common_dir,
                "uncertain_params": self.uncertain_params,
                "link_cpofiles": self.link_cpofiles}

    def element_version(self):
        return "0.1"
