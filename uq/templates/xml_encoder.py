import os
import logging
import xml.etree.ElementTree as et
from easyvvuq import OutputType
from easyvvuq.encoders.base import BaseEncoder
from utils import xml_io


# Specific Encoder
class XMLEncoder(BaseEncoder, encoder_name="xml_encoder"):

    def __init__(self, template_filename, target_filename,
                 common_dir,
                 link_cpofiles=False, link_data=False):

        # Check that user has specified the objests to use as template
        if template_filename is None:
            msg = ("XMLEncoder must be given 'template_filename': an XML file.")
            logging.error(msg)
            raise RuntimeError(msg)

        self.template_filename = template_filename
        self.target_filename = template_filename
        self.common_dir = common_dir
        self.link_cpofiles = link_cpofiles
        self.link_data = link_data

        # To parse the XML file
        xml_file = os.path.join(common_dir, template_filename)
        self.tree = et.parse(xml_file)

    # Creates simulation input files
    def encode(self, params={}, target_dir=''):
        if not target_dir:
            raise RuntimeError('No target directory specified to encoder')

        root = self.tree.getroot()
        for key, value in params.items():
            elem = root.find(xml_io.mapper[key])
            elem.text = str(value)

        # Do a symbolic link to other CPO, XML files and restart data
        os.system("ln -s " + self.common_dir + "*.xml " + target_dir)
        os.system("ln -s " + self.common_dir + "*.xsd " + target_dir)
        if self.link_cpofiles:
            os.system("ln -s " + self.common_dir + "*.cpo " + target_dir)
        if self.link_data:
            os.system("ln -s " + self.common_dir + "*.dat " + target_dir)

        # Write target input (XML file)
        target_file_path = os.path.join(target_dir, self.target_filename)
        if(os.path.isfile(target_file_path)):
            os.system("rm -rf " + target_file_path)

        self.tree.write(target_file_path)

    def get_restart_dict(self):
        return {"template_filename": self.template_filename,
                "target_filename": self.target_filename,
                "common_dir": self.common_dir,
                "link_cpofiles": self.link_cpofiles,
                "link_data": self.link_data}

    def element_version(self):
        return "0.2"
