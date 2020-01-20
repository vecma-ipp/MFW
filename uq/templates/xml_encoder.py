import os
import logging
from easyvvuq import OutputType
from easyvvuq.encoders.base import BaseEncoder
from xml.etree.ElementTree import parse
from utils import xml_io


# Specific Encoder
class XMLEncoder(BaseEncoder, encoder_name="xml_encoder"):

    def __init__(self, template_filename, target_filename,
                 common_dir, params_names=None):
        # Check that user has specified the object to use as template
        if template_filename is None:
            msg = ("XMLEncoder must be given 'template_filename': an XML file.")
            logging.error(msg)
            raise RuntimeError(msg)

        self.template_filename = template_filename
        self.target_filename = template_filename
        self.common_dir = common_dir
        self.params_names = params_names

        # To parse the XML file
        xml_file = os.path.join(common_dir, template_filename)
        self.tree = parse(xml_file)

    # Creates simulation input files
    def encode(self, params={}, target_dir=''):
        if not target_dir:
            raise RuntimeError('No target directory specified to encoder')

        if self.params_names is None:
            self.params_names = params.keys()

        root = self.tree.getroot()
        for key in self.params_names:
            elem = root.find(xml_io.mapper[key])
            value = params[key]
            elem.text = str(value)

        # Write target input (XML file)
        target_file_path = os.path.join(target_dir, self.target_filename)
        if(os.path.isfile(target_file_path)):
            os.system("rm " + target_file_path)
        self.tree.write(target_file_path)

        # Do a symbolic link to other files (cpo, xml and restart data)
        os.system("ln -s " + self.common_dir + "* " + target_dir + " >/dev/null 2>&1")

    def get_restart_dict(self):
        return {"template_filename": self.template_filename,
                "target_filename": self.target_filename,
                "common_dir": self.common_dir,
                "params_names": self.params_names}

    def element_version(self):
        return "0.2"
