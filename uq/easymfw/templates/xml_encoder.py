import os
import logging
from easyvvuq import OutputType
from easyvvuq.encoders.base import BaseEncoder
from .xml_element import XMLElement


# Specific Encoder for XML files
class XMLEncoder(BaseEncoder, encoder_name="xml_encoder"):

    def __init__(self, template_filename, target_filename,
                 input_params, common_dir) :

        # Check that user has specified the object to use as template
        if template_filename is None:
            msg = ("XMLEncoder must be given 'template_filename': an XML file.")
            logging.error(msg)
            raise RuntimeError(msg)

        self.template_filename = template_filename
        self.target_filename = template_filename
        self.input_params = input_params
        self.common_dir = common_dir

        # Parsing xml file
        xml_file = os.path.join(common_dir, template_filename)
        self.xml = XMLElement(xml_file)

    # Creates simulation input files
    def encode(self, params={}, target_dir=''):
        if not target_dir:
            raise RuntimeError('No target directory specified to encoder')

        if self.input_params is None:
            self.input_params = params

        for name in self.input_params.keys():
            value = params[name]
            self.xml.set_value(name, value)

        # Do a symbolic link to other files (cpo, xml and restart data)
        os.system("ln -s " + self.common_dir + "*.xml " + target_dir)
        os.system("ln -s " + self.common_dir + "*.xsd " + target_dir)
        os.system("ln -s " + self.common_dir + "*.cpo " + target_dir)
        for fname in os.listdir(self.common_dir):
            if fname.endswith(".dat"):
                os.system("ln -s " + self.common_dir + fname + " " + target_dir)

        # Write target input (XML file)
        target_file_path = os.path.join(target_dir, self.target_filename)
        if(os.path.isfile(target_file_path)):
            os.system("rm " + target_file_path)
        self.xml.save(target_file_path)


    def get_restart_dict(self):
        return {"template_filename": self.template_filename,
                "target_filename": self.target_filename,
                "common_dir": self.common_dir,
                "input_params": self.input_params}

    def element_version(self):
        return "0.4"
