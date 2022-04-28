import os
import logging
from easyvvuq import OutputType
#from easyvvuq.encoders.base import BaseEncoder
from .xml_element import XMLElement

# Specific Encoder for XML files
#class XMLEncoder(BaseEncoder, encoder_name="xml_encoder"):
class XMLEncoder:

    def __init__(self, xml_filename, input_dir,
                 input_params=None, target_filename=None):

        # Check that user has specified the object to use as xml
        if xml_filename is None:
            msg = ("XMLEncoder must be given 'xml_filename'.")
            logging.error(msg)
            raise RuntimeError(msg)

        self.xml_filename = xml_filename
        self.input_dir = input_dir
        self.input_params = input_params

        if target_filename is None:
            self.target_filename = xml_filename
        else:
            self.target_filename = target_filename

        # To parse xml file
        self.xml = XMLElement(xml_filename, xml_dir=input_dir)

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
        os.system("ln -s " + self.input_dir + "*.xml " + target_dir + " 2>/dev/null")
        os.system("ln -s " + self.input_dir + "*.xsd " + target_dir + " 2>/dev/null")
        os.system("ln -s " + self.input_dir + "*.cpo " + target_dir + " 2>/dev/null")
        for fname in os.listdir(self.input_dir):
            if fname.endswith(".dat"):
                os.system("ln -s " + self.input_dir + fname + " " + target_dir + " 2>/dev/null")

        # Write target input (XML file)
        target_file_path = os.path.join(target_dir, self.target_filename)
        if(os.path.isfile(target_file_path)):
            os.system("rm " + target_file_path)
        self.xml.save_file(target_file_path)


    def get_restart_dict(self):
        return {"xml_filename": self.xml_filename,
                "target_filename": self.target_filename,
                "input_dir": self.input_dir,
                "input_params": self.input_params}

    def element_version(self):
        return "0.5"
