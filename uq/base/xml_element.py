import logging
import os
from xml.etree.ElementTree import parse


class XMLElement():

    def __init__(self, xml_filename, xsd_filename=None, xml_dir=None):
        """
        Parameters
        ----------
        xml_filename : str
        xsd_filename : str
        xml_dir : str
            directory containning xml and xsd files.
        """

        if xml_filename is None:
            msg = "XMLElement must be given 'xml_filename'"
            logging.error(msg)
            raise RuntimeError(msg)

        if xsd_filename is None:
            xsd_filename = xml_filename.replace(".xml", ".xsd")

        self.xsd_file = xsd_filename
        xml_file = xml_filename

        if xml_dir is not None:
            xml_file = os.path.join(xml_dir, xml_filename)
            self.xsd_file = os.path.join(xml_dir, xsd_filename)

        # parse XML file
        self.xml_tree = parse(xml_file)
        self.xml_root = self.xml_tree.getroot()

        # reading the folder of file and its name
        self.xml_filename_full = xml_filename
        self.xml_filename_final = self.xml_filename_full.split('/')[-1]


    def get_value(self, param_name):
        """
        Parameters
        ----------
        param_name: str
            parameter name, examples:

        Returns
        -------
        float or vector.
        """

        # the element
        param_path = "./" + param_name.replace(".", "/")
        elem = self.xml_root.find(param_path)

        # the element type
        xsd_root = parse(self.xsd_file).getroot()
        tag = xsd_root.tag.replace("schema", "element")
        elem_name = param_name.split(".")[-1]
        path = tag + "[@name='" + elem_name + "']"
        node = xsd_root.find(path)
        if node is None:
            msg = "Wrong element name"
            logging.error(msg)
            raise RuntimeError(msg)

        attr = node.attrib
        if attr["type"] == "FloatList":
            elem_val = elem.text
            list_val = list(set(elem_val.split(" ")) - set([""]))
            if len(list_val) == 1:
                elem_type = "float"
            else:
                msg = "FloatList is not yet treated."
                logging.error(msg)
                raise RuntimeError(msg)
        else:
            elem_type = attr["type"].split(":")[1]

        # The element value
        if elem_type == "float":
            return float(elem.text)
        if elem_type == "integer":
            return int(elem.text)
        if elem_type == "boolean":
            return bool(elem.text)
        if elem_type == "string":
            return elem.text


    def set_value(self, param_name, value):
        """
        Parameters
        ----------
        param_name: str
            uncertain parameter or QoI name.
        value: float, int or list
            the corresponding value to set.
        """

        elem_name = "./" + param_name.replace(".", "/")
        elem = self.xml_root.find(elem_name)
        elem.text = str(value)


    def save_file(self, filename=None):
        """  Save the element into new xml file.

        Parameters
        ----------
        xml_filename : str
            the new xml filename to save.
            could be a final folder, a full psecification of path, or None
        """
        if filename.endswith('.xml'):
            self.xml_tree.write(filename)
        elif filename is not None:
            self.xml_tree.write(filename + '/' + self.xml_filename_final)
        else: 
            self.xml_tree.write(self.xml_filename_full)
