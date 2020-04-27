import logging
from xml.etree.ElementTree import parse


class XMLElement():
    # parse XML file
    def __init__(self, xml_file, xsd_file=None):
        """
        xml_file: the xml filename with the full path
        xsd_file: the xsd filename with the full path
        """

        if xml_file is None:
            msg = "XMLElement must be given xml 'filename'"
            logging.error(msg)
            raise RuntimeError(msg)

        if xsd_file is None:
            xsd_file = xml_file.replace(".xml", ".xsd")

        self.xsd_file = xsd_file

        self.xml_tree = parse(xml_file)
        self.xml_root = self.xml_tree.getroot()

    # get the value of the given param input
    def get_value(self, param_name):
        """
        param_name: str the parameter name
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
        param_name: str, uncertain parameter or QoI name
        value: the corresponding value to set
        """

        elem_name = "./" + param_name.replace(".", "/")
        elem = self.xml_root.find(elem_name)

        elem.text = str(value)

    # Save into new xml file
    def save(self, xml_filename):
        """
        xml_filename: the new xml filename with the full path
        """
        self.xml_tree.write(xml_filename)
