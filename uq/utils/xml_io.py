from logging import Logger
from os.path import splitext
from xml.etree.ElementTree import parse
from .statistics import get_dist

logger = Logger(__name__)


# Key = uncertain paramerter names
# Value = the path in the xml file
xml_mapper = {
    "amplitude_el": "./electrons/heating_el/WTOT_el",
    "position_el" : "./electrons/heating_el/RHEAT_el",
    "width_el"    : "./electrons/heating_el/FWHEAT_el",
    "amplitude_ion" : "./ions/heating/WTOT",
    "position_ion"  : "./ions/heating/RHEAT",
    "width_ion"     : "./ions/heating/FWHEAT"
}

# Returns dict for Campaign object and distribitions list for the Sampler
def get_xml_inputs(filename, config_dict):
    # filename: xml file name
    # config_dict: uncertain params

    ext = splitext(filename)[-1].lower()
    if ext == ".xml":
        xml_tree = parse(filename)
        root = xml_tree.getroot()
    else:
        logger.error("Not xml file format.")
        raise Exception(msg)

    for k, d in config_dict.items():
        # Get initial values
        elem = root.find(xml_mapper[k])
        val = float(elem.text)
        typ = d["type"]
        dist_name = d["distribution"]
        margin_error = d["margin_error"]

        # get the probability distribution
        dist = get_dist(dist_name, val, margin_error=marging_error)

        # Update output dict
        params.update({k: {"type": typ, "default": val}})
        vary.update({k: dist})

    return params, vary
