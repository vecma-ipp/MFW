from os.path import join
from xml.etree.ElementTree import parse
from .statistics import get_dist


# Key = uncertain paramerter names
# Value = the path in the xml file
mapper = {
    "amplitude_el": "./electrons/heating_el/WTOT_el",
    "position_el" : "./electrons/heating_el/RHEAT_el",
    "width_el"    : "./electrons/heating_el/FWHEAT_el",
    "amplitude_ion" : "./ions/heating/WTOT",
    "position_ion"  : "./ions/heating/RHEAT",
    "width_ion"     : "./ions/heating/FWHEAT"
}

# Returns dict for Campaign object and distribitions list for the Sampler
# TODO add new get_inputs from list values
def get_inputs(dirname, filename, config_dict):
    # dirname: location of xml file
    # filename: xml file name
    # config_dict: uncertain params

    xml_file = join(dirname, filename)
    xml_tree = parse(xml_file)
    root = xml_tree.getroot()
    params = {}
    vary = {}

    for k, d in config_dict.items():
        # Get initial values
        elem = root.find(mapper[k])
        val = float(elem.text)
        typ = d["type"]
        dist_name = d["distribution"]
        margin_error = d["margin_error"]

        # get the probability distribution
        dist = get_dist(dist_name, val, margin_error)

        # Update output dict
        params.update({k: {"type": typ, "default": val}})
        vary.update({k: dist})

    return params, vary
