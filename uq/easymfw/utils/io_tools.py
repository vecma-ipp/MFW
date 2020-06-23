import numpy
from .statistics import get_dist
from ..templates.xml_element import XMLElement
from ..templates.cpo_element import CPOElement

# TODO use one routine for both

def get_xml_inputs(xml_file, xsd_file, input_params):
    """

    input_params: dict
    eg.: {"param_name": {"dist":"Normal", "err":0.2}}

    Return two dicts:
    param for Campaign object (Encoder)
    vary (distributions list)  for the Sampler
    """

    xml = XMLElement(xml_file, xsd_file)
    params = {}
    vary = {}

    for name, attr in input_params.items():
        # get inital value and update params
        value = xml.get_value(name)

        attr_type = type(value)
        if attr_type == float:
            attr_type = "float"
        if attr_type == int:
            attr_type = "integer"
        if attr_type == list:
            attr_type = "list"

        params.update({name: {"type": attr_type, "default": value}})

        # get the probability distribution and update vary
        dist_name = attr["dist"]
        margin_error = attr["err"]
        dist = get_dist(dist_name, value, margin_error)
        vary.update({name: dist})

    return params, vary

def get_cpo_inputs(cpo_file, cpo_name, input_params):
    """
    input_params dict
    eg.: {"param_name": {"dist":"Normal", "err":0.2}}

    Return two dicts:
    param for Campaign object (Encoder)
    vary (distributions list)  for the Sampler
    """

    cpo = CPOElement(cpo_file, cpo_name)
    params = {}
    vary = {}

    for name, attr in input_params.items():
        # get inital value and update params
        value = cpo.get_value(name)
        # Select a part of input values
        if "idx" in attr.keys():
            indices = attr["idx"]
            if len(indices) == 1:
                value = value[indices[0]]
            else:
                new_value = []
                for i in indices:
                    new_value.append(value[i])
                value = new_value

        attr_type = type(value)

        if attr_type in [float, numpy.float64]:
            attr_type = "float"
        if attr_type == int:
            attr_type = "integer"
        if attr_type == list:
            attr_type = "list"

        params.update({name: {"type": attr_type, "default": value}})

        # get the probability distribution and update vary
        dist_name = attr["dist"]
        margin_error = attr["err"]
        dist = get_dist(dist_name, value, margin_error)
        vary.update({name: dist})

    return params, vary
