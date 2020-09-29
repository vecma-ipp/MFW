import turb_coeff as tc

import xml.etree.ElementTree as ET

from distutils.util import strtobool

from turb_coeff import write_diags, write_cpos, hmode, nrho_transp, nion, thresh, beta_reduction, etae_pinch, chi_d, chiratio_phi, ra0


def get_value(param_name, xml_root, xsd_root):
    param_path = "./" + param_name.replace(".", "/")
    elem = xml_root.find(param_path)
    return elem.text

def get_value_cast_func(param_name, xml_root, xsd_root):
    # get the parameter types
    tag = xsd_root.tag.replace("schema", "element")
    elem_name = param_name.split(".")[-1]
    path = tag + "[@name='" + elem_name + "']"
    node = xsd_root.find(path)
    attr = node.attrib
    if "type" in attr:
        elem_type = attr["type"].split(":")[1]
    else: 
        elem_type = 'string'

    if elem_type == "float":
        func = lambda x: float(x)
    elif elem_type == "integer":
        func = lambda x: int(x)
    elif elem_type == "boolean":
        func = lambda x: strtobool(x)
    elif elem_type == "string":
        func = lambda x: str(x)

    return func

def assign_turb_parameters(code_parameters_filename):
    """
    calls the XML parser for the code parameters and assign the
    resulting values to the corresponding variables
    """

    #TODO check if replaceble with xml.etree
#    if IMAS == True:
#        import imas_xml_parser
#    else:
#        import euitm_xml_parser

    #TODO check what difference doe it make
#    if IMAS == True:
#        code_parameters = {}
#        return_status = 0
#        i, nparm, n_values = 0
#    else:
#        code_parameters = {}
#        return_status = 0
#        i, nparm, n_values = 0

    parameter_list = ["flags.write_cpos", "flags.write_diags", "flags.hmode", 'flags.q_choice',
                      "physical.thresh", "physical.beta_reduction", "physical.etae_pinch", "physical.chi_d", "physical.chiratio_phi",
                      "grid.nrho_transp", "grid.ra0", "grid.nion",]
    code_parameters = {}

    # initialization
    nparm = 0
    n_values = 0
    return_status = 0

    # parse xml-string code_parameters.parameters using W3C XML schema
#    if IMAS == True:
#        imas_xml_parse(code_parameters, nparm, parameter_list)
#    else:
#        euitm_xml_parse(code_parameters, nparm, parameter_list)

    parameters = ET.parse(code_parameters_filename)
    xsd_root = ET.parse(code_parameters_filename.replace(".xml", ".xsd")).getroot()

    # assign variables
    current = parameters.getroot()

    for param in parameter_list:
        param_path = "./" + param.replace(".", "/")
        elem = parameters.findall(param_path)[0].text
        cast_func = get_value_cast_func(param, parameters, xsd_root)
        code_parameters[param] = cast_func(elem)


    

# """     #cname = current.name   # necessary for AIX
#     # parameters overall
#     #if cname == "parameters":
#         current = current.child
#     # parameter classes
#     elif cname == "flags":
#         current = current.child
#     elif cname == "physical":
#         current = current.child
#     elif cname == "grid":
#         current = current.child
#     # individual parameters
#     elif cname == "write_cpos":
#         if current.cvalue != None:
#             write_cpos = current.cvalue
#     elif cname == "write_diags":
#         if current.cvalue != None:
#             write_diags = current.cvalue
#     elif cname == "hmode":
#         if current.cvalue != None:
#             hmode = current.cvalue
#     elif cname == "q_choice":
#         if current.cvalue != None:
#          q_choice = current.cvalue
#     elif cname == "thresh":
#         if current.cvalue != None:
#          thresh = current.cvalue
#     elif cname == "beta_reduction":
#         if current.cvalue != None:
#          beta_reduction = current.cvalue
#     elif cname == "etae_pinch":
#         if current.cvalue != None:
#             etae_pinch = current.cvalue
#     elif cname == "chi_d":
#         if current.cvalue != None:
#             chi_d = current.cvalue
#     elif cname == "chiratio_phi":
#         if current.cvalue != None:
#          chiratio_phi = current.cvalue
#     elif cname == "nrho_transp":
#         if current.cvalue != None:
#             nrho_tranps = current.cvalue
#     elif cname == "ra0":
#         if current.cvalue != None:
#           ra0 = current.cvalue
#     elif cname == "nion":
#         if current.cvalue != None:
#             nion = current.cvalue
#     else:
#         print('ERROR: invalid parameter {}'.format(cname))
#         return_status = 1 """

#    if associated(current.sibling)) then
#        current => current%sibling
#        exit
#    if (associated(current%parent, parameter_list%first )) &
#        exit outer
#    if (associated(current%parent)) then
#        current => current%parent
#    else
#        write(*, *) 'ERROR: broken list.'
#        return

    return code_parameters, return_status
