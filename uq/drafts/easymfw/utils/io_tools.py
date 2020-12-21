import numpy
from ascii_cpo import read
from .statistics import get_dist
from ..templates.xml_element import XMLElement
from ..templates.cpo_element import CPOElement


def input_params(cpo_file, cpo_name, input_params, ftube_index=None):
    """
    Parameters
    ----------
    cpo_file : str
        cpo file name (with whole path).
    cpo_name : str
        cpo name type, from the following list
    input_params : dict
        containting: "param_name": {"dist_name":"Normal",
                                    "var_coeff":0.2}.
    ftube_index : int
        the index of the position of a flux tube, optional/

    Returns
    -------
    dict, dict
        param for Campaign object, containing, for example:
        "param_name": {"type": float,
                       "default":100.}.
        vary (distributions list) for the Sampler, containg for exmaple:
            "param_name": cp.Normal(default_value, var_coeff*default).
    """

    cpo = CPOElement(cpo_file, cpo_name)
    params = {}
    vary = {}

    for name, attr in input_params.items():
        # Get inital value and update params
        value = cpo.get_value(name)

        # Particular case, the flux tube index is given
        if ftube_index is not None:
            value = value[ftube_index]

        attr_type = type(value)

        if attr_type in [float, numpy.float64]:
            attr_type = "float"
        elif attr_type == int:
            attr_type = "integer"
        else:
            raise RuntimeError('Unexpected parameter type.')

        params.update({name: {"type": attr_type, "default": value}})

        # get the probability distribution and update vary
        dist_name = attr["dist_name"]
        var_coeff = attr["var_coeff"]
        dist = get_dist(dist_name, value, var_coeff)
        vary.update({name: dist})

    return params, vary

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






# Get a list of indices in rho_tor_norm (in coreprof) that correspond
# to the closest rho_thor_norm of flux tubes (in coretransp).
#   corprof cpo file in one of gem0 input
#   coretransp cpo file is the gem0 outpout
def get_fluxtube_index(corep_file, coret_file):
    corep = read(corep_file, 'coreprof')
    coret = read(coret_file, 'coretransp')

    # rho_tor_norm_transp_flux
    rt = coret.values[0].rho_tor_norm
    n_flux = len(rt)
    print('rho_cores = ', rt)

    # rho_tor_norm vector in coreprof
    r = corep.rho_tor_norm

    # the closest rho_tor in coreprof
    rp = [r.flat[numpy.abs(r - rt[i]).argmin()] for i in range(n_flux)]
    print('rho_corep = ', rp)

    # the corresponding indices
    ind = [list(r).index(rp[i]) for i in range(n_flux)]
    return ind
