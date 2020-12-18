import logging
import numpy as np
import chaospy as cp
from .cpo_element import CPOElement
from .xml_element import XMLElement

__all__ = ['get_dist', 'xml_inputs', 'cpo_inputs', 'ftube_indices']


def get_dist(name, value, err):
    """
    Return distribition:
     - Moments: mean = value, sdt = err*value.
     - Type: given by 'name' (supported: Normal and Uniform).
     - Dimension: univariate if value is scalar, multivariate if value is a list.

    Parameters
    ----------
    name : str
        The distribution name
    value : float, int or list
        The mean value of the distribution.
    err  : float

    Returns
    -------
    chaospy.Dist: the output distribution.
    """

    # TODO add the condition: shift if lower threshlod <= a critical value
    # => for verification: Values must be > 0
    if name.lower() == "normal":
            if type(value) == list:
                d = []
                for v in value:
                    if v == 0.:
                        dv = cp.Normal(v, err)
                    else:
                        dv = cp.Normal(v, err*v)
                    d.append(dv)
                dist = cp.J(*d)
            else:
                if value == 0.:
                    dist = cp.Normal(value, err)
                else:
                    dist = cp.Normal(value, err*value)

    elif name.lower() == "uniform":

        if type(value) == list:
            d = []
            for v in value:
                lo = (1. - np.sqrt(3)*err)*v
                up = (1. + np.sqrt(3)*err)*v
                if v == 0.:
                    up = err

                d.append(cp.Uniform(lo, up))
            dist = cp.J(*d)
        else:
            lo = (1. - np.sqrt(3)*err)*value
            up = (1. + np.sqrt(3)*err)*value
            if value == 0.:
                up = err
            dist = cp.Uniform(lo, up)

    # TODO add other relevant distributions
    else:
        msg = "Unknown distribution name: " + dist_name
        logging.error(msg)
        raise Exception(msg)

    return dist


def xml_inputs(xml_filename, xsd_filename, input_dir, input_params):
    """ Get UQ inputs (params and vary) used by EasyVVUQ
    Parameters
    ----------
    xml_filename : str
    xsd_filename : str
    input_dir : str
        directory containing xml and xsd files.
    input_params : dict
        containing: {key: value} like:
        {"param_name": {"dist_name":"Normal", "var_coeff":0.2}}

    Returns
    -------
    dict
        param for Campaign object, containing for example:
        "param_name": {"type": float, "default":100.}.
    dict
        vary (distributions list) for the Sampler, containg for exmaple:
            "param_name": cp.Normal(default_value, var_coeff*default).
    """

    xml = XMLElement(xml_filename, xsd_filename, input_dir)
    params = {}
    vary = {}

    for name, attr in input_params.items():
        # get inital value and update params
        value = xml.get_value(name)

        attr_type = type(value)
        if attr_type == float:
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


def cpo_inputs(cpo_filename, cpo_name, input_dir, input_params, ftube_index=None):
    """ Get UQ inputs (params and vary) used by EasyVVUQ
    Parameters
    ----------
    cpo_filename : str
    cpo_name : str
    input_params : dict
        containting: "param_name": {"dist_name":"Normal", "var_coeff":0.2}.
    ftube_index : int
        the index of the position of a flux tube (optional)

    Returns
    -------
    dict
        param for Campaign object, containing for example:
        "param_name": {"type": float, "default":100.}.
    dict
        vary (distributions list) for the Sampler, containg for exmaple:
            "param_name": cp.Normal(default_value, var_coeff*default).
    """

    cpo = CPOElement(cpo_filename, cpo_name, input_dir)
    params = {}
    vary = {}

    for name, attr in input_params.items():
        # Get inital value and update params
        value = cpo.get_value(name)

        # Particular case, the flux tube index is given
        if ftube_index is not None:
            value = value[ftube_index]

        attr_type = type(value)

        if attr_type in [float, np.float64]:
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


def ftube_indices(corep_file, coret_file):
    """ Get a list of indices in rho_tor_norm grid (from coreprof) which
    correspond to the closest rho_thor_norm of flux tubes (in coretransp).

    Parameters
    ----------
    corep_file : str
        coreprof file.
    coret_file : str
        coretransp file.

    Returns
    -------
    list
       contain flux tube indices.
    """

    from ascii_cpo import read
    corep = read(corep_file, 'coreprof')
    coret = read(coret_file, 'coretransp')

    # rho_tor_norm_transp_flux
    rt = coret.values[0].rho_tor_norm
    n_flux = len(rt)
    print('rho_cores = ', rt)

    # rho_tor_norm vector in coreprof
    r = corep.rho_tor_norm

    # the closest rho_tor in coreprof
    rp = [r.flat[np.abs(r - rt[i]).argmin()] for i in range(n_flux)]
    print('rho_corep = ', rp)

    # the corresponding indices
    ind = [list(r).index(rp[i]) for i in range(n_flux)]
    return ind
