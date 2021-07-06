import logging
import numpy as np
import chaospy as cp
from .cpo_element import CPOElement
from .xml_element import XMLElement

__all__ = ['get_dist', 'xml_inputs', 'cpo_inputs', 'ftube_indices']


def get_dist(name, value, err):
    """
    Parameters
    ----------
    name : str
        The distribution name
    value : float or int
        The mean value of the dist.
    err  : float
        The variation error (>0 and <=1)

    Returns
    -------
    chaospy.Dist: the output distribution.
    """

    if name.lower() == "normal":
        if value == 0.:
            dist = cp.Normal(value, err/3.)
        else:
            dist = cp.Normal(value, err*np.abs(value)/3.)

    elif name.lower() == "uniform":
        if value == 0.:
            dist = cp.Uniform(0., err)
        elif value > 0.:
            dist = cp.Uniform((1. - err)*value, (1. + err)*value)
        else :
            dist = cp.Uniform((1. + err)*value, (1. - err)*value)

    # TODO add other distributions
    else:
        msg = "Unknown distribution name: " + name
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
        {"param_name": {"dist":"Normal", "err":0.2}, "min":100., "max":2000}
        (min and max are optional)

    Returns
    -------
    dict
        param for Campaign object, containing for example:
        "param_name": {"type": float, "default":100.}.
    dict
        vary (distributions list) for the Sampler, containg for exmaple:
            "param_name": cp.Normal(default_value, err*default).
    """

    xml_elem = XMLElement(xml_filename, xsd_filename, input_dir)
    params, vary = _input_dicts(xml_elem, input_params)
    return params, vary


def cpo_inputs(cpo_filename, cpo_name, input_dir, input_params, ftube_index=None):
    """ Get UQ inputs (params and vary) used by EasyVVUQ
    Parameters
    ----------
    cpo_filename : str
        the cpo filename
    cpo_name : str
        the cpo name, from the following list:
            'coreprof', 'coretransp','equilibrium', 'coresource',
            'coreimpur', 'toroidfield' and 'coreneutrals'.
    input_params : dict
        containnig {key: value} like:
        {"param_name": {"dist":"Normal", "err":0.2}, "min":100., "max":2000}
        (min and max are optional)
    ftube_index : int
        the index of the position of a flux tube (optional)

    Returns
    -------
    dict
        param for Campaign object, containing for example:
        "param_name": {"type": float, "default":100.}.
    dict
        vary (distributions list) for the Sampler, containg for exmaple:
            "param_name": cp.Normal(default_value, err*default).
    """

    cpo_elem = CPOElement(cpo_filename, cpo_name, input_dir)
    params, vary = _input_dicts(cpo_elem, input_params, ftube_index)
    return params, vary


def _input_dicts(elem, input_params, ftube_index=None):

    params = {}
    vary = {}

    for name, attr in input_params.items():
        # get inital value (default) and update params
        value = elem.get_value(name)

        # Particular case, the flux tube index is given
        if ftube_index is not None:
            value = value[ftube_index]

        attr_type = type(value)
        if attr_type in [np.float64,  float]:
            attr_type = "float"
        elif attr_type in [np.int, int]:
            attr_type = "integer"
        else:
            raise RuntimeError('Unexpected parameter type.')

        d = {"type": attr_type, "default": value}
        if 'min' in attr:
            d.update({'min': attr['min']})
        if 'max' in attr:
            d.update({'max': attr['max']})
        params.update({name: d})

        # get the probability distribution and update vary
        dist_name = attr["dist"]
        err = attr["err"]
        dist = get_dist(dist_name, value, err)
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
