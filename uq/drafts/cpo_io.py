import logging
from os.path import join
from ascii_cpo import read
from .statistics import get_dist
from .cpo_tools import update_te, update_ti, update_te_grad, update_ti_grad


''' To be improved: cf. UAL module and do it in generic way
'''

# Get the initial value of the given params
def get_parameters(cpo_core, flux_index):
    params_mapper={}

    if cpo_core.base_path == "coreprof":
        corep_params = {
            "te_boundary" : cpo_core.te.boundary.value[0],
            "ti_boundary" : cpo_core.ti.boundary.value[0][0],
            "te" : cpo_core.te.value[flux_index],
            "ti" : cpo_core.ti.value[flux_index][0],
            "te_grad" : cpo_core.te.ddrho[flux_index],
            "ti_grad" : cpo_core.ti.ddrho[flux_index][0]
        }
        params_mapper.update(corep_params)

    if cpo_core.base_path == "coretransp":
        coret_params = {
            "te_transp_diff_eff": cpo_core.values[0].te_transp.diff_eff,
            "ti_transp_diff_eff": cpo_core.values[0].ti_transp.diff_eff[0]
        }
        params_mapper.update(coret_params)

    return params_mapper

# Set the given value of the input params
def set_parameters(cpo_core, param, value, flux_index=0):
        # Boundary conditions
        if param=="te_boundary":
            cpo_core.te.boundary.value[0] = value
        if param=="ti_boundary":
            cpo_core.ti.boundary.value[0][0] = value
            # In case of two ions species
            if len(cpo_core.ti.boundary.value[0]) == 2:
                cpo_core.ti.boundary.value[0][1] = value
        # Temperature at given flux index
        if param=="te":
            cpo_core.te.value[flux_index] = value
            update_te(cpo_core, value, flux_index)
        if param=="ti":
            cpo_core.ti.value[flux_index][0] = value
            update_ti(cpo_core, value, flux_index)
        if param=="te_grad":
            cpo_core.te.ddrho[flux_index] = value
            update_te_grad(cpo_core, value, flux_index)
        if param=="ti_grad":
            cpo_core.ti.ddrho[flux_index][0] = value
            update_ti_grad(cpo_core, value, flux_index)

# Get the values of the correponding quantities of interest
def get_qoi(cpo_core):
    qoi_mapper = {}

    if cpo_core.base_path == 'coreprof':
        coreprof_qoi = {
            "te": cpo_core.te.value,
            "ti": cpo_core.ti.value[:,0],
            "ne": cpo_core.ne.value,
            "ni": cpo_core.ni.value[:,0],
            "psi": cpo_core.ne.value,
            "q":  cpo_core.profiles1d.q
        }
        qoi_mapper.update(coreprof_qoi)

    if cpo_core.base_path == 'equilibrium':
        equil_qoi = {
            "gm1": cpo_core.profiles_1d.gm1,
            "gm2": cpo_core.profiles_1d.gm2,
            "gm3": cpo_core.profiles_1d.gm3,
            "gm4": cpo_core.profiles_1d.gm4,
            "gm5": cpo_core.profiles_1d.gm5,
            "gm6": cpo_core.profiles_1d.gm6,
            "gm7": cpo_core.profiles_1d.gm7,
            "gm8": cpo_core.profiles_1d.gm8,
            "gm9": cpo_core.profiles_1d.gm9,
        }
        qoi_mapper.update(equil_qoi)

    if cpo_core.base_path == 'coretransp':
        coretransp_qoi = {
            "te_transp_diff_eff": cpo_core.values[0].te_transp.diff_eff,
            "ti_transp_diff_eff": cpo_core.values[0].ti_transp.diff_eff[0],
            "te_transp_flux": cpo_core.values[0].te_transp.flux,
            "ti_transp_flux": cpo_core.values[0].ti_transp.flux[:,0]
        }
        qoi_mapper.update(coretransp_qoi)

    return qoi_mapper

# Get the cponame from the input filename
def get_cponame(filename):
    # List of cpo names used in ITM
    l = ['coreprof', 'coresource', 'coretransp', 'equilibrium',
         'coreimpur', 'toroidfield', 'coreneutrals']

    cponame = None
    for name in l:
        if name in filename:
            cponame = name
            break

    if cponame is None:
        msg = ("CPO filename " +filename+ " must contain cponame.")
        logging.error(msg)
        raise RuntimeError(msg)
    else:
        return cponame

#
def param2field(cpo, param):
    stack = param.split(".")
    field = cpo
    for attr in stack:
        field = getattr(field, attr)
    return field

# Returns dict for Campaign object and distribitions list for the Sampler
def get_inputs(cpo_file, config_dict):
    # dirname: location of cpo file
    # filename: cpo file name
    # config_dict: containg uncertrain params

    cpo_name = get_cponame(cpo_file)
    cpo_core = read(cpo_file, cpo_name)

    params = {}
    vary = {}

    for name, sub_dict in config_dict.items():
        field = param2field(cpo_core, name)
        typ = sub_dict["type"]
        if typ == "scalar":
            val = field.value[0]
        elif typ == "vector":
            val = field.value
        else:
            msg = "Wrong parameter type: " + typ
            logging.error(msg)
            raise RuntimeError(msg)

        # Build the probability distribution
        dist_name = sub_dict["dist"]
        margin_error = sub_dict["moe"]
        dist = get_dist(dist_name, val, margin_error)

        # Update output dict
        params.update({field: {"type": typ, "default": val}})
        vary.update({field: dist})

    return params, vary
