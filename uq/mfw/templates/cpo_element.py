import logging
import ual
import numpy as np
from ascii_cpo import read, write
from ..utils.statistics import get_dist


class CPOElement():

    def __init__(self, filename, cponame):
        # filename: the cpo filename
        # cponame: corresponding cpo object
        if filename is None:
            msg = "CPOElement must be given cpo 'filename'"
            logging.error(msg)
            raise RuntimeError(msg)

         # Used names used in ITM
        cpolist = ['coreprof', 'coretransp','equilibrium',
              'coresource', 'coreimpur', 'toroidfield', 'coreneutrals']
        if cponame not in cpolist:
            msg = ("CPOElement must be given a right 'cponame'")
            logging.error(msg)
            raise RuntimeError(msg)

        self.filename = filename
        self.core = read(filename, cponame)

    def get_value(self, param, index=0):
        # param: the parameter name
        # index: if return value is element of an array

        stack = param.split(".")
        field = self.core
        for attr in stack:
            field = getattr(field, attr)

        if type(field).__base__ == ual.ualdef.KeepInOrder:
            value = field.value
        else:
            value = field

        if type(value) == np.ndarray:
            value = value.flatten()

        return value

    def set_value(self, param, value, index=0):
        # param: uncertain parameter or QoI name
        # value: the corresponding value to set
        # index: if value is element of an array

        stack = param.split(".")
        field = self.core
        for attr in stack:
            field_base = field
            field = getattr(field, attr)

        if type(field).__base__ == ual.ualdef.KeepInOrder:
            old_value = getattr(field, "value")
            if type(old_value) == np.ndarray:
                new_value = old_value
                if len(np.shape(new_value)) == 1:
                    if type(value) == np.ndarray:
                        new_value[:] = value
                    else:
                        new_value[index] = value
                else:
                    if type(value) == np.ndarray:
                        new_value[:][0] = value
                    else:
                        new_value[index][0] = value
            setattr(field, "value", new_value)

        else:
            old_value = getattr(field_base, stack[-1])
            if type(old_value) == np.ndarray:
                new_value = old_value
                if len(np.shape(new_value)) == 1:
                    if type(value) == np.ndarray:
                        new_value[:] = value
                    else:
                        new_value[index] = value
                else:
                    if type(value) == np.ndarray:
                        new_value[:][0] = value
                    else:
                        new_value[index][0] = value
            setattr(field_base, stack[-1], new_value)

    def save(self, filename):
        write(self.core, filename)


# Return two dict:
#    param for Campaign object (Encoder)
#    vary (distributions list)  for the Sampler
def get_inputs(cpo_file, cpo_name, input_params):
    # input_params dict
    # eg.: {"param_name": {"type":"float", "dist":"Normal", "err":0.2}}

    cpo = CPOElement(cpo_file, cpo_name)
    params = {}
    vary = {}
    index = 0 # TODO move it to input_params dict

    for name, attr in input_params.items():
        # get initial value
        value = cpo.get_value(name, index)

        # Build the probability distribution
        attr_type = attr["type"]
        if attr_type == "float":
            if type(value) == np.ndarray:
               value = float(value[index])
        else:
            print("not yet implemented")

        dist_name = attr["dist"]
        margin_error = attr["err"]
        dist = get_dist(dist_name, value, margin_error)

        # Update output dict
        params.update({name: {"type": attr_type, "default": value}})
        vary.update({name: dist})

    return params, vary
