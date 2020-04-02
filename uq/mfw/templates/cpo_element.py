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
        self.cponame = cponame
        self.core = read(filename, cponame)

    def get_value(self, param, index=None):
        # param: str the parameter name
        # index: integer

        stack = param.split(".")
        field = self.core
        if self.cponame == "coretransp":
            field = self.core.values[0]

        for attr in stack:
            field = getattr(field, attr)

        if type(field).__base__ == ual.ualdef.KeepInOrder:
            if hasattr(field, "value"):
                value = field.value
            else:
                msg = "CPOElement.get_value check 'param' name"
                logging.error(msg)
                raise RuntimeError(msg)
        else:
            value = field

        if type(value) == np.ndarray:
            value = value.T
            if index is not None:
                value = value[index]

        return value

    def set_value(self, param, value, index=None):
        # param: uncertain parameter or QoI name
        #        example: "te.boundary"
        # value: the corresponding value to set
        # index: if value is element of an array

        stack = param.split(".")
        field = self.core
        if self.cponame == "coretransp":
            field = self.core.values[0]

        for attr in stack:
            field_base = field
            field = getattr(field, attr)

        if type(field).__base__ == ual.ualdef.KeepInOrder:
            if hasattr(field, "value"):
                old_value = getattr(field, "value")
                if type(old_value) == np.ndarray:
                    if index is None:
                        new_value = value
                    else:
                        new_value = old_value
                        new_value[index] = value
                    setattr(field, "value", new_value.T)
                else:
                    setattr(field, "value", value)
            else:
                msg = "CPOElement.set_value check 'param' name"
                logging.error(msg)
                raise RuntimeError(msg)

        else:
            old_value = getattr(field_base, stack[-1])
            if type(old_value) == np.ndarray:
                new_value = value.T
                setattr(field_base, stack[-1], new_value)
            else:
                setattr(field_base, stack[-1], value)

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
        dist_name = attr["dist"]
        margin_error = attr["err"]
        dist = get_dist(dist_name, value, margin_error)

        # Update output dict
        params.update({name: {"type": attr_type, "default": value}})
        vary.update({name: dist})

    return params, vary
