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

    def get_value(self, param):
        # param: str the parameter name

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
                msg = "CPOElement: check 'param' name in get_value"
                logging.error(msg)
                raise RuntimeError(msg)
        else:
            value = field

        # Needed for sampling
        if type(value) == np.ndarray:
            shape = np.shape(value)
            # Particular case
            if "boundary" in stack:
                if len(shape) == 1:
                    value = value[0]
                else:
                    if shape[1] == 1:
                        value = float(value[0])
                    else:
                        value = list(value[0])
            else:
                # Element with two spices
                if len(shape) == 2:
                    if shape[1] == 1:
                        value = list(value.T[0])
                    if shape[1] == 2:
                        value = [list(value[i]) for i in range(len(value))]
                else:
                    value = list(value)

        return value

    def set_value(self, param, value):
        # param: uncertain parameter or QoI name
        # value: the corresponding value to set

        stack = param.split(".")
        field = self.core
        if self.cponame == "coretransp":
            field = self.core.values[0]

        for attr in stack:
            field_base = field
            field = getattr(field, attr)

        if type(field).__base__ == ual.ualdef.KeepInOrder:
            if hasattr(field, "value"):
                old_value = field.value
                if type(old_value) == np.ndarray:
                    new_value = old_value
                    shape = np.shape(new_value)
                    # Particular case
                    if "boundary" in stack:
                        if len(shape) == 1:
                            new_value[0] = value
                        else:
                            if shape[1] == 1:
                                new_value[0] = value
                            else:
                                new_value[0] = np.array(value)
                    else:
                        # Element with two spieces
                        if len(shape) == 2 and shape[1] == 2:
                            new_value.T[0] = np.array(value[0])
                            new_value.T[1] = np.array(value[1])
                        else:
                            new_value = np.array(value)
                    setattr(field, "value", new_value)
                else:
                    setattr(field, "value", value)
            else:
                msg = "CPOElement: check 'param' name in set_value"
                logging.error(msg)
                raise RuntimeError(msg)
        else:
            old_value = getattr(field_base, stack[-1])
            if type(old_value) == np.ndarray:
                new_value = old_value
                shape = np.shape(new_value)
                # Element with two spices
                if len(shape) == 2 and shape[1] == 2:
                    new_value.T[0] = np.array(value[0])
                    new_value.T[1] = np.array(value[1])
                else:
                    new_value = np.array(value)
                setattr(field, stack[-1], new_value)
            else:
                setattr(field, stack[-1], value)

    # Save into new file
    def save(self, filename):
        write(self.core, filename)


# Return two dicts:
#    param for Campaign object (Encoder)
#    vary (distributions list)  for the Sampler
def get_inputs(cpo_file, cpo_name, input_params):
    # input_params dict
    # eg.: {"param_name": {"dist":"Normal", "err":0.2}}

    cpo = CPOElement(cpo_file, cpo_name)
    params = {}
    vary = {}

    for name, attr in input_params.items():
        # Get initial value
        value = cpo.get_value(name)
        if type(value)==list:
            attr_type = "list"
        else:
            attr_type = "float"
        params.update({name: {"type": attr_type, "default": value}})

        # Build the probability distribution
        dist_name = attr["dist"]
        margin_error = attr["err"]

        dist = get_dist(dist_name, value, margin_error)
        vary.update({name: dist})

    return params, vary
