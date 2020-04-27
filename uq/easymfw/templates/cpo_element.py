import logging
import ual
import numpy as np
from ascii_cpo import read, write


class CPOElement():

    def __init__(self, cpo_file, cpo_name):
        # cpo_file: the cpo filename
        # cpo_name: corresponding cpo object
        if cpo_file is None:
            msg = "CPOElement must be given cpo 'cpo_file'"
            logging.error(msg)
            raise RuntimeError(msg)

         # Used names used in ITM
        cpolist = ['coreprof', 'coretransp','equilibrium',
              'coresource', 'coreimpur', 'toroidfield', 'coreneutrals']
        if cpo_name not in cpolist:
            msg = ("CPOElement must be given a right 'cpo_name'")
            logging.error(msg)
            raise RuntimeError(msg)

        self.cpo_name = cpo_name
        self.core = read(cpo_file, cpo_name)

    def get_value(self, param):
        # param: str the parameter name

        stack = param.split(".")
        field = self.core
        if self.cpo_name == "coretransp":
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

    def set_value(self, param, value, index=None):
        # param: uncertain parameter or QoI name
        # value: the corresponding value to set
        # index: list of indices to update if value is list

        # TODO check value and index size and if index are 'int' in the interval [0, len(value)[.
        # TODO add kack to update neigbours (eg. te and te.ddho, etc..)

        stack = param.split(".")
        field = self.core
        if self.cpo_name == "coretransp":
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
                        elif len(np.shape(value)) == 0:
                            new_value = np.array([value])
                        else:
                            new_value = np.array(value)

                    # update a part of the field
                    if index is None:
                        setattr(field, "value", new_value)
                    else:
                        j = 0
                        for i in index:
                            old_value[i] = new_value[j]
                            j += 1
                        setattr(field, "value", old_value)
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
                elif len(np.shape(value)) == 0:
                    new_value = np.array([value])
                else:
                    new_value = np.array(value)

                # update a part of the field
                if index is None:
                    setattr(field_base, stack[-1], new_value)
                else:
                    j = 0
                    for i in index:
                        old_value[i] = new_value[j]
                        j += 1
                    setattr(field_base, stack[-1], old_value)
            else:
                setattr(field_base, stack[-1], value)

    # Save into new file
    def save(self, filename):
        write(self.core, filename)
