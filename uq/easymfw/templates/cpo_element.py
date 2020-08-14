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
            msg = ("CPOElement: wrong 'cpo_name'")
            logging.error(msg)
            raise RuntimeError(msg)

        self.cpo_name = cpo_name
        self.core = read(cpo_file, cpo_name)

    def get_value(self, param):
        # param: str the parameter name
        # examples: "te.boundary.value", "te.value", "values[0].te_transp.diff_eff"

        # Number of ion species
        nion = 1
        if self.cpo_name in ["coreprof", 'coretransp', 'coresource']:
            nion = len(self.core.compositions.ions)

        field = self.core
        if self.cpo_name == 'coretransp':
            field = self.core.values[0]

        stack = param.split(".")
        for attr in stack:
            field = getattr(field, attr)

        # take into accout some particular cases
        if "boundary" in stack:
            if "ti" in stack and nion == 1:
                value = field[0][0]
            else:
                value = field[0]
        elif "ti" in stack or "ni" in stack or "ti_transp" in stack:
            if nion == 1:
                value = list(field.T[0])
            else:
                value = [list(field.T[0]), list(field.T[1])]
        else:
            value = field

        if type(value) == np.ndarray:
            value = list(value)

        return value

    def set_value(self, param, value, index=None):
        # Number of ion species
        nion = 1
        if self.cpo_name in ["coreprof", 'coretransp', 'coresource']:
            nion = len(self.core.compositions.ions)

        # the root
        field_value = self.core
        if self.cpo_name == 'coretransp':
            field_value = self.core.values[0]

        stack = param.split(".")
        for attr in stack:
            field_base = field_value
            field_value = getattr(field_value, attr)

        if index is None:
            new_value = value
        else:
            # TODO check value and index have the same sizes, and
            # if index is composed by int in [0, size(field_value)-1]
            new_value = self.get_value(param)
            j = 0
            for i in index:
                new_value[i] = value[j]
                j += 1

        # take into accout some particular cases
        if "boundary" in stack:
            if "ti" in stack and nion == 1:
                field_value[0][0] = new_value
            else:
                field_value[0] = new_value
        elif "ti" in stack or "ni" in stack or "ti_transp" in stack:
            if nion == 1:
                field_value = np.array(new_value)
            else:
                field_value.T[0] = np.array(new_value[0])
                field_value.T[1] = np.array(new_value[1])
        else:
            if type(field_value) == np.ndarray:
                field_value = np.array(new_value)
            else:
                field_value = new_value

        setattr(field_base, stack[-1], field_value)


    # Save into new file
    def save(self, filename):
        write(self.core, filename)
