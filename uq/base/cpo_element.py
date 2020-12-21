import logging
import os
import numpy as np
from ascii_cpo import read, write


class CPOElement():

    def __init__(self, cpo_filename, cpo_name, cpo_dir=None):
        """
        Parameters
        ----------
        cpo_filename : str
            cpo filename.
        cpo_name : str
            cpo name type, from the following list:
            'coreprof', 'coretransp','equilibrium', 'coresource', 'coreimpur',
            'toroidfield' and 'coreneutrals'.
        cpo_dir : str
            absolute path to directory containning cpo file.
        """

        # names used in ITM
        cpo_list = ['coreprof', 'coretransp','equilibrium',
              'coresource', 'coreimpur', 'toroidfield', 'coreneutrals']
        if cpo_name not in cpo_list:
            msg = ("CPOElement: wrong 'cpo_name'")
            logging.error(msg)
            raise RuntimeError(msg)

        # CPO file
        if cpo_dir is None:
            cpo_file = cpo_filename
        else:
            cpo_file = os.path.join(cpo_dir, cpo_filename)

        # cpo object and name
        self.core = read(cpo_file, cpo_name)
        self.cpo_name = cpo_name


    def get_value(self, param):
        """
        Parameters
        ----------
        param : str
            parameter name,  examples: "te.boundary.value", "te_transp.diff_eff"

        Returns
        -------
        float or vector
        """

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
        """
        Parameters
        ----------
        param : str
            parameter name,  examples: "te.boundary.value", "te_transp.diff_eff"
        value: real, int or list
            the value to set
        index: int
            the index position of the value in the list case, optional
        """

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
                field_value.T[0] = np.array(new_value)
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
    def save_file(self, filename):
        """
        Parameters
        ----------
        filename : str
        """

        write(self.core, filename)
