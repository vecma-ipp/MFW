import _MODULE
import f90wrap.runtime
import logging

def update_file(filename, n, values):
    """
    update_file(filename, n, values)
    
    
    Defined at wrappers.f90 lines 3-48
    
    Parameters
    ----------
    filename : str
    n : int
    values : float array
    
    """
    _MODULE.f90wrap_update_file(filename=filename, n=n, values=values)

