#! /usr/bin/env python
# System imports
from numpy.distutils.core import setup

all_py_modules = ['ual', 'ualdef', 'ual_low_level_wrapper', '__init__', 'topinfo','amns','antennas','bb_shield','compositionc','coredelta','corefast','coreneutrals','coreimpur','coreprof','coresource','coretransp','cxdiag','distribution','distsource','ecediag','edge','efcc','equilibrium','fusiondiag','halphadiag','heat_sources','interfdiag','ironmodel','langmuirdiag','launchs','lithiumdiag','mhd','magdiag','msediag','nbi','ntm','neoclassic','orbit','pellets','pfsystems','polardiag','power_conv','reflectomet','rfadiag','sawteeth','scenario','solcurdiag','temporary','toroidfield','tsdiag','turbulence','wall','waves',]

# ual low level setup
setup(name        = "ual",
      description = "High level interface for ual with dummy back-end",
      author      = "Matthieu Haefele, Olivier Hoenen",
      py_modules  = all_py_modules
      )
