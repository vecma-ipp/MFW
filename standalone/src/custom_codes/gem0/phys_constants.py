from scipy import constants as cnst
from scipy.constants import physical_constants as pc

pi = 3.141592653589793238462643383280

ee = 1.602176565E-019 #< elementary charge [C]
#ee = 4.8032e-10 # esu. CGS?

ev = 1.602176565E-19 #=ee

kb = 1.602176565E-019 #SI? ,=ee
#kb = 1.6022e-12 #CGS?

aion_d = 1./0.510998910 #2.0

me = 9.10938291E-31 #< electron mass [kg]
#me = 9.1095e-28 # electron mass [g], CGS?
md = 1875.612793 * aion_d *me
#md = 3.34358348e-27 #< deuteron mass [kg]

mu_0 = 4.0e-7 * pi #< vacuum permeability [H/m]

cc = 1.0 #< speed of light [m/s]

lcoul = 14.0 #< Coulomb logarithm

itm_amu = 1.660538921E-27  #< atomic mass unit [kg]
