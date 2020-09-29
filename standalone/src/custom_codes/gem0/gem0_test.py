
import gem0
from gem0 import gem

#from euitm_schemas import  type_coreprof, type_equilibrium, type_coretransp
#from read_structures import open_read_file, close_read_file, read_cpo
#from write_structures import open_write_file, close_write_file, write_cpo
#from deallocate_structures import deallocate_cpo

from gem0_singleton import GEM0Singleton

# init_step = 0 # inital step count

if __name__ == '__main__':
    gem0_obeject = GEM0Singleton()
    gem0_obeject.gem0_test()
