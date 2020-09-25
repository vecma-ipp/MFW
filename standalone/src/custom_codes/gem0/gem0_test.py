
import gem0
from gem0 import gem

#from euitm_schemas import  type_coreprof, type_equilibrium, type_coretransp
#from read_structures import open_read_file, close_read_file, read_cpo
#from write_structures import open_write_file, close_write_file, write_cpo
#from deallocate_structures import deallocate_cpo

from ascii_cpo import read, write

from ual.coreprof import coreprof
from ual.coretransp import coretransp
from ual.equilibrium import equilibrium

from assign_turb_parameters import assign_turb_parameters

# Import xml_file_reader
import xml.etree.ElementTree as ET
#from easymfw.templates.xml_element import XMLElement

# init_step = 0 # inital step count

# CPO files
equil_file_in = "gem0_equilibrium_in.cpo"
corep_file_in = "gem0_coreprof_in.cpo"
coret_file_out = "gem0_coretransp_out.cpo"

def get_code_params():

    print ("> Get code params")
    # fill_param(code_parameters, 'gem0.xml', '', 'gem0.xsd') #TODO check if fill_param() does exactly the same as parsing
    #code_parameters = XMLElement('gem0.xml')
    code_parameters, _ = assign_turb_parameters('gem0.xml')

    return code_parameters

def get_code_ios():
    #coret = {}
    ios = 0

    # Read CPO file and write structures
    equil = read(equil_file_in, "equilibrium")
    corep = read(corep_file_in, "coreprof")
    #coret = read(coret_file_out, "coretransp")
    coret = coretransp()

    return equil, corep, coret

def gem0_test():
    """
    runs a gem0 python replacement as a standalone program
    perfroms inputs, outputs and code run
    """
    
    code_parameters = get_code_params()
    equil, corep, coret = get_code_ios()

    print ("> Run gem0 routine")
    coret = gem(equil, corep, coret, code_parameters)

    # Transfer CPO to buffer / write file
    print("> Writing transport cpo")
    write(coret, coret_file_out, 'coretransp')

if __name__ == '__main__':
    gem0_test()
