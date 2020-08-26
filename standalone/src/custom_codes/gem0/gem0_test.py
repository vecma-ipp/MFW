
from gem0 import gem

#from euitm_schemas import  type_coreprof, type_equilibrium, type_coretransp
#from read_structures import open_read_file, close_read_file, read_cpo
#from write_structures import open_write_file, close_write_file, write_cpo
#from deallocate_structures import deallocate_cpo

from ascii_cpo import read, write

from ual.coreprof import coreprof
from ual.coretransp import coretransp
from ual.equilibrium import equilibrium

#import xml_file_reader
import xml.etree.ElementTree as ET

# init_step = 0 # inital step count

def gem0_cpo(equil, corep, coret) :

    equil = equilibrium()
    corep = coreprof()
    coret = coretransp()
    #code_parameters = {}

    # print ("python GEM0 wrapper in python")
    # print ("get code params")
    # fill_param(code_parameters, 'gem0.xml', '', 'gem0.xsd') #TODO check if fill_param() does exactly the same as parsing
    code_parameters = ET.parse('gem0.xml') # TODO check if schema is not needed

    # print ("run gem0 routine")
    gem(equil, corep, coret, code_parameters)

def gem0_test():
    """
    run a gem0 python replacement as a standalone program
    perfroms inputs, outputsa and code run
    """

    # CPO files
    equil_file_in = "gem0_equilibrium_in.cpo"
    corep_file_in = "gem0_coreprof_in.cpo"
    coret_file_out = "gem0_coretransp_out.cpo"

    coret = {}

    ios = 0

    # Read CPO file and write structures

    equil = read(equil_file_in, "equilibrium")
    corep = read(corep_file_in, "coreprof")
    #coret = read(coret_file_out , "coretransp")

    gem0_cpo(equil, corep, coret)

    # transfer CPO to buf
    write(coret_file_out, coret[0], 'coretransp')

if __name__ == '__main__':
    gem0_test()
