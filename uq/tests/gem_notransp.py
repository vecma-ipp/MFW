import os
import sys

import pickle
import csv
import json

from math import ceil

import easyvvuq as uq

# EasyVVUQ/QCG-PJ
#import eqi

# from ual
from ascii_cpo import read

# from current package
from base.cpo_encoder import CPOEncoder
from base.cpo_decoder import CPODecoder
from base.xml_element import XMLElement
from base.utils import cpo_inputs, ftube_indices

# from easyvvuq1.1
from easyvvuq.actions import Encode, Decode, Actions, CreateRunDirectory, ExecuteQCGPJ, ExecuteLocal, ExecuteSLURM, QCGPJPool
from easyvvuq.actions.execute_qcgpj import EasyVVUQParallelTemplate

# form qcg-pj
from qcg.pilotjob.executor_api.qcgpj_executor import QCGPJExecutor
from qcg.pilotjob.api.manager import LocalManager


'''
Perform UQ for the Turblence code GEM run for ~750 consecutive iterations.
Uncertainties are driven by:
The electon and ion temperature and their gradient localised on the Flux tube position.
'''


print('TEST GEM-NT-VARY: START')

print('Version of EasyVVUQ: '.format(uq.__version__))

# We test 1 flux tube
# run gem_test in strandalone and use:
#base.utils.ftube_indices('gem_coreprof_in.cpo','gem_coretransp_out.cpo') to get the index
#TODO double check from XML, alternatively simply read from xml

#ftube_index = 66 # 67 would not consider python/fortran numeration difference and apparently would result in variation differenct in an off place of a profile
#ftube_index = 94
ftube_index = 68

# Machine name
SYS = os.environ['SYS']

# home directory, for venv
HOME = os.environ['HOME']

# Working directory
tmp_dir = os.environ['SCRATCH']

# From Slurm script (intelmpi)
mpi_instance =  os.environ['MPICMD']
#mpi_instance = 'mpirun'

# do not use intelmpi+MARCONI+QCG !
#mpi_model = 'default' #'srunmpi' #'intelmpi' #'openmpi'
# works with 'default' on MARCONI, currently not on COBRA
mpi_model = os.environ['MPIMOD']

# CPO files location
cpo_dir = os.path.abspath("../workflows/AUG_28906_6") 
#cpo_dir = os.path.abspath("../standalone/bin")

# XML and XSD files location
#xml_dir = os.path.abspath("../workflows")
xml_dir = os.path.abspath("../standalone/bin")

# The executable code to run
obj_dir = os.path.abspath("../standalone/bin/"+SYS)
exec_code = "loop_gem_notransp"  #"loop_gem_notransp" 

# Define the uncertain parameters
# Electron temperature and its gradient

# Find such a nested set of quadrature abcissas so that coordinates used for U[-0.1,0.1]
# within a 1st-order Gauss-Legandre are used as smaller (by absolute value) cooridantes for 
# a 3rd-order G-S for some U[-a,+a]
# Here only a particular coefficient!
alpha_q = 1.
#alpha_q = a = 1./np.sqrt( (9./7.) - 6./7.*np.sqrt(6./5.) ) 

input_params = {
    "te.value": {"dist": "Uniform", "err":  0.1*alpha_q, "min": 0.},
    "ti.value": {"dist": "Uniform", "err":  0.1*alpha_q, "min": 0.},
    "te.ddrho": {"dist": "Uniform", "err":  0.1*alpha_q, "max": 0.},
    "ti.ddrho": {"dist": "Uniform", "err":  0.1*alpha_q, "max": 0.}
}

nparams = len(input_params)

# CPO file containg initial values of uncertain params
input_filename = "ets_coreprof_in.cpo"
input_cponame = "coreprof"

# The quantities of intersts and the cpo file to set them
output_columns = [
                 "te_transp.flux",
                 "ti_transp.flux"
                 ]
#output_filename = "gem_coretransp_out.cpo"
#workaround: read 450th iteration file
#output_filename = "gem_coretransp_0100.cpo" # TODO either read from folder, or make the set-up more flexible
output_filename = "gem_coretransp_0450.cpo"
output_cponame = "coretransp"

# parameter space for campaign and the distributions list for the sampler
params, vary = cpo_inputs(cpo_filename=input_filename,
                          cpo_name=input_cponame,
                          input_dir=cpo_dir,
                          input_params=input_params,
                          ftube_index=ftube_index)

# Initialize Campaign object
campaign_name = "VARY_1FT_GEM_NT_"
my_campaign = uq.Campaign(name=campaign_name, work_dir=tmp_dir)

# Create new directory for inputs
campaign_dir = my_campaign.campaign_dir
common_dir = campaign_dir +"/common/"
os.mkdir(common_dir)

# Copy input CPO files (cf. test_gem0.f90)
os.system("cp " + cpo_dir + "/ets_equilibrium_in.cpo "
                + common_dir + "gem_equilibrium_in.cpo")
os.system("cp " + cpo_dir + "/ets_coreprof_in.cpo "
                + common_dir + "/gem_coreprof_in.cpo")

# Copy restart file
os.system("cp " + cpo_dir + "/t0?.dat " + common_dir)

# Copy XML and XSD files
os.system("cp " + xml_dir + "/gem.xml " + common_dir)
os.system("cp " + xml_dir + "/gem.xsd " + common_dir)

# Copy  exec file
os.system("cp " + obj_dir +"/"+ exec_code + " " + common_dir)
exec_path = os.path.join(common_dir, exec_code)

# TODO check if this index is read correctly
ftube_index_test = ftube_indices(common_dir + '/gem_coreprof_in.cpo', 
        #'/marconi/home/userexternal/yyudin00/code/MFW/standalone/bin/gem_coretransp_out.cpo',
          xml_dir + '/gem_coretransp_out.cpo',
          False) # TODO : where to get the output file and should it be in common folder?

print('The flux tube location defined from the cpo files is: {}'.format(ftube_index_test))

# Create the encoder and the decoder
input_filename = "gem_coreprof_in.cpo"
encoder = CPOEncoder(cpo_filename=input_filename,
                     cpo_name=input_cponame,
                     input_dir=common_dir,
                     ftube_index=ftube_index)

#TODO: decoder has to read the last spawned coretransp cpo file
decoder = CPODecoder(cpo_filename=output_filename,
                     cpo_name=output_cponame,
                     output_columns=output_columns)

#####################################################
### --- Post EasyVVUQ release modifications ---

# get ncores
gemxml = XMLElement(xml_dir + "/gem.xml")
npesx = gemxml.get_value("cpu_parameters.domain_decomposition.npesx")
npess = gemxml.get_value("cpu_parameters.domain_decomposition.npess")
nftubes = gemxml.get_value("cpu_parameters.parallel_cases.nftubes")
ncores = npesx*npess*nftubes

pol_order = 1
#pol_order = 3
nruns = (pol_order + 1)**nparams # Nr=(Np+Nd, Nd)^T=(Np+Nd)!/(Np!*Nd!)  |=(3+4)!/3!4! = 5*6*7/6 = 35 => instead 3^4=81 ?
ncores_tot = ncores * nruns

n_cores_p_node = 48
if SYS == 'MARCONI':
    n_cores_p_node = 48
elif SYS == 'COBRA':
    n_cores_p_node = 40
    #n_cores_p_node = 80

nnodes = ceil(1.*ncores/n_cores_p_node)
nnodes_tot = ceil(1.*ncores_tot/n_cores_p_node) # not entirely correct due to an 'overkill' problem i.e. residual cores at one/more nodes may not be able to allocate any jobs, but here everything is devisible; also an import from 'math'

#exec_path_comm = mpi_instance + ' -n '+ str(ncores) + ' -N '+ str(nnodes) + ' ' + exec_path
exec_path_comm = mpi_instance + ' -n '+ str(ncores) + ' ' + exec_path

print('Total number of nodes required for all jobs: {0}'.format(nnodes_tot))
print('Number of cores required for single code instance computed: {0}'.format(ncores))
print('Executing turbulence code with the line: ' + exec_path_comm) 

print('Creating an ExecuteQCGPJ')
#execute = ExecuteQCGPJ(
#                      ExecuteLocal(exec_path_comm)
                      #ExecuteSLURM(
                      #             template_script=exec_path,
                      #             variable='runs/'
                      #            )
#                      )

if mpi_model=='default':
    # when execution model is 'default': 'execute' should be set to exec_path_comm
    execute=ExecuteLocal(exec_path_comm)
else:
    execute=ExecuteLocal(exec_path)

# Execution
#qcgpjexec = eqi.Executor(my_campaign)
#qcgpjexec.create_manager(log_level='info')

#qcgpjexec.add_task(eqi.Task(
#    eqi.TaskType.EXECUTION,
#    eqi.TaskRequirements(cores=ncores),
#    model=mpi_instance,
#    application=exec_path
#))/

# Will draw all (of the finite set of samples)
#my_campaign.draw_samples()
#my_campaign.populate_runs_dir()

#qcgpjexec.run(processing_scheme=eqi.ProcessingScheme.EXEC_ONLY)

# custom template for parallel job exectution

template_par_simple = {
                       ###'name': 'gem_long_var_simp',
                       #'exec': exec_path,
                       
                       #'venv' : os.path.join(HOME, 'python394'), 
                       #'venv' : os.path.join(HOME, 'conda-envs/python394'),

                       'numCores': ncores,
                       'numNodes': nnodes,
                       'model': mpi_model, # 'default' -- should work with 'default' at MARCONI
                      }

template_par_cust = {
                      'name': 'gem_long_varied',
                      'execution': {
                                     #'exec': exec_path,
                                     'model': mpi_model,
                                     'model_options': {
                                                        #'mpirun': '',
                                                        #'mpirun_args' : ''
                                                      },
                                      #'modules': ['impi']
                                      #'venv' : os.path.join(HOME, 'python394')             
                                   },
                      'resources': {
                                     'numCores': { 
                                                   'exact': ncores
                                                 },
                                     'numNodes': {
                                                   'exact': nnodes # Mind if it occupies a single node entirely
                                                 },
                                   },  
                    }

# create list of actions in the campaign
actions = Actions(
                  CreateRunDirectory('/runs'), 
                  Encode(encoder), 
                  execute,
                  Decode(decoder)
                 ) # does CreateRunDirectory also set ups the dir? (for us the dir's themselves might be already created)

# Add the app (automatically set as current app)
my_campaign.add_app(name=campaign_name,
                    params=params,
                    actions=actions)

# Create the samples
my_sampler = uq.sampling.PCESampler(vary=vary, polynomial_order=pol_order)
my_campaign.set_sampler(my_sampler)
print('Creating an Executor')
#executor=QCGPJExecutor(log_level='debug')

try:
    print('Creating resource pool')

    with QCGPJPool(
                  qcgpj_executor=QCGPJExecutor(log_level='debug'), # =executor,
                  template=EasyVVUQParallelTemplate(),
                  template_params=template_par_simple,  #_cust
                  ) as qcgpj:

        print('> Executing jobs and collating results')
        exec_res = my_campaign.execute(pool=qcgpj)
        exec_res.collate()
       
        #print(os.environ['QCG_PM_CPU_SET'])
        #print(qcgpj.template.template()[0])
    
except Exception as e:

    print('!>> Exception during batch execution! :')
    print(e)

#################################
### --- Old part ---

# Post-processing analysis
print('Now finally analysing results')
#TODO: make sure decoder reads the file that exists, this may be a numbered file; current workaround: read a file from a fixed number of iteration
#TODO: make a decoder that check the results folder and using a regex finds the latest number of iteration or the oldest file
analysis = uq.analysis.PCEAnalysis(sampler=my_sampler, qoi_cols=output_columns)
my_campaign.apply_analysis(analysis)

# TODO: make a restart version of the workflow
# 1. get exisiting profile shapes and their description as a varied parameter
# 2. for the _coreprofile.cpo get the snapshot files for GEM (TFILE)
# 3. get the same xml and equilibium files that were used for previus batch of iterations
#      as well as info on flux tube coodinate, and parallel processing info 
# 5. run the campaign
# TODO: reuse functionality from the campaign restart (resume?) functionality

# Get results
results = my_campaign.get_last_analysis()

# Get Descriptive Statistics
mean_el = results.describe('te_transp.flux', 'mean')
std_el = results.describe('te_transp.flux', 'std')
mean_io = results.describe('ti_transp.flux', 'mean')
std_io = results.describe('ti_transp.flux', 'std')

s1_el = results.sobols_first('te_transp.flux')
s1_io = results.sobols_first('ti_transp.flux')

print("TE TRANSP FLUX")
print("Mean: ", mean_el)
print("Std: ", std_el)
print("Sob1: ", s1_el)

print("TI TRANSP FLUX")
print("Mean: ", mean_io)
print("Std: ", std_io)
print("Sob1: ", s1_io)

### NEW PART: saving results and serialising DB

pickle_filename = 'gem_notransp_results_' + os.environ['SLURM_JOBID']  + '.pickle'
with open(pickle_filename, "bw") as file_pickle:
    pickle.dump(results, file_pickle)

db_json_filename = 'gem_notransp_db_' + os.environ['SLURM_JOBID'] + '.json'
db_json = my_campaign.campaign_db.dump()
with open(db_json_filename, "w") as db_file_json:
    json.dump(db_json, db_file_json)

pprint.pprint(results.raw_data)

"""
csv_filename = 'gem_notransp_results_' + os.environ['SLURM_JOBID'] + '.csv'
with open(csv_filename, "w") as file_csv:
    w = csv.DictWriter(file_csv, results.raw_data.keys())
    w.writeheader()
    for r in results.raw_data:
        w.writerow(r)
"""

#results.raw_data.to_scv('UQGEMCAMP_' + os.environ['SLURM_JOBID'] + '_results.csv', index=False)

print('>>> TEST GEM-NT-VARY: END')

