import os
#from re import M
import sys

import pickle
import csv
import json
import pprint # for debugging

import numpy as np
from math import ceil

import easyvvuq as uq

# from ual
from ascii_cpo import read

# from current package
from base.cpo_encoder import CPOEncoder
from base.cpo_decoder import CPODecoder
from base.xml_element import XMLElement
from base.utils import cpo_inputs, ftube_indices
from base.evvuq_partemplate_wenv import EasyVVUQParallelTemplateWithEnv

# from easyvvuq1.1
from easyvvuq.actions import Encode, Decode, Actions, CreateRunDirectory, ExecuteQCGPJ, ExecuteLocal, ExecuteSLURM, QCGPJPool, ActionPool
from easyvvuq.actions.execute_qcgpj import EasyVVUQParallelTemplate
from easyvvuq.constants import Status

# form qcg-pj
from qcg.pilotjob.executor_api.qcgpj_executor import QCGPJExecutor
from qcg.pilotjob.api.manager import LocalManager


'''
Perform UQ for the Turblence code GEM run for ~750 consecutive iterations.
Uncertainties are driven by:
The electron and ion temperature and their gradient localised on the Flux tube position.
'''


print('TEST GEM-NT-VARY: START')

print('Version of EasyVVUQ: '.format(uq.__version__))

# We test 1 flux tube
# Run gem_test in strandalone and use:
#base.utils.ftube_indices('gem_coreprof_in.cpo','gem_coretransp_out.cpo') to get the index - currently it is only a check

# Previus campaign ID
campaign_id = str(sys.argv[1])

# Machine name
SYS = os.environ['SYS']

# Home directory, for venv
HOME = os.environ['HOME']

# Working directory
tmp_dir = os.environ['SCRATCH']

# From Slurm script: srun
mpi_instance =  os.environ['MPICMD']

# From Slurm script: default
#mpi_model = 'default' #'srunmpi' #'intelmpi' #'openmpi'
mpi_model = os.environ['MPIMOD']

# CPO files location
cpo_dir = os.path.abspath("../workflows/AUG_28906_6") 
#cpo_dir = os.path.abspath("../standalone/bin")

# XML and XSD files location
#xml_dir = os.path.abspath("../workflows")
xml_dir = os.path.abspath("../standalone/bin")

# The executable code to run
obj_dir = os.path.abspath("../standalone/bin/"+SYS)
exec_code = "loop_gem_notransp"

gemxml = XMLElement(xml_dir + "/gem.xml")

# The restart version of the workflow
# 1. get exisiting profile shapes and their description as a varied parameter
# 2. for the *_coreprofile.cpo get the snapshot files for GEM (TFILE)
# 3. get the same xml and equilibium files that were used for previous batch of iterations
#      as well as info on flux tube coordinate, and parallel processing info 
# 5. run the campaign

# Initialize Campaign object
# Reusing last campaign's dir, DB, etc.
# options:
#   campaign.rerun() - currently used
#   campaigne.db.resume() - not necessary?
#   campaign(db_location=...) - currently used
#   campaign(state_file=...) - not necessary?

campaign_name = "VARY_1FT_GEM_" #ATTENTION: changed due to switch to arbitrary set of runs; TODO: has to bea read from DB or SLURM scrip
db_location = 'sqlite:///' + tmp_dir + '/' + campaign_name + campaign_id + '/campaign.db'
print("> Loading existing campaign from a database at: {}".format(db_location))
my_campaign = uq.Campaign(
                          name=campaign_name,
                          db_location=db_location,
                          work_dir=tmp_dir, #trying, on 17.11.22:01:17 resume campaign created a dir in /uq/
                          #state_file=,
                         )

# FOR THE RESTART CAMPAIGN, EVERYTHING (FILE, DIRS, etc.) SHOULD BE ALREADY THERE
# Create new directory for inputs
campaign_dir = my_campaign.campaign_dir
common_dir = campaign_dir +"/common/"

exec_path = os.path.join(common_dir, exec_code)

# Define the uncertain parameters

# Find such a nested set of quadrature abcissas so that coordinates used for U[-0.1,0.1]
# within a 1st-order Gauss-Legandre are used as smaller (by absolute value) cooridantes for 
# a 3rd-order G-S for some U[-a,+a]
# Here only a particular coefficient!

#alpha_q = 1. # TODO: The error should be read from the existing database, as well as input parameters
#alpha_q = a = 1./np.sqrt( (9./7.) - 6./7.*np.sqrt(6./5.) ) 
alpha_q = 2.5

# Electron and ion temperature and their gradients
input_params = {
    "te.value": {"dist": "Uniform", "err":  0.1*alpha_q, "min": 0.},
    "ti.value": {"dist": "Uniform", "err":  0.1*alpha_q, "min": 0.},
    "te.ddrho": {"dist": "Uniform", "err":  0.1*alpha_q, },
    "ti.ddrho": {"dist": "Uniform", "err":  0.1*alpha_q, },
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
# Workaround: read i-th iteration file
output_filename = "gem_coretransp_0450.cpo" # TODO either read from folder, or make the set-up more flexible
output_cponame = "coretransp"

# Define coordinate of flux tubes
#ftube_index = 66 # 67 would not consider python/fortran numeration difference and apparently would result in variation difference in an off place of a profile
#ftube_index = 94
ftube_index = 68 # TODO: Should be read from the exisiting XML in common folder
#ftube_index = gemxml.get_value('equilibrium_parameters.geometric.ra0')

# Parameter space for campaign and the distributions list for the sampler
"""
params, vary = cpo_inputs(cpo_filename=input_filename,
                          cpo_name=input_cponame,
                          input_dir=cpo_dir,
                          input_params=input_params,
                          ftube_index=ftube_index)
"""

# Check if this index is read correctly
ftube_index_test = ftube_indices(common_dir + '/gem_coreprof_in.cpo', 
        '../standalone/bin/gem_coretransp_out.cpo',
        False)
print('The flux tube location defined from the cpo files is: {}'.format(ftube_index_test))

if ftube_index != ftube_index_test[0]:
    print("Different flux tube coordinate is used") # could be assert, might rewrite or through away if script is run for more flux tubes

ftube_index = ftube_index_test[0] # ATTENTION: test value read from XML is actually used

# FOR THE RESTART CAMPAIGN, EVERYTHING (ENCODER, DECODER, ACTIONS, VARY, etc.) SHOULD BE ALREADY THERE
# Create the encoder and the decoder
input_filename = "gem_coreprof_in.cpo"

#TODO: decoder has to read the last spawned coretransp cpo file
decoder = CPODecoder(cpo_filename=output_filename,
                     cpo_name=output_cponame,
                     output_columns=output_columns)

#####################################################
### --- Post EasyVVUQ release modifications ---

# Get ncores
npesx = gemxml.get_value("cpu_parameters.domain_decomposition.npesx")
npess = gemxml.get_value("cpu_parameters.domain_decomposition.npess")
nftubes = gemxml.get_value("cpu_parameters.parallel_cases.nftubes")
ncores = npesx*npess*nftubes

pol_order = int(os.environ['POLORDER']) # TODO: should be read from the campaign database

#nruns = (pol_order + 1)**nparams # Nr=(Np+Nd, Nd)^T=(Np+Nd)!/(Np!*Nd!) |E.G.|=(3+4)!/3!4! = 5*6*7/6 = 35 => instead of 3^4=81?
nruns = int(os.environ['RUNRANGE'])    # ATTENTION: this might be less preferable for campaigns with parameter values chosen from a grid

ncores_tot = ncores * nruns
# current case: nruns=(5, 1)^T=5 not 16 ; achieved if using Point Collocation with regression via constructing PCESampler(regression=True)

n_cores_p_node = 48
if SYS == 'MARCONI':
    n_cores_p_node = 48
elif SYS == 'COBRA':
    n_cores_p_node = 40

nnodes = ceil(1.*ncores/n_cores_p_node)
nnodes_tot = ceil(1.*ncores_tot/n_cores_p_node) # not entirely correct due to an 'overkill' problem i.e. residual cores at one/more nodes may not be able to allocate any jobs, but here everything is devisible; also an import from 'math'

# Command line to be executed
exec_comm_flags = ' '
#exec_comm_flags += ' -vvvvv --profile=all --slurmd-debug=3 '
#exec_comm_flags = ' --oversubscribe --overcommit '

#exec_path_comm = mpi_instance + ' -n '+ str(ncores) + ' -N '+ str(nnodes) + ' ' + exec_path
#exec_path_comm = mpi_instance + ' -n '+ str(ncores) + ' ' + exec_path
exec_path_comm = mpi_instance + exec_comm_flags + ' ' + exec_path

print('Total number of nodes requires for all jobs: {0}'.format(nnodes_tot))
print('Number of cores required for single code instance computed: {0}'.format(ncores))
print('Executing turbulence code with the line: ' + exec_path_comm) 

# Creating an 'Execute' object
#print('Creating an ExecuteLocal, not ExecuteQCGPJ')

if mpi_model=='default':
    # When execution model is 'default': 'execute' should be set to exec_path_comm
    execute=ExecuteLocal(exec_path_comm)
elif mpi_model=='srunmpi':
    #execute=ExecuteLocal(exec_path)
    execute=ExecuteLocal(exec_path_comm) #TODO: trying out (srunmpi exec model) + (srun [com] [arg]) cli line
else:
    execute=ExecuteLocal(exec_path)

# Custom template for parallel job execution
template_par_simple = {
                       #'venv' : os.path.join(HOME, 'python394'), 
                       'numCores': ncores,
                       'numNodes': nnodes,
                       'model': mpi_model, # 'default' - QCG-PJ pool should work with 'default', as well as a commandline with 'mpiexec'
                      }

# Create list of actions in the campaign
# FOR THE RESTART NEED TO PERFORM NEW ACTIONS (w.o. Encode)

#actions = Actions(
#                  CreateRunDirectory('/runs'), 
#                  Encode(encoder), 
#                  execute,
#                  Decode(decoder)
#                 )

rerun_actions = Actions(execute)

resume_actions = Actions(
                         execute,
                         Decode(decoder),
                        )

"""
my_campaign.add_app(name=campaign_name,
                    params=params,
                    actions=actions)
"""

# Restore the sampler: check if needed to track number of runs
"""
my_sampler = uq.sampling.PCESampler(vary=vary, polynomial_order=pol_order)
my_campaign.set_sampler(my_sampler)
"""
my_sampler = my_campaign.get_active_sampler()
my_campaign.set_sampler(my_sampler, update=True)

# Need to change actions: only the 'execute' has to be performed
# Try:
#  a. replace_actions() for 'execute' only - should not work
#  a'. replace_actions() for 'execute' and 'Decode' - currently using
#  b. apply_to_each_sample() 'execute' only - doesn't work as ActionPool has to be difined beforehand
#  c. use recolate() - not necessary?
#  d. set_active_app() to the app with replaced actions - currently in use
#  f. use resurrect_app() - not necessary?
#  g. use .campaign_db.resume_campaign() - not necessary?

#my_campaign.campaign_db.resume_campaign()

#my_old_app = my_campaign.get_active_app()
#print(my_old_app)
#print(my_sampler)

my_campaign.replace_actions(app_name=campaign_name, 
                            actions=resume_actions)

# List of run existing run numbers to be executed again (continued)
run_ids = [str(x+1) for x in range(nruns)] # just numbers are correct id-s
print('Number of runs is {} and passed list of run id-s is: {}'.format(nruns, run_ids))

# Getting run list from the DB, better strip 'run_' from elements and use further instead of run_ids
run_names = [x for x in my_campaign.campaign_db.run_ids()]
print('Run names from the DB: {}'.format(run_names)) # run_{d} is a name but not id

# Checking the list of runs and their satus before the reassigning jobs
#pprint.pprint(my_campaign.list_runs()) ###DEBUG
#pprint.pprint(my_campaign.campaign_db.get_run_status(run_ids)) ###DEBUG

# Finding all runs and setting them as ENCODED i.e. before to-be-executed
#my_campaign.rerun(run_ids) # TODO fix error, all runs in '1wu9k2wa' database are marked as NEW 
my_campaign.campaign_db.set_run_statuses(run_ids, Status.ENCODED)

# Checking the content of the read campaign DB
#db_json = my_campaign.campaign_db.dump()
#pprint.pprint(db_json) ###DEBUG

# Checking the list of runs and their satus before the resume
#pprint.pprint(my_campaign.list_runs()) ###DEBUG
#pprint.pprint(my_campaign.campaign_db.get_run_status(run_ids)) ###DEBUG

my_campaign.set_app(campaign_name)

# ONLY AFTER HERE WE NEED AGAIN TO CHANGE SOMETHING w.r.t. PAST EXECUTION i.e. CREATE RESOURCE POOL; BY THIS TIME OTHER THINGS HAVE TO BE READY
print('Creating an Executor')

try:
    print('Creating resource pool')

    with QCGPJPool(
                  qcgpj_executor=QCGPJExecutor(log_level='debug'), 
                  #template=EasyVVUQParallelTemplate(),
                  template=EasyVVUQParallelTemplateWithEnv(),
                  template_params=template_par_simple,
                  ) as qcgpj:

        print('> Executing jobs and collating results')
        
        #exec_res = my_campaign.execute(pool=qcgpj)
        
        my_action_pool = my_campaign.apply_for_each_sample(
                                          actions=resume_actions,
                                          status=Status.ENCODED,
                                          sequential=False,
                                                          )
    
        print('> Actions defined, now executing')    
        exec_res = my_action_pool.start(pool=qcgpj)
        # TODO from this scripts codes still crash with SIGSEGV even if put on a separate node

        print('> Execution completed, now collating')
        exec_res.collate()
       
        #print(os.environ['QCG_PM_CPU_SET'])
        #print(qcgpj.template.template()[0]) ###DEBUG
        print('> Now leaving (and destructing?) the execution resource pool')
    
except Exception as e:

    print('!>> Exception during batch execution! :')
    print(e)

#################################
### --- Old part ---

# Post-processing analysis
print('Now finally analysing results')
#TODO: make sure decoder reads the file that exists, this may be a numbered file; current workaround: read a file from a fixed number of iteration
#TODO: make a decoder that check the results folder and using a regex finds the latest number of iteration or the oldest file
#TODO: make GEM iteration wrapper to continue numeration of GEM calls/macroiterations - currently every restart/slurm-batch will restart numbers from 0

#analysis = uq.analysis.PCEAnalysis(sampler=my_sampler, qoi_cols=output_columns)
analysis = uq.analysis.BasicStats(qoi_cols=output_columns) #ATTENTION: this is general but might break further code e.g. during Sobols' calculation
my_campaign.apply_analysis(analysis) 

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

pprint.pprint(results.raw_data) ###DEBUG

"""
json_filename = 'gem_notransp_results_' + os.environ['SLURM_JOBID']  + '.json'
with open(json_filename, "w") as json_file:
    json.dump(results.raw_data, json_file)  # TODO: save ndarrays
"""

csv_filename = 'gem_notransp_results_' + os.environ['SLURM_JOBID'] + '.csv'
with open(csv_filename, "w") as file_csv:
    w = csv.DictWriter(file_csv, results.raw_data.keys()) 
    w.writeheader()
    for r in results.raw_data:
        w.writerow(r)

#results.raw_data.to_scv('UQGEMCAMP_' + os.environ['SLURM_JOBID'] + '_results.csv', index=False)

print('>>> TEST GEM-NT-VARY: END')

