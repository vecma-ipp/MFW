import os
import time
import easyvvuq as uq
from easymfw.templates.cpo_encoder import CPOEncoder
from easymfw.templates.cpo_decoder import CPODecoder
from easymfw.templates.xml_encoder import XMLEncoder
from easymfw.utils.io_tools import get_cpo_inputs
from easymfw.utils.io_tools import get_xml_inputs

'''
Perform UQ for the workflow ETS + CHEASE.
Uncertainties are driven by:
- External sources of Electrons heating.
- Boundary condition in Plasma Edge of electrons tempurature.
'''

print('UQ ETS-CHEASE: START')

t0 = time.time()

# Machine name
SYS = os.environ['SYS']

# Working directory
tmp_dir = os.environ['SCRATCH']

# CPO files
cpo_dir = os.path.abspath("../workflows/AUG_28906_6")
#cpo_dir = os.path.abspath("../workflows/JET_92436_23066")
#cpo_dir = os.path.abspath("../workflows/AUG_28906_6_BgB")

# XML and XSD files
xml_dir = os.path.abspath("../workflows")

# The executable code to run
obj_dir = os.path.abspath("../standalone/bin/"+SYS)
exec_code = "ets_chease"

# Define the uncertain parameters
# Electron boudary condition
input_params_bc = {
    "te.boundary.value": {
        "dist": "Uniform",
        "err": 0.2,
    }
}
# Electron heating Sources
input_params_src = {
    "electrons.heating_el.WTOT_el":{
        "dist": "Uniform",
        "err": 0.2,
    },
    "electrons.heating_el.RHEAT_el":{
        "dist": "Uniform",
        "err": 0.2,
    },
    "electrons.heating_el.FWHEAT_el":{
        "dist": "Uniform",
     "err": 0.2,
    }
}

input_params = {}
input_params.update(input_params_bc)
input_params.update(input_params_src)

# CPO and XML files containg initiail values of uncertain params
input_cpofilename = "ets_coreprof_in.cpo"
input_cponame = "coreprof"
input_xmlfilename = "source_dummy.xml"
input_xsdfilename = "source_dummy.xsd"

# The quantities of intersts and the cpo file to set them
output_columns = ["profiles_1d.pressure"]
output_filename = "chease_equilibrium_out.cpo"
output_cponame = "equilibrium"

# Get input parmeters
# params: the parameter space for campaign object
# vary: adistributions list for the sampler
input_cpofile = os.path.join(cpo_dir, input_cpofilename)
params_cpo, vary_cpo = get_cpo_inputs(cpo_file=input_cpofile,
                                      cpo_name=input_cponame,
                                      input_params=input_params_bc)

input_xmlfile = os.path.join(xml_dir, input_xmlfilename)
input_xsdfile = os.path.join(xml_dir, input_xsdfilename)
params_xml, vary_xml = get_xml_inputs(xml_file=input_xmlfile,
                                      xsd_file=input_xsdfile,
                                      input_params=input_params_src)

# Merge the params dict
params = {**params_cpo, **params_xml}
vary = {**vary_cpo, **vary_xml}

# Initialize Campaign object
campaign_name = "UQ_TrEq_"
my_campaign = uq.Campaign(name=campaign_name, work_dir=tmp_dir)

# Create new directory for inputs
campaign_dir = my_campaign.campaign_dir
common_dir = campaign_dir +"/common/"
os.mkdir(common_dir)

# Copy XML and XSD files
os.system("cp " + xml_dir + "/ets.xml "    + common_dir)
os.system("cp " + xml_dir + "/ets.xsd "    + common_dir)
os.system("cp " + xml_dir + "/chease.xml " + common_dir)
os.system("cp " + xml_dir + "/chease.xsd " + common_dir)
os.system("cp " + xml_dir + "/source_dummy.xml " + common_dir)
os.system("cp " + xml_dir + "/source_dummy.xsd " + common_dir)

# Copy input CPO files in common directory
os.system("cp " + cpo_dir + "/ets_coreprof_in.cpo "    + common_dir)
os.system("cp " + cpo_dir + "/ets_equilibrium_in.cpo " + common_dir)
os.system("cp " + cpo_dir + "/ets_coreimpur_in.cpo "   + common_dir)
os.system("cp " + cpo_dir + "/ets_coretransp_in.cpo "  + common_dir)
os.system("cp " + cpo_dir + "/ets_toroidfield_in.cpo " + common_dir)

# Copy  exec file
os.system("cp " + obj_dir +"/" + exec_code + " " + common_dir)

t1 = time.time()

# Create the encoders
encoder_cpo = CPOEncoder(template_filename=input_cpofilename,
                         target_filename=input_cpofilename,
                         input_cponame=input_cponame,
                         common_dir=common_dir,
                         input_params=input_params_bc)

encoder_xml = XMLEncoder(template_filename = input_xmlfilename,
                         target_filename = input_xmlfilename,
                         input_params=input_params_src,
                         common_dir=common_dir)

# Combine both encoders into a single encoder
encoder = uq.encoders.MultiEncoder(encoder_cpo, encoder_xml)

# Create the decoder
decoder = CPODecoder(target_filename=output_filename,
                     output_columns=output_columns,
                     output_cponame=output_cponame)

# Create a collation element for this campaign
collater = uq.collate.AggregateSamples(average=False)

# Add the ETS app (automatically set as current app)
my_campaign.add_app(name=campaign_name,
                    params=params,
                    encoder=encoder,
                    decoder=decoder,
                    collater=collater)
t2 = time.time()

# Create the sampler
my_sampler = uq.sampling.PCESampler(vary=vary, polynomial_order=3)
my_campaign.set_sampler(my_sampler)

# Will draw all (of the finite set of samples)
my_campaign.draw_samples()

t3 = time.time()

# Encoding
my_campaign.populate_runs_dir()

t4 = time.time()

# Running
exec_path = os.path.join(common_dir, exec_code)
my_campaign.apply_for_each_run_dir(uq.actions.ExecuteLocal(exec_path))

t5 = time.time()

# Collate
my_campaign.collate()

t6 = time.time()

# Post-processing analysis
analysis = uq.analysis.PCEAnalysis(sampler=my_sampler, qoi_cols=output_columns)
my_campaign.apply_analysis(analysis)

# Get results
results = my_campaign.get_last_analysis()

t7 = time.time()

# Get Descriptive Statistics
stat = {}
sob1 = {}
perc = {}
dist = {}
for qoi in output_columns:
    stat[qoi] = results['statistical_moments'][qoi]
    sob1[qoi] = results['sobols_first'][qoi]
    perc[qoi] = results['percentiles'][qoi]
    dist[qoi] = results['output_distributions'][qoi]

# Save graphics
from easymfw.utils import plots
from ascii_cpo import read

equil_file = os.path.join(cpo_dir, "ets_equilibrium_in.cpo")
equil = read(equil_file, "equilibrium")
rho = equil.profiles_1d.rho_tor

uparams_names = list(params.keys())

#for qoi in output_columns:
    #q = qoi.split('.')[-1]
qoi = "profiles_1d.pressure"
plots.plot_stats_all(rho, stat[qoi], perc[qoi], dist[qoi],
                 xlabel=r'$\rho_{tor} [m]$', ylabel="P [Pa]",
                 ftitle='Pressure profile',
                 fname='data/outputs/STAT_p')

plots.plot_sobols_all(rho, sob1[qoi], uparams_names,
                  ftitle='1st Sobol indices: '+q,
                  fname='data/outputs/SA_'+q+"_"+campaign_name)

t8 = time.time()

print("vary = ", vary)
print('Ns = ', my_sampler.n_samples)
print('Time for initializing = %.3f' %(t1-t0))
print('Time for initializing Campaign = %.3f' %(t2-t1))
print('Time for sampling = %.3f' %(t3-t2))
print('Time for populating  = %.3f' %(t4-t3))
print('Time for running  = %.3f' %(t5-t4))
print('Time for collating = %.3f' %(t6-t5))
print('Time for analysis = %.3f' %(t7-t6))
print('Time for plotting  = %.3f' % (t8-t7))

print('UQ ETS-CHEASE: END')
