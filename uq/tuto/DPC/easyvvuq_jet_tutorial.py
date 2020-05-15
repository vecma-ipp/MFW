import os
import easyvvuq as uq
import chaospy as cp
import pickle
import time
import numpy as np 
import matplotlib.pylab as plt

time_start = time.time()
# Set up a fresh campaign called "jet_pce."
my_campaign = uq.Campaign(name='jet_pce.')

# Define parameter space
params = {
    "Qe_tot":   {"type": "float",   "min": 1.0e6, "max": 50.0e6, "default": 2e6}, 
    "H0":       {"type": "float",   "min": 0.00,  "max": 1.0,    "default": 0}, 
    "Hw":       {"type": "float",   "min": 0.01,  "max": 100.0,  "default": 0.1}, 
    "Te_bc":    {"type": "float",   "min": 10.0,  "max": 1000.0, "default": 100}, 
    "chi":      {"type": "float",   "min": 0.01,  "max": 100.0,  "default": 1}, 
    "a0":       {"type": "float",   "min": 0.2,   "max": 10.0,   "default": 1}, 
    "R0":       {"type": "float",   "min": 0.5,   "max": 20.0,   "default": 3}, 
    "E0":       {"type": "float",   "min": 1.0,   "max": 10.0,   "default": 1.5}, 
    "b_pos":    {"type": "float",   "min": 0.95,  "max": 0.99,   "default": 0.98}, 
    "b_height": {"type": "float",   "min": 3e19,  "max": 10e19,  "default": 6e19}, 
    "b_sol":    {"type": "float",   "min": 2e18,  "max": 3e19,   "default": 2e19}, 
    "b_width":  {"type": "float",   "min": 0.005, "max": 0.02,   "default": 0.01}, 
    "b_slope":  {"type": "float",   "min": 0.0,   "max": 0.05,   "default": 0.01}, 
    "nr":       {"type": "integer", "min": 10,    "max": 1000,   "default": 10}, 
    "dt":       {"type": "float",   "min": 1e-3,  "max": 1e3,    "default": 100},
    "out_file": {"type": "string",  "default": "output.csv"}
}
"""
str = ""
first = True
for k in params.keys():
    if first:
        str += '{"%s": "$%s"' % (k,k) ; first = False
    else:
        str += ', "%s": "$%s"' % (k,k)
str += '}'
print(str, file=open('jet.template','w'))
"""

# Create an encoder, decoder and collater for PCE test app
encoder = uq.encoders.GenericEncoder(template_fname='jet.template',
                                     delimiter='$',
                                     target_filename='jet_in.json')


decoder = uq.decoders.SimpleCSV(target_filename="output.csv",
                                output_columns=["te", "ne", "rho", "rho_norm"],
                                header=0)

collater = uq.collate.AggregateSamples(average=False)

# Add the app (automatically set as current app)
my_campaign.add_app(name="jet",
                    params=params,
                    encoder=encoder,
                    decoder=decoder,
                    collater=collater)

time_end = time.time()
print('Time for phase 1', time_end-time_start)
time_start = time.time()

# Create the sampler
vary = {
    "Qe_tot":   cp.Uniform(1.8e6, 2.2e6),
    "H0":       cp.Uniform(0.0,   0.2),
    "Hw":       cp.Uniform(0.1,   0.5),
    "chi":      cp.Uniform(0.8,   1.2), 
    "Te_bc":    cp.Uniform(80.0,  120.0)
}
"""
    "a0":       cp.Uniform(0.9,   1.1), 
    "R0":       cp.Uniform(2.7,   3.3), 
    "E0":       cp.Uniform(1.4,   1.5), 
    "b_pos":    cp.Uniform(0.95,  0.99), 
    "b_height": cp.Uniform(5e19,  7e19), 
    "b_sol":    cp.Uniform(1e19,  3e19), 
    "b_width":  cp.Uniform(0.015, 0.025), 
    "b_slope":  cp.Uniform(0.005, 0.020)
"""

# Associate a sampler with the campaign
my_campaign.set_sampler(uq.sampling.PCESampler(vary=vary, polynomial_order=3))

# Will draw all (of the finite set of samples)
my_campaign.draw_samples()

time_end = time.time()
print('Time for phase 2', time_end-time_start)
time_start = time.time()

my_campaign.populate_runs_dir()

time_end = time.time()
print('Time for phase 3', time_end-time_start)
time_start = time.time()

cwd = os.getcwd()
cmd = f"{cwd}/jet_model.py jet_in.json"
my_campaign.apply_for_each_run_dir(uq.actions.ExecuteLocal(cmd, interpret='python3'))

time_end = time.time()
print('Time for phase 4', time_end-time_start)
time_start = time.time()

my_campaign.collate()

time_end = time.time()
print('Time for phase 5', time_end-time_start)
time_start = time.time()

# Post-processing analysis
my_campaign.apply_analysis(uq.analysis.PCEAnalysis(sampler=my_campaign.get_active_sampler(), qoi_cols=["te", "ne", "rho", "rho_norm"]))

time_end = time.time()
print('Time for phase 6', time_end-time_start)
time_start = time.time()

# Get Descriptive Statistics
results = my_campaign.get_last_analysis()
stats = results['statistical_moments']['te']
per = results['percentiles']['te']
sobols = results['sobols_first']['te']
rho = results['statistical_moments']['rho']['mean']
rho_norm = results['statistical_moments']['rho_norm']['mean']

time_end = time.time()
print('Time for phase 7', time_end-time_start)
time_start = time.time()

my_campaign.save_state("campaign_state.json")

###old_campaign = uq.Campaign(state_file="campaign_state.json", work_dir=".")

pickle.dump(results, open('jet_results.pickle','bw'))
###saved_results = pickle.load(open('jet_results.pickle','br'))

time_end = time.time()
print('Time for phase 8', time_end-time_start)

plt.ion()

plt.figure() 
plt.plot(rho, stats['mean'], 'b-', label='Mean')
plt.plot(rho, stats['mean']-stats['std'], 'b--', label='+1 std deviation')
plt.plot(rho, stats['mean']+stats['std'], 'b--')
plt.fill_between(rho, stats['mean']-stats['std'], stats['mean']+stats['std'], color='b', alpha=0.2)
plt.plot(rho, per['p10'], 'b:', label='10 and 90 percentiles')
plt.plot(rho, per['p90'], 'b:')
plt.fill_between(rho, per['p10'], per['p90'], color='b', alpha=0.1)
plt.fill_between(rho, [r.lower[0] for r in results['output_distributions']['te']], [r.upper[0] for r in results['output_distributions']['te']], color='b', alpha=0.05)
plt.legend(loc=0)
plt.xlabel('rho [m]')
plt.ylabel('Te [eV]')
plt.title(my_campaign.campaign_dir)


plt.figure() 
for k in sobols.keys(): plt.plot(rho, sobols[k][0], label=k)
plt.legend(loc=0)
plt.xlabel('rho [m]')
plt.ylabel('sobols_first')
plt.title(my_campaign.campaign_dir)


plt.figure() 
for k in results['sobols_total']['te'].keys(): plt.plot(rho, results['sobols_total']['te'][k][0], label=k)
plt.legend(loc=0)    
plt.xlabel('rho [m]')
plt.ylabel('sobols_total')
plt.title(my_campaign.campaign_dir)

plt.figure()
for i, D in enumerate(results['output_distributions']['te']):
    _Te = np.linspace(D.lower[0], D.upper[0], 101)
    _DF = D.pdf(_Te)
    plt.loglog(_Te, _DF, 'b-')
    plt.loglog(stats['mean'][i], np.interp(stats['mean'][i], _Te, _DF), 'bo')
    plt.loglog(stats['mean'][i]-stats['std'][i], np.interp(stats['mean'][i]-stats['std'][i], _Te, _DF), 'b*')
    plt.loglog(stats['mean'][i]+stats['std'][i], np.interp(stats['mean'][i]+stats['std'][i], _Te, _DF), 'b*')
    plt.loglog(per['p10'][i],  np.interp(per['p10'][i], _Te, _DF), 'b+')
    plt.loglog(per['p90'][i],  np.interp(per['p90'][i], _Te, _DF), 'b+')
plt.xlabel('Te')
plt.ylabel('distribution function')


"""
Time for phase 1 0.3424241542816162
Time for phase 2 78.29818987846375
Time for phase 3 12.24399185180664
Time for phase 4 5167.55646109581
Time for phase 5 65.48898196220398
Time for phase 6 191.89830493927002
Time for phase 7 1.9788742065429688e-05



Time for phase 1 0.4670450687408447
Time for phase 2 39.2676100730896
Time for phase 3 2.6985158920288086
Time for phase 4 5216.318249940872
Time for phase 5 74.85999393463135
Time for phase 6 537.1869812011719
Time for phase 7 0.002438068389892578
Time for phase 8 0.10825705528259277

"""

"""
---------------------------------------------------------------------------
TypeError                                 Traceback (most recent call last)
~/src/EasyVVUQ/jet/easyvvuq_jet_tutorial.py in <module>
    131 my_campaign.save_state("campaign_state.json")
    132 
--> 133 old_campaign = uq.Campaign(state_file="campaign_state.json", work_dir=".")
    134 
    135 pickle.dump(results, open('jet_results.pickle','bw'))

/Volumes/G-RAID with Thunderbolt/dpc/GIT/EasyVVUQ/env/lib/python3.7/site-packages/easyvvuq-0.5.1+223.gd452619-py3.7.egg/easyvvuq/campaign.py in __init__(self, name, db_type, db_location, work_dir, state_file, change_to_state, verify_all_runs)
    149         # campaign with a new campaign database
    150         if state_file is not None:
--> 151             self._state_dir = self.init_from_state_file(state_file)
    152             if change_to_state:
    153                 os.chdir(self._state_dir)

/Volumes/G-RAID with Thunderbolt/dpc/GIT/EasyVVUQ/env/lib/python3.7/site-packages/easyvvuq-0.5.1+223.gd452619-py3.7.egg/easyvvuq/campaign.py in init_from_state_file(self, state_file)
    276         # Resurrect the sampler
    277         self._active_sampler_id = campaign_db.get_sampler_id(self.campaign_id)
--> 278         self._active_sampler = campaign_db.resurrect_sampler(self._active_sampler_id)
    279 
    280         self.set_app(self._active_app_name)

/Volumes/G-RAID with Thunderbolt/dpc/GIT/EasyVVUQ/env/lib/python3.7/site-packages/easyvvuq-0.5.1+223.gd452619-py3.7.egg/easyvvuq/db/sql.py in resurrect_sampler(self, sampler_id)
    292 
    293         serialized_sampler = self.session.query(SamplerTable).get(sampler_id).sampler
--> 294         sampler = BaseSamplingElement.deserialize(serialized_sampler)
    295         return sampler
    296 

/Volumes/G-RAID with Thunderbolt/dpc/GIT/EasyVVUQ/env/lib/python3.7/site-packages/easyvvuq-0.5.1+223.gd452619-py3.7.egg/easyvvuq/sampling/base.py in deserialize(serialized_sampler)
    114             inputs["state"]["vary"] = Vary.deserialize(inputs["state"]["vary"]).vary_dict
    115 
--> 116         sampler = AVAILABLE_SAMPLERS[inputs["element_name"]](**inputs["state"])
    117         return sampler
    118 

TypeError: __init__() got an unexpected keyword argument 'params_size'

"""
