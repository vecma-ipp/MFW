import os
import easyvvuq as uq
import chaospy as cp
import pickle
import time
import numpy as np
import matplotlib.pylab as plt


time_start = time.time()

# Set to True if execution with QCJ-PilotJob
EXEC_PJ = True

# Model infos
TEMPLATE = "DPC/jet.template"
APPLICATION = "DPC/jet_model.py"
ENCODED_FILENAME = "jet_in.json"

# tmpdir can be defined in sbach script or bash profile, otherwise use "/tmp/"
tmpdir = os.environ["SCRATCH"]
jobdir = os.getcwd()

# Set up a fresh campaign
campaign_name = "jet_uqpj"
my_campaign = uq.Campaign(name=campaign_name,  work_dir=tmpdir)

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
    "b_width":  {"type": "float",   "min": 0.005, "max": 0.025,   "default": 0.01},
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

# QoI output and cols
output_filename = params["out_file"]["default"]
qoi_cols = ["te"]

# Create an encoder, decoder and collater for PCE test app
encoder = uq.encoders.GenericEncoder(template_fname=jobdir + '/' +TEMPLATE,
                                     delimiter='$',
                                     target_filename=ENCODED_FILENAME)


decoder = uq.decoders.SimpleCSV(target_filename=output_filename,
                                output_columns=qoi_cols,
                                header=0)

collater = uq.collate.AggregateSamples(average=False)

# Add the app (automatically set as current app)
my_campaign.add_app(name=campaign_name,
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
    "Te_bc":    cp.Uniform(80.0,  120.0),
    "a0":       cp.Uniform(0.9,   1.1),
    "R0":       cp.Uniform(2.7,   3.3),
    "E0":       cp.Uniform(1.4,   1.5),
    "b_pos":    cp.Uniform(0.95,  0.99),
    "b_height": cp.Uniform(5e19,  7e19),
    "b_sol":    cp.Uniform(1e19,  3e19),
    "b_width":  cp.Uniform(0.015, 0.025),
    "b_slope":  cp.Uniform(0.005, 0.020)
}

# Associate a sampler with the campaign
my_sampler = uq.sampling.PCESampler(vary=vary, polynomial_order=4,
                                    regression=True)
print('Number of samples: ', my_sampler.n_samples)
my_campaign.set_sampler(my_sampler)

# Will draw all (of the finite set of samples)
my_campaign.draw_samples()

time_end = time.time()
print('Time for phase 2', time_end-time_start)
time_start = time.time()

# Will perform encoder part (it can be done by PJ)
my_campaign.populate_runs_dir()

time_end = time.time()
print('Time for phase 3', time_end-time_start)
time_start = time.time()

# Commande code to execute
cmd = jobdir + "/" + APPLICATION + " " + ENCODED_FILENAME

# QCG-PJ execution
if EXEC_PJ:
    # GCG-PJ wrapper
    import easypj
    from easypj import TaskRequirements, Resources
    from easypj import Task, TaskType, SubmitOrder

    qcgpjexec = easypj.Executor()
    qcgpjexec.create_manager(dir=my_campaign.campaign_dir, log_level='info')

    qcgpjexec.add_task(Task(
        TaskType.EXECUTION,
        TaskRequirements(cores=Resources(exact=1)),
        application='python3 ' + cmd
    ))

    qcgpjexec.run(
        campaign=my_campaign,
        submit_order=SubmitOrder.EXEC_ONLY
    )

    qcgpjexec.terminate_manager()

# Local execution
else:
    my_campaign.apply_for_each_run_dir(uq.actions.ExecuteLocal(cmd, interpret='python3'))

time_end = time.time()
print('Time for phase 4', time_end-time_start)
time_start = time.time()

# Collater
my_campaign.collate()

time_end = time.time()
print('Time for phase 5', time_end-time_start)
time_start = time.time()

# Post-processing analysis
my_campaign.apply_analysis(uq.analysis.PCEAnalysis(sampler=my_campaign.get_active_sampler(), qoi_cols=qoi_cols))

time_end = time.time()
print('Time for phase 6', time_end-time_start)
time_start = time.time()

# Get Descriptive Statistics
results = my_campaign.get_last_analysis()
stats = results['statistical_moments']['te']
per = results['percentiles']['te']
sobols = results['sobols_first']['te']
sobols_tot = results['sobols_total']['te']
#rho = results['statistical_moments']['rho']['mean']
#rho_norm = results['statistical_moments']['rho_norm']['mean']

time_end = time.time()
print('Time for phase 7', time_end-time_start)
time_start = time.time()

my_campaign.save_state("campaign_state.json")

# Restart campaign
old_campaign = uq.Campaign(state_file="campaign_state.json", work_dir=tmpdir)

pickle.dump(results, open('jet_results.pickle','bw'))
saved_results = pickle.load(open('jet_results.pickle','br'))

time_end = time.time()
print('Time for phase 8', time_end-time_start)

plt.switch_backend('agg')
# Just for interactive plots:
#plt.ion()

# to get rho directly
import jet
Te, ne, rho, rho_norm = jet.solve_Te(plots=False)

fig1 = plt.figure()
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
fig1.savefig("fig1")
plt.close(fig1)

#fig2 = plt.figure()
#for k in sobols.keys(): plt.plot(rho, sobols[k][0], label=k)
fig2, axs2 = plt.subplots(nrows=4, ncols=4, sharex=True, sharey=True)
for i, k in enumerate(sobols.keys()):
    plt.plot(rho, sobols[k][0], label=k)
    ax = axs2[i//4, i%4]
    ax.plot(xrho, sobols[k][0])
    ax.grid()
    ax.set_title(k)
#plt.legend(loc=0)
#plt.xlabel('rho [m]')
#plt.ylabel('sobols_first')
#plt.title(my_campaign.campaign_dir)
plt.title('sobols_first')
fig2.savefig("fig2")
plt.close(fig2)

#fig3 = plt.figure()
#for k in results['sobols_total']['te'].keys(): plt.plot(rho, results['sobols_total']['te'][k][0], label=k)
fig3, axs3 = plt.subplots(nrows=4, ncols=4, sharex=True, sharey=True)
for i, k in enumerate(sobols_tot.keys()):
    plt.plot(rho, sobols_tot[k][0], label=k)
    ax = axs3[i//4, i%4]
    ax.plot(xrho, sobols_tot[k][0])
    ax.grid()
    ax.set_title(k)
plt.legend(loc=0)
#plt.xlabel('rho [m]')
#plt.ylabel('sobols_total')
#plt.title(my_campaign.campaign_dir)
plt.title('sobols_total')
fig3.savefig("fig3")
plt.close(fig3)

fig4 = plt.figure()
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
fig4.savefig("fig4")
plt.close(fig4)


"""
5 params:
2 nodes: 80 Cores
PCE REG:
Number of samples:  252

Time for phase 1 1.6128513813018799
Time for phase 2 4.705455780029297
Time for phase 3 1.5110487937927246
Time for phase 4 360.0726363658905
Time for phase 5 5.015333652496338
Time for phase 6 736.4704015254974
Time for phase 7 9.799003601074219e-05
Time for phase 8 0.33681583404541016

Wall time: 18min41s



13 params:


"""
