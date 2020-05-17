#! /usr/bin/env python
import os
import easyvvuq as uq
import chaospy as cp
import pickle
import time
import numpy as np 
import matplotlib.pylab as plt
from dask.distributed import Client, LocalCluster
### from dask_jobqueue import SLURMCluster

if __name__ == '__main__':      ### This is needed if you are using a local cluster; see https://github.com/dask/dask/issues/3877#issuecomment-425692984
    
    time_start = time.time()
    # Set up a fresh campaign called "jet_pce."
    my_campaign = uq.CampaignDask(name='jet_pce.')

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
        "nr":       {"type": "integer", "min": 10,    "max": 1000,   "default": 100}, 
        "dt":       {"type": "float",   "min": 1e-3,  "max": 1e3,    "default": 100},
        "out_file": {"type": "string",  "default": "output.csv"}
    }
    """ Snippet for writing the template file
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
    print('Number of samples = %s' % my_campaign.get_active_sampler().count)

    time_end = time.time()
    print('Time for phase 2', time_end-time_start)
    time_start = time.time()

    my_campaign.populate_runs_dir()

    time_end = time.time()
    print('Time for phase 3', time_end-time_start)
    time_start = time.time()

    ### cluster = SLURMCluster(job_extra=['--qos=p.tok.2h'], queue='p.tok', cores=32, memory='180 GB', processes=32)
    ### cluster.scale(32)
    ### print(cluster.job_script())
    ### client = Client(cluster)

    ### cluster = LocalCluster()
    ### cluster
    ### client = Client(cluster)
    ### cluster.scale(4)

    from dask.distributed import Client
    client = Client(processes=True, n_workers=4, threads_per_worker=1)
    print(client)

    cwd = os.getcwd()
    cmd = f"{cwd}/jet_model.py jet_in.json"
    print(cmd)
    my_campaign.apply_for_each_run_dir(uq.actions.ExecuteLocal(cmd, interpret='python3'), client)

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
    plt.plot(rho, per['p10'].ravel(), 'b:', label='10 and 90 percentiles')
    plt.plot(rho, per['p90'].ravel(), 'b:')
    plt.fill_between(rho, per['p10'].ravel(), per['p90'].ravel(), color='b', alpha=0.1)
    plt.fill_between(rho, [r.lower[0] for r in results['output_distributions']['te']], [r.upper[0] for r in results['output_distributions']['te']], color='b', alpha=0.05)
    plt.legend(loc=0)
    plt.xlabel('rho [m]')
    plt.ylabel('Te [eV]')
    plt.title(my_campaign.campaign_dir)
    plt.savefig('Te.png')

    plt.figure() 
    for k in sobols.keys(): plt.plot(rho, sobols[k][0], label=k)
    plt.legend(loc=0)
    plt.xlabel('rho [m]')
    plt.ylabel('sobols_first')
    plt.title(my_campaign.campaign_dir)
    plt.savefig('sobols_first.png')

    plt.figure() 
    for k in results['sobols_total']['te'].keys(): plt.plot(rho, results['sobols_total']['te'][k][0], label=k)
    plt.legend(loc=0)    
    plt.xlabel('rho [m]')
    plt.ylabel('sobols_total')
    plt.title(my_campaign.campaign_dir)
    plt.savefig('sobols_total.png')

    plt.figure()
    for i, D in enumerate(results['output_distributions']['te']):
        _Te = np.linspace(D.lower[0], D.upper[0], 101)
        _DF = D.pdf(_Te)
        plt.loglog(_Te, _DF, 'b-')
        plt.loglog(stats['mean'][i], np.interp(stats['mean'][i], _Te, _DF), 'bo')
        plt.loglog(stats['mean'][i]-stats['std'][i], np.interp(stats['mean'][i]-stats['std'][i], _Te, _DF), 'b*')
        plt.loglog(stats['mean'][i]+stats['std'][i], np.interp(stats['mean'][i]+stats['std'][i], _Te, _DF), 'b*')
        plt.loglog(per['p10'].ravel()[i],  np.interp(per['p10'].ravel()[i], _Te, _DF), 'b+')
        plt.loglog(per['p90'].ravel()[i],  np.interp(per['p90'].ravel()[i], _Te, _DF), 'b+')
    plt.xlabel('Te')
    plt.ylabel('distribution function')
    plt.savefig('distribution_functions.png')

"""
%run -i easyvvuq_jet_dask_tutorial.py
Time for phase 1 0.28307414054870605
Number of samples = 3125
Time for phase 2 29.378397226333618
Time for phase 3 2.4428601264953613
<Client: 'tcp://127.0.0.1:63591' processes=4 threads=4, memory=17.18 GB>
/Users/dpc/src/EasyVVUQ/jet/jet_model.py jet_in.json
Time for phase 4 3159.74453663826
Time for phase 5 449.5551199913025
Time for phase 6 1438.8484630584717
Time for phase 7 1.0728836059570312e-05
Time for phase 8 0.49858593940734863





%run -i easyvvuq_jet_dask_tutorial.py                                   
Time for phase 1 0.47551512718200684
Number of samples = 3125
Time for phase 2 27.04229712486267
Time for phase 3 2.219496250152588
Time for phase 4 4426.590360164642
Time for phase 5 289.60593724250793
Time for phase 6 811.7055320739746
Time for phase 7 2.09808349609375e-05
Time for phase 8 0.3314039707183838





%run -i easyvvuq_jet_dask_tutorial.py                                   
Time for phase 1 0.3907310962677002
Number of samples = 3125
Time for phase 2 26.767902851104736
Time for phase 3 2.9965028762817383
Time for phase 1 0.5438997745513916
Time for phase 1 0.54457688331604
Time for phase 1 0.5456032752990723
Time for phase 1 0.5510129928588867
Number of samples = 3125
Time for phase 2 63.53254795074463
Number of samples = 3125
Time for phase 2 63.56011414527893
Number of samples = 3125
Time for phase 2 64.70177698135376
Number of samples = 3125
Time for phase 2 64.723384141922
Time for phase 3 5.098176002502441
Time for phase 3 5.140179872512817





%run -i easyvvuq_jet_dask_tutorial.py                                   
Time for phase 1 0.39667820930480957
Number of samples = 3125
Time for phase 2 24.74001407623291
Time for phase 3 2.2722489833831787
Time for phase 1 0.5663909912109375
Time for phase 1 0.5660111904144287
Time for phase 1 0.5694892406463623
Time for phase 1 0.5694599151611328
Number of samples = 3125
Time for phase 2 57.98588705062866
Number of samples = 3125
Time for phase 2 58.028733253479004
Number of samples = 3125
Time for phase 2 58.20742201805115
Number of samples = 3125
Time for phase 2 58.28035593032837
Time for phase 3 5.187877893447876
Time for phase 3 5.215036153793335





%run -i easyvvuq_jet_dask_tutorial.py                                  
Time for phase 1 1.1621198654174805
Time for phase 2 112.04156613349915
Time for phase 4 4460.67898607254
Time for phase 5 324.477196931839
Time for phase 6 595.1378879547119
Time for phase 7 0.00030493736267089844
Time for phase 8 0.45113706588745117






Time for phase 1 0.4670450687408447
Time for phase 2 39.2676100730896
Time for phase 3 2.6985158920288086
Time for phase 4 5216.318249940872
Time for phase 5 74.85999393463135
Time for phase 6 537.1869812011719
Time for phase 7 0.002438068389892578
Time for phase 8 0.10825705528259277

"""

