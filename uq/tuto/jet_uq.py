import os
import easyvvuq as uq
import chaospy as cp

'''
Perform UQ for the workflow Transport-Equilibrium-Turblence.
Uncertainties are driven by the following input parameters:
- Qe_tot: heating power [W].
- H0: position of Gaussian [m].
- Hw: width of Gaussian [m].
- Te_bc: outer edge Te boundary condition [eV].
'''


print('>>> JET_UQ: START')

# Define parameter space
# TODO we can define also the min and max for each param.
params = {
    "Qe_tot": {"type": "float", "default": 2e6},
    "H0": {"type": "float", "default": 0.},
    "Hw": {"type": "float", "default": 0.1},
    "Te_bc": {"type": "float", "default": 100.},
    "out_file": {"type": "string", "default": "output.csv"}
}

# output file for QoI
output_columns = ["Te", "ne"]

# Set up a campaign object
print('>>> Initialize Campaign object')
campaign_name = "tmpuq_"
my_campaign = uq.Campaign(name=campaign_name)

# Create an encoder, decoder and collater
encoder = uq.encoders.GenericEncoder(
    template_fname='jet.template',
    delimiter='$',
    target_filename='jet_in.json')

decoder = uq.decoders.SimpleCSV(target_filename="output.csv",
                                output_columns=output_columns,
                                header=0)

collater = uq.collate.AggregateSamples(average=False)

# Add the app (automatically set as current app)
my_campaign.add_app(name=campaign_name,
                    params=params,
                    encoder=encoder,
                    decoder=decoder,
                    collater=collater)

# Create the sampler
print('>>> Create the sampler')
vary = {
    "Qe_tot": cp.Uniform(0.8*2e6, 1.2*2e6),
    "H0": cp.Uniform(0, 0.2),
    "Hw": cp.Uniform(0.8*0.1, 1.2*0.1),
    "Te_bc": cp.Normal(100, 0.2*100)
}

my_sampler = uq.sampling.PCESampler(vary=vary,
                                    polynomial_order=3,
                                    regression=True)

# Associate the sampler with the campaign
my_campaign.set_sampler(my_sampler)

# Will draw all (of the finite set of samples)
print('>>> Draw Samples- Ns = ', my_sampler._number_of_samples)
my_campaign.draw_samples()

print('>>> Populate runs_dir')
my_campaign.populate_runs_dir()

cwd = os.getcwd()
cmd = f"{cwd}/jet.py jet_in.json"
my_campaign.apply_for_each_run_dir(uq.actions.ExecuteLocal(cmd, interpret='python3'))
my_campaign.collate()

# Post-processing analysis
print('>>> Post-processing analysis')
my_analysis = uq.analysis.PCEAnalysis(sampler=my_sampler,
                                          qoi_cols=output_columns)
my_campaign.apply_analysis(my_analysis)

# Get Descriptive Statistics
print('>>> Get results')
results = my_campaign.get_last_analysis()

stat = {}
sob1 = {}
for qoi in output_columns:
    stat[qoi] = results['statistical_moments'][qoi]
    sob1[qoi] = results['sobols_first'][qoi]

# Save graphics
print('>>> Plot Descriptive Statistics')
import jet
import pylab as plt

Te, ne, rho, rho_norm = jet.solve_Te()

mean = stat["Te"]["mean"]
std = stat["Te"]["std"]

plt.plot(rho, mean, 'b-', alpha=0.6, label='Mean')
plt.plot(rho, mean-std, 'b-', alpha=0.25)
plt.plot(rho, mean+std, 'b-', alpha=0.25)
plt.fill_between(rho, mean-std, mean+std, alpha=0.2, label=r'Mean $\pm$ deviation')
plt.xlabel(r"$\rho$")
plt.ylabel(r"$T_e$")
plt.title("Te profile")
plt.grid()
plt.legend()
plt.show()
print('>>> JET_UQ: END')
