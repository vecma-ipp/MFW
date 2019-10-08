import chaospy as cp
import easyvvuq as uq

__author__ = 'Jalal Lakhlili'
__license__ = "LGPL"


def test_pce(tmpdir):

    # Set up a fresh campaign called "pce"
    my_campaign = uq.Campaign(name='pce', work_dir=tmpdir)

    # Define parameter space
    params = {
        "kappa": {
            "type": "real",
            "min": "0.0",
            "max": "0.1",
            "default": "0.025"},
        "t_env": {
            "type": "real",
            "min": "0.0",
            "max": "40.0",
            "default": "15.0"},
        "out_file": {
            "type": "str",
            "default": "output.csv"}}

    output_filename = params["out_file"]["default"]
    output_columns = ["te", "ti"]

    # Create an encoder and decoder for PCE test app
    encoder = uq.encoders.GenericEncoder(
        template_fname='tests/cooling/cooling.template',
        delimiter='$',
        target_filename='cooling_in.json')
    decoder = uq.decoders.SimpleCSV(target_filename=output_filename,
                                    output_columns=output_columns,
                                    header=0)

    # Add the PCE app (automatically set as current app)
    my_campaign.add_app(name="pce",
                        params=params,
                        encoder=encoder,
                        decoder=decoder
                        )

    # Create a collation element for this campaign
    collater = uq.collate.AggregateSamples(average=False)
    my_campaign.set_collater(collater)

    # Create the sampler
    vary = {
        "kappa": cp.Uniform(0.025, 0.075),
        "t_env": cp.Uniform(15, 25)
    }

    my_sampler = uq.sampling.PCESampler(vary=vary,
                                        polynomial_order=3)

    # Associate the sampler with the campaign
    my_campaign.set_sampler(my_sampler)

    # Will draw all (of the finite set of samples)
    my_campaign.draw_samples()

    my_campaign.populate_runs_dir()
    my_campaign.apply_for_each_run_dir(uq.actions.ExecuteLocal(
        "tests/cooling/cooling_model.py cooling_in.json"))

    my_campaign.collate()

    # Update after here

    # Post-processing analysis
    my_analysis = uq.analysis.PCEAnalysis(sampler=my_sampler,
                                           qoi_cols=output_columns)

    my_campaign.apply_analysis(my_analysis)

    results = my_campaign.get_last_analysis()

    # Get Descriptive Statistics
    stats = results['statistical_moments']['te']
    per = results['percentiles']['te']
    sobols = results['sobol_first_order']['te']

    # Test saving and reloading campaign
    #state_file = tmpdir + "pce_state.json"
    #my_campaign.save_state(state_file)
    #new = uq.Campaign(state_file=state_file, work_dir=tmpdir)
    #print(new)

    return stats, per, sobols


if __name__ == "__main__":

    stats, per, sobols = test_pce("/ptmp/ljala/")

    import matplotlib.pyplot as plt
    import numpy as np
    mean = stats["mean"]
    std = stats["std"]
    p10 = per["p10"]
    p90 = per["p90"]

    s_kappa = sobols["kappa"]
    s_t_env = sobols["t_env"]

    t = np.linspace(0, 200, 150)

    fig1 = plt.figure()

    ax11 = fig1.add_subplot(111)
    ax11.plot(t, mean, 'g-', alpha=0.75, label='Mean')
    ax11.plot(t,  p10, 'b-', alpha=0.25)
    ax11.plot(t,  p90, 'b-', alpha=0.25)
    ax11.fill_between(
        t,
        p10,
        p90,
        alpha=0.25,
        label='90% prediction interval')
    ax11.set_xlabel('Time')
    ax11.set_ylabel('Temperature', color='b')
    ax11.tick_params('y', colors='b')
    ax11.legend()

    ax12 = ax11.twinx()
    ax12.plot(t, std, 'r-', alpha=0.5)
    ax12.set_ylabel('Standard deviation', color='r')
    ax12.tick_params('y', colors='r')

    ax11.grid()
    ax11.set_title('Statistical moments')

    fig2 = plt.figure()
    ax2 = fig2.add_subplot(111)
    ax2.plot(t, s_kappa, 'b-', label=r'$\kappa$')
    ax2.plot(t, s_t_env, 'g-', label=r'$t_{env}$')

    ax2.set_xlabel('Time')
    ax2.set_ylabel('Sobol indices')
    ax2.set_title('First order Sobol indices')
    ax2.grid()
    ax2.legend()

    plt.show()
