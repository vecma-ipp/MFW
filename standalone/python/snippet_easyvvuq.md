We use Chaospy to create the distributions, and create a parameter dictionary:

```python
# Input file containing information about parameters read from ets_coretransp_in.cpo
input_json = "coretransp.json"

# 1. Initialize `Campaign` object
my_campaign = uq.Campaign(state_filename=input_json, workdir=tmp_dir)

# 2. Set which parameters we wish to include in the analysis and the
#    join probability distribution
number_of_params = 8
list_dist = [uq.distributions.normal(diff_eff[i], 0.2*diff_eff[i]) for i in range(number_of_params)]
my_campaign.vary_param("diff_eff", dist=uq.joint(list_dist))

# # Generate orthogonal polynomials corresponding to the joint disctribution
P = cp.orth_ttr(k, dist)

# 3. Determine the runs to be executed in order to sample the parameter space.
#    Settings for the chosen number of runs are produced using `Sampler`s
#    which determine how combinations of parameters are selected for each one

# First we create three samples where the varying parameter ()"mu", the mean)
# is chosen directly from the selected distribution. If multiple parameters
# were allowed to vary then all would be sampled independently.
number_of_samples = 8
random_sampler = uq.elements.sampling.RandomSampler(my_campaign)
my_campaign.add_runs(random_sampler, max_num=number_of_samples)

# 4. Create directories containing inputs for each run containing the
#    parameters determined by the `Sampler`(s).
#    This makes use of the `Encoder` specified in the input file.
my_campaign.populate_runs_dir()

# 5. Run execution - note this method of running all jobs is just for demo
#    purposes.
my_campaign.apply_for_each_run_dir(
        uq.actions.ExecuteLocal("gauss.py gauss_in.json"))

# 6. Aggregate the results from all runs.
#    This makes use of the `Decoder` selected in the input file to interpret the
#    run output and produce data that can be integrated in a summary pandas
#    dataframe.

output_filename = my_campaign.params_info['out_file']['default']
output_columns = ['Value']

aggregate = uq.elements.collate.AggregateSamples(
                                                my_campaign,
                                                output_filename=output_filename,
                                                output_columns=output_columns,
                                                header=0,
                                                average=True
                                                )
aggregate.apply()

# 7. Run analysis - in this case generate bootstrap estimate of the mean and
#    accompanying error bars.
ensemble_boot = uq.elements.analysis.EnsembleBoot(my_campaign)
results, output_file = ensemble_boot.apply()

# These lines output the analysed data and a log of the steps performed.
pprint(results)

pprint(my_campaign.log)

# The saved state of the `Campaign` contains the state of all runs and can be
# loaded to perform further sampling or analysis.
my_campaign.save_state('final_state.json')

```
