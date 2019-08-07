## Installation

1. Install [EasyVVUQ](https://easyvvuq.readthedocs.io/en/latest/installation.html) library.
2. Make sure that the standalone code is compiled. Cf. [README.rst](https://github.com/vecma-ipp/MFW/blob/devel/README.rst) in the root directory of the project.
3. Compile the wrapper codes by performing `make` command in the wrappers folder (uq/wrappers).

## Example Usage

Here we show an example where we describe Python implementations of UQ workflow of the ETS code. 
The script can be found in [ets_uq_test.py](https://github.com/vecma-ipp/MFW/blob/devel/standalone/uq/test_uq_ets.py), where we examine the uncertainty effect of initial conditions in the electron and ion temperature ('Te' and 'Ti'). 
The input files for this example are the ETS application [wrappers/ets_run.f90](https://github.com/vecma-ipp/MFW/blob/devel/standalone/uq/wrappers/ets_run.f90) and an input template [inputs/boundaries.template](https://github.com/vecma-ipp/MFW/blob/devel/standalone/uq/inputs/boundaries.template). 


The usage of the ETS application is:

    ets_run <input_file>

It outputs a single file called `output.csv`, which has two columns ‘Te’ and ‘Ti’.
The template will be used to generate files called input.nml that will be the input to each run of `ets_run`.

### Step 1: 
We start by application setup

```python
SYS = os.environ['SYS']                             # Machine name, cf. config file in the root folder.
tmp_dir = os.environ['SCRATCH']                     # Working directory: to be defined in .bashrc file.
cpo_dir = os.path.abspath("../data/TESTS/")         # Location of the CPO files.
ets_run = os.path.abspath("../bin/"+SYS+"/ets_run") # The path to the executable of ETS application.
```

### Step 2: 
We define Parameter Space that reflects the provided options in the boudaries.template, i.e.: the list of uncertain parameters and the output file name. 

```python
params = {
    "Te_boundary": {
        "type": "float",
        "default": "113."
    },
    "Ti_boundary": {
        "type": "float",
        "default": "180."
    },
    "out_file": {
        "type": "string",
        "default": "output.csv"
    }
}
# List of quantities of interest.
output_columns = ["Te", "Ti"] 
```

### Step 3
We create the Campaign object. It is the main EasyVVUQ component that coordinates the UQ workflow and acts as an interface to a database. 

```python
my_campaign = uq.Campaign(name = 'uq_ets', work_dir=tmp_dir)

```

We specify three necessary EasyVVUQ objects
- The Encoder: to make the generated samples understandable by the application.
- The Decoder: to convert the output into a form that can be analysed.
- The Collater: to aggreate output data in a single data structure for analysis.

```python
encoder = uq.encoders.GenericEncoder(
    template_fname='inputs/boundaries.template',
    delimiter='#',
    target_filename='input.nml')
    
decoder = uq.decoders.SimpleCSV(target_filename=output_filename,
                                output_columns=output_columns,
                                header=0)

my_campaign.add_app(name="uq_ets",
                    params=params,
                    encoder=encoder,
                    decoder=decoder
                    )

collater = uq.collate.AggregateSamples(average=False)
my_campaign.set_collater(collater)
```


### Step 4
In order to generate samples, we specify the distributions of the uncertain parameters to vary. For the current example we read the initial parameters, 'Te_boundary' and 'Ti_boundary', read from `ets_coreprof_in.cpo` file and we use [Chaospy](https://github.com/jonathf/chaospy) to create the distributions. Afterwards, we define a correspondant Sampler `PCESampler` based on the Polynomial Chaos Expansion and we associate it with the campaign object created in the previous step.

```python
vary = {
    uncertain_params[0]: cp.Normal(Te_boundary, 0.2*Te_boundary),
    uncertain_params[1]: cp.Normal(Ti_boundary, 0.2*Ti_boundary)
}
my_sampler = uq.sampling.PCESampler(vary=vary, polynomial_order=4)

my_campaign.set_sampler(my_sampler)
```


### Step 5
To achieve the run of the ETS application for each sample, we draw samples to produce the appropriate input files
 and create directory for each run. 
```python
my_campaign.draw_samples()
my_campaign.populate_runs_dir()
my_campaign.apply_for_each_run_dir(uq.actions.ExecuteLocal(ets_run + " input.nml"))
```

### Step 6
Finally, we collect the outputs and run analysis.


```python
my_campaign.collate()

analysis = uq.analysis.PCEAnalysis(sampler=my_sampler, qoi_cols=output_columns)
my_campaign.apply_analysis(analysis)
```

Now, we can get the results of the analysis and obtain descriptive statistics.

```python
results = my_campaign.get_last_analysis()

stats_te = results['statistical_moments']['Te']
pctl_te = results['percentiles']['Te']
sobols_te = results['sobols_first']['Te']
```
