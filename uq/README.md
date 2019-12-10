## Installation

1. Install [EasyVVUQ](https://easyvvuq.readthedocs.io/en/latest/installation.html) library.
2. Make sure that the standalone code is compiled. Cf. [README.rst](https://github.com/vecma-ipp/MFW/blob/devel/README.rst) in the root directory of the project.
3. Make sure that the that ython interface for the UAL is built (mandatory to use `ascii_cpo` module). Cf.: [README](https://github.com/vecma-ipp/MFW/tree/devel/ual/python_interface).

#### To use the PilotJob
4. Install QCG PilotJob Manager directly from github by typing:
```
pip install --upgrade git+https://github.com/vecma-project/QCG-PilotJob.git@issue_37_monitoring#egg=qcgPilotManager --user
```

## Example Usage

Here we show an example where we describe Python implementation of UQ workflow of the ETS code. 
The script can be found in [ets_uq.py](https://github.com/vecma-ipp/MFW/blob/devel/uq/ets_uq.py), where we examine the uncertainty effects driven by the boundary conditions in the electron and ion temperature ('Te' and 'Ti'). 
The model code for this example is the ETS application [ets_test.f90](https://github.com/vecma-ipp/MFW/blob/devel/standalone/src/ets_test.f90) and it outputs a single CPO file called `ets_coreprof_out.cpo`.


### Step 1: 
We start by application setup:

```python
SYS = os.environ['SYS']                               # Machine name, cf. config file in the root folder.
tmp_dir = os.environ['SCRATCH']                       # Working directory: to be defined in .bashrc file.
cpo_dir = os.path.abspath("../workflows/AUG_28906_6") # Location of the CPO files.
xml_dir = os.path.abspath("../workflows")             # Location of the XML and XSD files.
obj_dir = os.path.abspath("../standalone/bin/"+SYS)   # The path to the application executables.
exec_code = "ets_test"                                # The name of ETS application.
```

### Step 2: 
We define Parameter Space: 

```python
# The uncertain parameters 
uncertain_params = {
    "Te_boundary": {
        "type": "float",
        "distribution": "Normal",
        "margin_error": 0.2,
    },
    "Ti_boundary": {
        "type": "float",
        "distribution": "Normal",
           "margin_error": 0.2,
      }
}

# A list of quantities of intersts
output_columns = ["Te", "Ti"] 
```


### Step 3
We create the Campaign object. It is the main EasyVVUQ component that coordinates the UQ workflow and acts as an interface to a database. 

```python
campaign_name = "uq_ets"
my_campaign = uq.Campaign(name=campaign_name, work_dir=tmp_dir)

```

We specify three necessary EasyVVUQ objects
- The Encoder: to make the generated samples understandable by the application.
- The Decoder: to convert the output into a form that can be analysed.
- The Collater: to aggreate output data in a single data structure for analysis.

```python
input_filename = "ets_coreprof_in.cpo" # to read the initial parameters and 
encoder = CPOEncoder(template_filename=input_filename,
                     target_filename="ets_coreprof_in.cpo",
                     common_dir=common_dir,
                     uncertain_params=uncertain_params,
                     cpo_name="coreprof",
                     link_xmlfiles=True)

# To specify the distributions of the uncertain parameters to vary
params, vary = encoder.draw_app_params()

output_filename = "ets_coreprof_out.cpo" # contains the quantities of interest values
decoder = CPODecoder(target_filename=output_filename,
                     cpo_name="coreprof",
                     output_columns=output_columns)

collater = uq.collate.AggregateSamples(average=False)

my_campaign.add_app(name=campaign_name,
                    params=params,
                    encoder=encoder,
                    decoder=decoder,
                    collater=collater)
```


### Step 4
In order to generate samples, we define a correspondant Sampler `PCESampler` based on the Polynomial Chaos Expansion and we associate it with the campaign object created in the previous step.

```python
my_sampler = uq.sampling.PCESampler(vary=vary, polynomial_order=4)
my_campaign.set_sampler(my_sampler)
```

### Step 5
To achieve the run of the ETS application for each sample, we draw samples to produce the appropriate input files
 and create directory for each run. 
```python
my_campaign.draw_samples()
my_campaign.populate_runs_dir()
exec_path = os.path.join(obj_dir, exec_code) # the application path
my_campaign.apply_for_each_run_dir(uq.actions.ExecuteLocal(exec_path))
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
