## Installation

1. Make sure that the standalone code is compiled. Cf. [README.rst] (https://github.com/vecma-ipp/MFW/blob/devel/README.rst) in the root directory of the project.
2. Compile the wrapper codes by performing `make` commande in the wrappers folder.
3. Install [EasyVVUQ] (https://easyvvuq.readthedocs.io/en/latest/installation.html) library.


## Example Usage

Here we show an example where we describe Python implementations of UQ worflow of the ETS code. 
The script can be found in [ets_uq_test.py] (https://github.com/vecma-ipp/MFW/blob/devel/standalone/uq/test_uq_ets.py), where we examine the effect of uncertainties from initial conditions in ion and election temperature (te and ti).

The input files for this example are the ETS application (wrappers/ets_run.f90) and an input template (inputs/bounadries.template). 


The usage of the ETS application is:

    ets_run <input_file>

It outputs a single file called output.csv, which has two columns ‘te’ and ‘ti’.
The template will be used to generate files called input.nml that will be the input to each run of ets_run.

### Step 1: 
We start by application setup

```python
SYS = os.environ['SYS']                              # Machine name (cf. config file in the root folder).
mp_dir = os.environ['SCRATCH']                       # Working directory: to be defined in the .bashrc file.
cpo_dir = os.path.abspath("../data/TESTS/")          # Location of the CPO files.
ets_run = os.path.abspath("../bin/"+SYS+"/ets_run ") # The path to the executable of ETS application.
uncertain_params = ["Te_boundary", "Ti_boundary"]    # Uncertain paramters list.
```

### Step 2: 
We define Parameter Space that refelects the provided options in the boudaries.template, i.e.: the list of uncertain parameters and the output file name. 

```python
params = {
    uncertain_params[0]: {
        "type": "float",
        "default": "113."
    },
    uncertain_params[1]: {
        "type": "float",
        "default": "180."
    },
    "out_file": {
        "type": "string",
        "default": "output.csv"
    }
}

output_columns = ["te", "ti"] # List of quantities of interest.
```

### Step 3
We creat the Campaign object. It is the main EasyVVUQ component that coordinates the UQ workflow and acts as an interface to a database (CampaignDB) which will store information about the application etc. ...


```python
my_campaign = uq.Campaign(name = 'uq_ets', work_dir=tmp_dir)

```

Specify: Encoder and Collater.

### Step 4
Create Sampler 


### Step 5
Run the application.

### Step 6
Collate outputs and run analysis.
