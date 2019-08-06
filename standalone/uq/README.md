## Installation

1. Install [EasyVVUQ](https://easyvvuq.readthedocs.io/en/latest/installation.html) library.
2. Make sure that the standalone code is compiled. Cf. [README.rst](https://github.com/vecma-ipp/MFW/blob/devel/README.rst) in the root directory of the project.
3. Compile the wrapper codes by performing `make` commande in the wrappers folder.

## Example Usage

Here we show an example where we describe Python implementations of UQ worflow of the ETS code. 
The script can be found in [ets_uq_test.py](https://github.com/vecma-ipp/MFW/blob/devel/standalone/uq/test_uq_ets.py), where we examine the effect of uncertainties from initial conditions in ion and election temperature (te and ti).

The input files for this example are the ETS application (wrappers/ets_run.f90) and an input template (inputs/bounadries.template). 


The usage of the ETS application is:

    ets_run <input_file>

It outputs a single file called output.csv, which has two columns ‘Te’ and ‘Ti’.
The template will be used to generate files called input.nml that will be the input to each run of ets_run.

### Step 1: 
We start by application setup

```python
SYS = os.environ['SYS']                             # Machine name, cf. config file in the root folder.
tmp_dir = os.environ['SCRATCH']                     # Working directory: to be defined in .bashrc file.
cpo_dir = os.path.abspath("../data/TESTS/")         # Location of the CPO files.
ets_run = os.path.abspath("../bin/"+SYS+"/ets_run") # The path to the executable of ETS application.
```

### Step 2: 
We define Parameter Space that refelects the provided options in the boudaries.template, i.e.: the list of uncertain parameters and the output file name. 

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
