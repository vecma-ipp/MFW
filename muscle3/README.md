# MUSCLE3 application(s)


## Requirements

- muscle3 (>= 0.3.1), libmuscle needs to be installed with fortran support
- currently muscle3 does not fully support Intel toolchain, so better compiling the entire MFW for GCC (see and adapt config if needed)
- components here rely on standalone sources, so standalone should be properly compiled 



## Instructions

### Prepared installs of libmuscle

#### MARCONI

Do `module use -p $WORK/MODULES` (you can add that line to your `~/.bash_profile`), then load `mfw/gnu-6.1.0` that sets all compiler and libs modules from MARCONI to compile and run MFW with GCC toolchains (but for your convenience there is also a version of this module for the Intel toolchain that you can use at the moment when you don't want to compile or run with muscle3: `mfw/intel-17.0`) and `muscle3` modules (no toolchain here at the moment as muscle3 is only compiled for GCC, but there you will be able to choose different versions of muscle3 releases). 
Finally, don't forget to set `SYS=MARCONI-GNU` when doing the build step.

#### GATEWAY

Most depending tools and libs can be loaded with the `itmenv` module. 
For reference, Muscle2 can be set by sourcing `/gh/g2olivh/public/muscle2/compat-1.2/etc/muscle.profile.csh` (or without the `.csh` if you are under bash). 
Muscle3 (0.3.1) has been installed under `/gh/g2olivh/public/muscle3/0.3.1`. While there is no script or module at the moment, you can just set `MUSCLE3_HOME` env variable to this dir, then prepend `$MUSCLE3_HOME/lib` to your `LD_LIBRARY_PATH` and `$MUSCLE3_HOME/lib/pkgconfig` to your `PKG_CONFIG_PATH`. 


### Install MUSCLE3 for Python in a virtual environment

Simply follow these [instructions](https://muscle3.readthedocs.io/en/latest/installing.html#python).


### Building

After making sure that your system has GNU configuration (modules + `SYS` env variable), build standalone from the top level (`cd MFW; make standalone`), then build components executables for muscle3 (`cd muscle3 ; make`).


### Running

Check workflow configuration and parameters in `workflow/fusion.ymmsl`, execute workflow by running the script `cd workflow ; ./gem0_workflow.sh`. 
If you installed muscle3 in Python venv using a different path compared to instructions above, just update the activation line accordingly in this script.




## TODO

- GEM (makefile?) needs some work for being compatible with GCC
- in case GEM cannot be made to work with GCC, it should be possible to use ifort together with libmuscle compiled with GCC (according to Lourens' [instructions](https://muscle3.readthedocs.io/en/latest/installing.html#intel-fortran-compiler))
- makefiles should be slightly reorganized to handle better dependencies (ual -> externals -> standalone -> muscle3)
- muscle2 kernels should also rely on standalone for sake of comparison and avoiding code duplication

