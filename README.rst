Welcome to MFW
=================
MFW is Multiscale Fusion application using transport, equilibrium and turbulence submodels coupled together.

Installation
------------

The code can be installed by cloning the Github repository::

    git clone git@github.com:vecma-ipp/MFW.git
    cd MFW
    
The Standalone code can be compiled by using:: 

    make standalone MPI=yes
    
     
In this case, the modules that should be loaded are: **intel**, **impi**, **mkl** and **fftw-mpi**.
