Welcome to ComPat
=================

ComPat is a H2020 project on Computing Patterns for High Performance Multiscale Computing, for which a Fusion application is being developed using transport, equilibrium and turbulence submodels coupled together.

Installation
------------

The code can be installed by cloning the Github repository::

    git clone git@github.com:vecma-ipp/compat.git
    cd compat
    
The Standalone code can be compiled by using:: 

    make standalone MPI=yes
    
     
In this case, the modules should be loaded are: ``intel``, ``impi``, ``mkl`` and ``fftw-mpi``.
