#!/bin/bash

# TODO: to keep this line?
# initiate virtualenv 
. ~/.virtualenvs/easyvvuq-qcgpj/bin/activate

# Do not change anything below

# Set environment variable for use by EasyVVUQ-QCGPJ integrator
export EASYPJ_CONF="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)/$(basename "${BASH_SOURCE[0]}")"
