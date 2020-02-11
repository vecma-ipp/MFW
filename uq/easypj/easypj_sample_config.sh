#!/bin/bash

# WORKS ONLY IN BASH - SHOULD BE CHANGED (EG. TO GLOBAL PATHS) IN CASE OF OTHER INTERPRETERS
this_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
this_file=$(basename "${BASH_SOURCE[0]}")

PYTHONPATH="${PYTHONPATH}:${this_dir}"
ENCODER_MODULES="custom_encoder"
export PYTHONPATH
export ENCODER_MODULES

export EASYPJ_CONFIG=$this_dir/$this_file
