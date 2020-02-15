#!/bin/bash

# WORKS ONLY IN BASH - SHOULD BE CHANGED (EG. TO GLOBAL PATHS) IN CASE OF OTHER INTERPRETERS
this_dir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"
this_file=$(basename "${BASH_SOURCE[0]}")

ENCODER_MODULES="mfw.templates.cpo_encoder;mfw.templates.xml_encoder"
export ENCODER_MODULES

module load impi

export EASYPJ_CONFIG=$this_dir/$this_file
