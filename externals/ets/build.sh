#!/bin/tcsh -fe

make SYS=amd64_intel_12.opt clean
make SYS=amd64_intel_12.opt depend
make SYS=amd64_intel_12.opt compile_ets_workflow

make SYS=amd64_intel_12 clean
make SYS=amd64_intel_12 depend
make SYS=amd64_intel_12 compile_ets_workflow
