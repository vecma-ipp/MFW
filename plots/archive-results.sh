#!/bin/bash 

if [ $# != 2 ]; then
    echo "Usage: $0 <archive dir> <results dir>"
    exit 0
fi

ADDFILES=""
if [ -f $2/imp4dv_coretransp_0000.cpo ]; then
    ADDFILES+="$2/imp4dv_coretransp_????.cpo"
fi

tar -cvzf $1/$2.tgz $2/ets_coreprof_????.cpo $2/gem_coretransp_????.cpo ${ADDFILES} $2/chease_equilibrium_????.cpo $2/h??.dat $2/t??.dat $2/*.xml $2/*.cxa.rb $2/fout_* $2/*.err $2/*.out 
