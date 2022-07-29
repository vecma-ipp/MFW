#!/bin/bash

cd gem_data/

qs=('te' 'ti' 'ne' 'ni')
ts=('gem' 'resuq' 'stats' 'fft' 'scan')
ns=5

for q in ${qs[@]} ; do
  for t in ${ts[@]} ; do	  
    mkdir ${q}/${t}/
    mv ${q}/${t}_* ${q}/${t}/
    for n in `seq 1 ${ns}`; do
      mkdir ${q}/${t}/${n}/
      mv ${q}/${t}/*_${n}* ${q}/${t}/${n}/
    done
  done
done


