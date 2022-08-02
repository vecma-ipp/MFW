#!/bin/bash

cd gem_data/

qs=('te' 'ti' 'ne' 'ni')
ts=('gem' 'resuq' 'stats' 'fft' 'scan' 'pdf' 'hist')
ns=1

for q in ${qs[@]} ; do

  mkdir ${q}

  for t in ${ts[@]} ; do	  
   
    mkdir ${q}/${t}/
    #mv ${q}/${t}_* ${q}/${t}/
   
    for n in `seq 1 ${ns}`; do
   
      mkdir ${q}/${t}/${n}/
    
      mv ${t}*${q}*_${n}* ${q}/${t}/${n}/
   
    done
  done
done


