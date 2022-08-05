#!/bin/bash

cd gem_data/

# Put ouputs after GEM postprocessing into quantity/plot/runnum folder structure

qs=('te' 'ti' 'ne' 'ni')
ts=('gem' 'resuq' 'stats' 'fft' 'scan' 'pdf' 'hist')
ns=1
nf=6

for q in ${qs[@]} ; do

  mkdir ${q}

  for t in ${ts[@]} ; do	  
   
    mkdir ${q}/${t}/
    #mv ${q}/${t}_* ${q}/${t}/
   
    for n in `seq ${ns} ${nf}`; do
   
      mkdir ${q}/${t}/${n}/
    
      mv ${t}*${q}*_${n}* ${q}/${t}/${n}/
   
    done
  done
done


