#!/bin/bash -l

export date=21072023
export num=8

mkdir bckp_${date}.${num}

cp gem.xml bckp_${date}.${num}/
cp gem_test_batch.sh bckp_${date}.${num}/
cp gem_coreprof_in.cpo bckp_${date}.${num}/
cp gem_equilibrium_in.cpo bckp_${date}.${num}/

rm p*.dat

mv *.dat bckp_${date}.${num}/
mv fout* bckp_${date}.${num}/
mv stopped bckp_${date}.${num}/
mv gem_coretransp_*.cpo bckp_${date}.${num}/
mv imp4dv_coretransp_*.cpo bckp_${date}.${num}/

