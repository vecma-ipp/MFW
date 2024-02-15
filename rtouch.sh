#!/bin/bash

#gotoscr
cd /ptmp/yyudin/

folders=( UQ_8FTgem0_abcd1234/ UQ_8FTgem_csldvnei/ )

for folder in ${folders[@]}; do
  find ${folder}/ -type f -exec touch {} +
done

