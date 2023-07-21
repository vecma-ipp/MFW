#!/bin/bash -l

n_list=(1 4 5 8 16 32)

for n in ${!n_list[@]}; do

	# TODO: substitute expression with a variable in sed
      	sed "s/<npess> .* <\/npess>/<npess> \${n} <\/npess>/g" gem.xml > gem.xml.new.${n}
	
	# TODO: substitute expression with a result of an arithmetic operation in sed
	sed "s/srun .* .\/COBRA\//srun -n \$((\${n}*8))/g" gem_test_batch.sh > gem_test_batch.sh.new.${n} 

done

