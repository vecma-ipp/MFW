
SYS=COBRA
orig_repo_loc=/u/yyudin/code/MFW
new_run_dir=gem_runs_check

mkdir ${new_run_dir}

# starting clone fresh MFW repo at dev branch

cd ../working

git clone git@github.com:vecma-ipp/MFW.git

cd MFW  

# find all coreprof.cpo files

for f in $(find . -type f -name *coreprof*cpo); do

    # go to original repo directory and create a new folder for every original cpo file

    d_path=$(dirname ${f} | sed "s|^\.\/||")
    d_name=$(basename ${f} .cpo)

    mkdir -p ${orig_repo_loc}/${new_run_dir}/${d_path}/${d_name}

    cp ${f}   ${orig_repo_loc}/${new_run_dir}/${d_path}/${d_name}/gem_coreprof_in.cpo

    # copy xml, xsd, sh, cpo-s and exectuables to every new directory

    # if the folder contains and equilibrium file, copy it, else get a default one
    # file naming assumption might fail, especially if there are multiple equilibrium files in a folder
    if [ -e ./${d_path}/*equilibrium*.cpo ] 
    then
      echo "using local equilibrium in "${d_path}/${d_name}
      cp ./${d_path}/*equilibrium*.cpo  ${orig_repo_loc}/${new_run_dir}/${d_path}/${d_name}/gem_equilibrium_in.cpo
    else
      echo "using default equilibrium in "${d_path}/${d_name}     
      # the only directory with multiple equilibria (for negative triangularity) should be covered by this
      cp ${orig_repo_loc}/workflows/AUG_28906_6/ets_equilibrium_in.cpo   ${orig_repo_loc}/${new_run_dir}/${d_path}/${d_name}/gem_equilibrium_in.cpo
    fi

    cp ${orig_repo_loc}/workflows/gem_8n_8ft.xml   ${orig_repo_loc}/${new_run_dir}/${d_path}/${d_name}/gem.xml
    cp ${orig_repo_loc}/workflows/gem.xsd   ${orig_repo_loc}/${new_run_dir}/${d_path}/${d_name}/

    cp ${orig_repo_loc}/standalone/bin/${SYS}/loop_gem_notransp   ${orig_repo_loc}/${new_run_dir}/${d_path}/${d_name}/
    cp ${orig_repo_loc}/uq/slurm/gem_loop_nt_8ft.sh   ${orig_repo_loc}/${new_run_dir}/${d_path}/${d_name}/

    # submit a GEM run for every new directory

    cd ${orig_repo_loc}/${new_run_dir}/${d_path}/${d_name}/
    sbatch gem_loop_nt_8ft.sh
    cd /u/yyudin/code/working/MFW/

done

# display the ion heat fluxes at time step #100 for all the runs

grep -iRn ti_transp%flux --include="gem_coretransp_0100.cpo" -A 5

