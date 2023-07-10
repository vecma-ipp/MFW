
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
    
    cp ${orig_repo_loc}/workflows/AUG_28906_6/ets_equilibrium_in.cpo   ${orig_repo_loc}/${new_run_dir}/${d_path}/${d_name}/gem_equilibrium_in.cpo

    cp ${orig_repo_loc}/workflows/gem_1n_8ft.xml   ${orig_repo_loc}/${new_run_dir}/${d_path}/${d_name}/gem.xml
    cp ${orig_repo_loc}/workflows/gem.xsd   ${orig_repo_loc}/${new_run_dir}/${d_path}/${d_name}/

    cp ${orig_repo_loc}/standalone/bin/${SYS}/loop_gem_notransp   ${orig_repo_loc}/${new_run_dir}/${d_path}/${d_name}/
    cp ${orig_repo_loc}/uq/slurm/gem_loop_nt_8ft.sh   ${orig_repo_loc}/${new_run_dir}/${d_path}/${d_name}/

    # submit a GEM run for every new directory

    cd ${orig_repo_loc}/${new_run_dir}/${d_path}/${d_name}/
    sbatch gem_loop_nt_8ft.sh
    cd /u/yyudin/code/working/MFW/

done



