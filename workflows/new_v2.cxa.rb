################################################################################
###   configuration file for a MUSCLE CxA                                    ###
################################################################################
abort "this is a configuration file to be used with the MUSCLE bootstrap utility" if __FILE__ == $0




################################################################################
#     WORKFLOW LOGIC OPTIONS                                                   #
################################################################################
# some options 
# could use optparse class
TAU = 0.01
SYS = "MARCONI"
CORES = 1024

################################################################################
#     declare kernels                                                          #
################################################################################
init     = NativeInstance.new('init', '../kernels/bin/'+SYS+'/continue_kernelB')
transp   = NativeInstance.new('transp', '../kernels/bin/'+SYS+'/ets_kernelB')
equil    = NativeInstance.new('equil', '../kernels/bin/'+SYS+'/chease_kernelB')
f2dv     = NativeInstance.new('f2dv', '../kernels/bin/'+SYS+'/imp4dv_kernelB')
turb     = MPIInstance.new('turb', '../kernels/bin/'+SYS+'/gem_kernelB', mpiexec_args: "-np #{CORES}")
dupEquil = Instance.new('dupEquil', 'muscle.core.kernel.DuplicationMapper')
dupCorep = Instance.new('dupCorep', 'muscle.core.kernel.DuplicationMapper')


################################################################################
#     setting parameters                                                       #
################################################################################

# global <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
$env['time_step'] = TAU
$env['init_step'] = 0
$env['target_step'] = 0
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


# init submodel <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
init['solve_Ni'] = 0
init['solve_Ne'] = 0
init['solve_psi'] = 0
init['n_add_coef'] = "0.0"
init['n_mult_coef'] = "1.0"
#
init['continue_path'] = "AUG_28906_4/"
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


# transport submodel <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
transp['solver_type'] = 4
transp['sigma_source'] = 0
transp['quasi_neut'] = 0
transp['tau'] = TAU
transp['amix'] = 1.0
transp['amixtr'] = 1.0
transp['conv'] = "1.e0"
transp['convrec'] = "1.e-4"
transp['ohmic_heating_multiplier'] = 1.0
#
transp['floor_alpha'] = 0.01
transp['floor_beta'] = 0.0
transp['floor_gamma'] = 0.0
#
transp['inner_steps_init'] = 1
transp['inner_steps_limit'] = 1000000
transp['inner_steps_incr_factor'] = 10
transp['limit_te_deviation'] = 0.2
transp['limit_dte_deviation'] = 0.1
#
transp['limit_evo'] = false
#
transp['d_max_evo'] = 0.0
transp['d_ceil'] = 2000.0
transp['d_floor'] = 0.5
transp['d_limit'] = true
transp['core_d_cst'] = true
transp['edge_d_cst'] = true
#
transp['v_max_evo'] = 0.0
transp['v_ceil'] = 0.0
transp['v_floor'] = 0.0       #make sure v is 0 at the core
transp['v_limit'] = false
transp['core_v_cst'] = false  #make sure v is 0 at the core
transp['edge_v_cst'] = true
#
transp['end_loop'] = true
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


# f2dv submodel <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
f2dv['f_limit'] = true
f2dv['f_floor'] = 0.0
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>




################################################################################
#     configure connection scheme                                              #
################################################################################

#=====init========================================
init.couple(transp, {
   'equilibrium_out' => 'equilibrium_init', 
   'coreprof_out' => 'coreprof_init', 
   'coretransp_out' => 'coretransp_init', 
   'coresource_out' =>'coresource_init', 
   'coreimpur_out' =>'coreimpur_init',
   'toroidfield_out' =>'toroidfield_init'
})
#init.couple(transp, 'inputCPOs')

#=====transp======================================
transp.couple(equil, 'equilibrium_out' => 'equilibrium_in')
transp.couple(dupCorep, 'coreprof_out')

#=====equil=======================================
equil.couple(dupEquil, 'equilibrium_out')

#=====turb========================================
dupCorep.couple(f2dv, 'corep3' => 'coreprof_in')
dupEquil.couple(f2dv, 'equil3' => 'equilibrium_in')
turb.couple(f2dv, 'coretransp_out' => 'coretransp_in')
f2dv.couple(transp, 'coretransp_out' => 'coretransp_in')

#=====dup=========================================
dupCorep.couple(turb, 'corep1' => 'coreprof_in')
dupCorep.couple(transp, 'corep2' => 'coreprof_in')
dupEquil.couple(turb, 'equil1' => 'equilibrium_in')
dupEquil.couple(transp, 'equil2' => 'equilibrium_in')

