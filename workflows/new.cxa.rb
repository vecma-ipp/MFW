################################################################################
###   configuration file for a MUSCLE CxA                                    ###
################################################################################
abort "this is a configuration file to be used with the MUSCLE bootstrap utility" if __FILE__ == $0




################################################################################
#     configure cxa properties                                                 #
################################################################################
#m = Muscle.LAST
#m.add_classpath File.dirname(__FILE__)#+"[REL_PATH_TO_CLASSES]"
#cxa = Cxa.LAST
#cxa.env["cxa_path"] = File.dirname(__FILE__)




################################################################################
#     WORKFLOW LOGIC OPTIONS                                                   #
################################################################################
# some options 
# could use optparse class
TAU = 0.01
EVOLVE_EQUIL = true
FLUX_TO_DV   = true
NOISE        = false
CONST_TURB   = false
SYS = "GW"
NPROC = "16"

INIT_K   = "CONTINUE"
EQUI_K   = "CHEASE"
TURB_K   = "ORB5"
TRANSP_K = "ETS"

if TURB_K == "DFEFI" or TURB_K == "ORB5"
  FLUX_TO_DV = true
end

################################################################################
#     configuration of commands                                                #
################################################################################
case INIT_K
when "UAL"
  init = NativeInstance.new('init', '../kernels/bin/'+SYS+'/init_ual_kernelB')
when "CONTINUE"
  init = NativeInstance.new('init', '../kernels/bin/'+SYS+'/continue_kernelB')
else
  abort "Unknown init kernel"
end

case TRANSP_K
when "ETS"
  transp = NativeInstance.new('transp', '../kernels/bin/'+SYS+'/ets_kernelB')
when "ETS_DEBUG"
  transp = NativeInstance.new('transp', '../kernels/bin/'+SYS+'/ets_kernelB', args: 'totalview')
else
  abort "Unknown transport kernel"
end

if FLUX_TO_DV
  f2dv = NativeInstance.new('f2dv', '../kernels/bin/'+SYS+'/imp4dv_kernelB')
end

if EVOLVE_EQUIL
  case EQUI_K
  when "BDSEQ"
    equil = NativeInstance.new('equil', '../kernels/bin/'+SYS+'/bdseq_kernelB')
  when "CHEASE"
    equil = NativeInstance.new('equil', '../kernels/bin/'+SYS+'/chease_kernelB')
  else
    abort "Unknown equilibrium kernel"
  end
end

if not CONST_TURB
  case TURB_K
  when "BOHMGB"
    turb = NativeInstance.new('turb', '../kernels/bin/'+SYS+'/bohmgb_kernelB')
  when "GEM0"
    turb = NativeInstance.new('turb', '../kernels/bin/'+SYS+'/gem0_kernelB')
  when "GEM"
    turb = MPIInstance.new('turb', '../kernels/bin/'+SYS+'/gem_kernelB', mpiexec_args: '-np '+NPROC)
  when "DFEFI"
    turb = MPIInstance.new('turb', '../kernels/bin/'+SYS+'/dfefi_kernelB', mpiexec_args: '-np '+NPROC)
  when "ORB5"
    turb = MPIInstance.new('turb', '../kernels/bin/'+SYS+'/orb5_kernelB', mpiexec_args: '-np '+NPROC)
  else
    abort "Unknown turbulence kernel"
  end
else
  bypass = NativeInstance.new('bypass', '../kernels/bin/'+SYS+'/bypass_kernelB')
  dupCoretInit = Instance.new('dupCoretInit', 'muscle.core.kernel.DuplicationMapper')
  dupCoret = Instance.new('dupCoret', 'muscle.core.kernel.DuplicationMapper')
end
dupEquil = Instance.new('dupEquil', 'muscle.core.kernel.DuplicationMapper')
dupCorep = Instance.new('dupCorep', 'muscle.core.kernel.DuplicationMapper')


################################################################################
#     setting parameters                                                       #
################################################################################

# global <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
$env['time_step'] = TAU
$env['init_step'] = 0
$env['target_step'] = 3
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


# init submodel <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
init['solve_Ni'] = 0
init['solve_Ne'] = 0
init['solve_psi'] = 0
init['n_add_coef'] = "0.0"
init['n_mult_coef'] = "1.0"
#=====only for init_ual kernel====================
init['user'] = "denka" #"public" #"olivh"
init['machine'] = "aug"
init['version'] = "4.10b"
init['shot'] = 28906 #87412
init['run'] = 4 #890 #1 #850
init['time'] = 0.05 #49.108 #50.4 #49.1
#=====only for continue kernel====================
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
if FLUX_TO_DV
  f2dv['f_limit'] = true
  f2dv['f_floor'] = 0.0
end
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


if not CONST_TURB
  # turb submodel <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
  case TURB_K
  when "BOHMGB"
    turb['cus_ftubes'] = true
    turb['N_ftubes'] = 8
  end
  #>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
end


################################################################################
#     declare kernels                                                          #
################################################################################




################################################################################
#     configure connection scheme                                              #
################################################################################
#cs = cxa.cs

#=====init========================================
if CONST_TURB
  init.couple(transp, {'equilibrium_out' => 'equilibrium_init', 
                'coreprof_out' => 'coreprof_init', 
                'coresource_out' =>'coresource_init', 
                'coreimpur_out' =>'coreimpur_init',
                'toroidfield_out' =>'toroidfield_init'})
  init.couple(dupCoretInit, 'coretransp_out')
else
  init.couple(transp, {'equilibrium_out' => 'equilibrium_init', 
                'coreprof_out' => 'coreprof_init', 
                'coretransp_out' => 'coretransp_init', 
                'coresource_out' =>'coresource_init', 
                'coreimpur_out' =>'coreimpur_init',
                'toroidfield_out' =>'toroidfield_init'})
end

#=====transp======================================
if EVOLVE_EQUIL 
  transp.couple(equil, {'equilibrium_out' => 'equilibrium_in'})
else
  transp.couple(dupEquil, 'equilibrium_out')
end
transp.couple(dupCorep, 'coreprof_out')

#=====equil=======================================
if EVOLVE_EQUIL
  equil.couple(dupEquil, 'equilibrium_out')
end

if CONST_TURB
  dupCoretInit.couple(transp, 'coretransp_init')
  dupCoretInit.couple(bypass, 'cpo_in_init')
  bypass.couple(dupCoret, 'cpo_out')
  dupCoret.couple(transp, 'coretransp_in')
  dupCoret.couple(bypass, 'cpo_in_loop')
else
  #=====turb========================================
  if FLUX_TO_DV
    dupCorep.couple(f2dv, {'corep3' => 'coreprof_in'})
    dupEquil.couple(f2dv, {'equil3' => 'equilibrium_in'})
    turb.couple(f2dv, {'coretransp_out' => 'coretransp_in'})
    f2dv.couple(transp, {'coretransp_out' => 'coretransp_in'})
  else
    turb.couple(transp, {'coretransp_out' => 'coretransp_in'})
  end

  #=====dup=========================================
  dupCorep.couple(turb, {'corep1' => 'coreprof_in'})
  dupEquil.couple(turb, {'equil1' => 'equilibrium_in'})
end
dupCorep.couple(transp, 'coreprof_in')
dupEquil.couple(transp, 'equilibrium_in')
