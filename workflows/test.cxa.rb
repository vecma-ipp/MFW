################################################################################
###   configuration file for a MUSCLE CxA                                    ###
################################################################################
abort "this is a configuration file for to be used with the MUSCLE bootstrap utility" if __FILE__ == $0




################################################################################
#     configure cxa properties                                                 #
################################################################################
cxa = Cxa.LAST
cxa.env["cxa_path"] = File.dirname(__FILE__)




################################################################################
#     WORKFLOW LOGIC OPTIONS                                                   #
################################################################################
# some options 
# could use optparse class
TAU = 0.01
EVOLVE_EQUIL = true
FLUX_TO_DV   = true
NOISE        = false
SYS = "MARCONI"

INIT_K   = "CONTINUE"
EQUI_K   = "CHEASE"
TURB_K   = "ORB5"
TRANSP_K = "ETS"

if TURB_K == "DFEFI"
  FLUX_TO_DV = true
end

################################################################################
#     configuration of commands                                                #
################################################################################
case INIT_K
when "UAL"
  cxa.env["init:command"] = "../kernels/bin/"+SYS+"/init_ual_kernelB"
when "CONTINUE"
  cxa.env["init:command"] = "../kernels/bin/"+SYS+"/continue_kernelB"
else
  abort "Unknown init kernel"
end

case TRANSP_K
when "ETS"
  cxa.env["transp:command"] = "../kernels/bin/"+SYS+"/ets_kernelB"
when "ETS_DEBUG"
  cxa.env["transp:command"] = "totalview"
  cxa.env["transp:args"] = "../kernels/bin/"+SYS+"/ets_kernelB"
else
  abort "Unknown transport kernel"
end

if EVOLVE_EQUIL
  case EQUI_K
  when "BDSEQ"
    cxa.env["equil:command"] = "../kernels/bin/"+SYS+"/bdseq_kernelB"
  when "CHEASE"
    cxa.env["equil:command"] = "../kernels/bin/"+SYS+"/chease_kernelB"
  else
    abort "Unknown equilibrium kernel"
  end
end

if FLUX_TO_DV
  cxa.env["f2dv:command"] = "../kernels/bin/"+SYS+"/imp4dv_kernelB"
end

case TURB_K
when "BOHMGB"
  cxa.env["turb:command"] = "../kernels/bin/"+SYS+"/bohmgb_kernelB"
when "GEM0"
  cxa.env["turb:command"] = "../kernels/bin/"+SYS+"/gem0_kernelB"
when "GEM"
  cxa.env["turb:command"] = "../kernels/bin/"+SYS+"/gem_kernelB"
when "DFEFI"
  cxa.env["turb:command"] = "../kernels/bin/"+SYS+"/dfefi_kernelB"
when "ORB5"
  cxa.env["turb:command"] = "../kernels/bin/"+SYS+"/orb5_kernelB"
else
  abort "Unknown turbulence kernel"
end


################################################################################
#     setting parameters                                                       #
################################################################################

# global <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
cxa.env["time_step"] = TAU
cxa.env["init_step"] = 0
cxa.env["target_step"] = 10
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


# turb submodel <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
case TURB_K
when "BOHMGB"
  cxa.env["turb:cus_ftubes"] = true
  cxa.env["turb:N_ftubes"] = 8
else
  case SYS
  when "HELIOS"
    cxa.env["turb:mpiexec_command"] = "srun"
  else
    cxa.env["turb:mpiexec_args"] = "-n 512"
  end
end
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


# init submodel <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
cxa.env["init:solve_Ni"] = 0
cxa.env["init:solve_Ne"] = 0
cxa.env["init:solve_psi"] = 0
cxa.env["init:n_add_coef"] = "0.0"
cxa.env["init:n_mult_coef"] = "1.0"
#=====only for init_ual kernel====================
cxa.env["init:user"] = "denka" #"public" #"olivh"
cxa.env["init:machine"] = "aug"
cxa.env["init:version"] = "4.10b"
cxa.env["init:shot"] = 28906 #87412
cxa.env["init:run"] = 4 #890 #1 #850
cxa.env["init:time"] = 0.05 #49.108 #50.4 #49.1
#=====only for continue kernel====================
cxa.env["init:continue_path"] = "AUG_28906_4/"
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


# transport submodel <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
cxa.env["transp:solver_type"] = 4
cxa.env["transp:sigma_source"] = 0
cxa.env["transp:quasi_neut"] = 0
cxa.env["transp:tau"] = TAU
cxa.env["transp:amix"] = 1.0
cxa.env["transp:amixtr"] = 1.0
cxa.env["transp:conv"] = "1.e0"
cxa.env["transp:convrec"] = "1.e-4"
cxa.env["transp:ohmic_heating_multiplier"] = 1.0
#
cxa.env["transp:floor_alpha"] = 0.01
cxa.env["transp:floor_beta"] = 0.0
cxa.env["transp:floor_gamma"] = 0.0
#
cxa.env["transp:inner_steps_init"] = 1
cxa.env["transp:inner_steps_limit"] = 1000000
cxa.env["transp:inner_steps_incr_factor"] = 10
cxa.env["transp:limit_te_deviation"] = 0.25
cxa.env["transp:limit_dte_deviation"] = 0.15
#
cxa.env["transp:limit_evo"] = false
#
cxa.env["transp:d_max_evo"] = 0.0
cxa.env["transp:d_ceil"] = 200.0
cxa.env["transp:d_floor"] = 0.5
cxa.env["transp:d_limit"] = true
cxa.env["transp:core_d_cst"] = true
cxa.env["transp:edge_d_cst"] = true
#
cxa.env["transp:v_max_evo"] = 0.0
cxa.env["transp:v_ceil"] = 0.0
cxa.env["transp:v_floor"] = 0.0       #make sure v is 0 at the core
cxa.env["transp:v_limit"] = false
cxa.env["transp:core_v_cst"] = false  #make sure v is 0 at the core
cxa.env["transp:edge_v_cst"] = true
#
cxa.env["transp:end_loop"] = true
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


# f2dv submodel <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
if FLUX_TO_DV
  cxa.env["f2dv:f_limit"] = true
  cxa.env["f2dv:f_floor"] = 0.0
end
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>




################################################################################
#     declare kernels                                                          #
################################################################################
cxa.add_kernel('init', 'muscle.core.standalone.NativeKernel')
cxa.add_kernel('transp', 'muscle.core.standalone.NativeKernel')
if EVOLVE_EQUIL
  cxa.add_kernel('equil', 'muscle.core.standalone.NativeKernel')
end
if FLUX_TO_DV
  cxa.add_kernel('f2dv', 'muscle.core.standalone.NativeKernel')
end
case TURB_K
when "GEM"
  cxa.add_kernel('turb', 'muscle.core.standalone.MPIKernel')
when "DFEFI"
  cxa.add_kernel('turb', 'muscle.core.standalone.MPIKernel')
else
  cxa.add_kernel('turb', 'muscle.core.standalone.NativeKernel')
end
cxa.add_kernel('dupEquil', 'muscle.core.kernel.DuplicationMapper')
cxa.add_kernel('dupCorep', 'muscle.core.kernel.DuplicationMapper')




################################################################################
#     configure connection scheme                                              #
################################################################################
cs = cxa.cs

#=====init========================================
cs.attach('init' => 'transp') {
  tie('equilibrium_out', 'equilibrium_init')
  tie('coreprof_out', 'coreprof_init')
  tie('coretransp_out', 'coretransp_init')
  tie('coresource_out', 'coresource_init')
  tie('coreimpur_out', 'coreimpur_init')
  tie('toroidfield_out', 'toroidfield_init')
}

#=====transp======================================
if EVOLVE_EQUIL
  cs.attach('transp' => 'equil') {
    tie('equilibrium_out', 'equilibrium_in')
  }
else
  cs.attach('transp' => 'dupEquil') {
    tie('equilibrium_out')
  }
end
cs.attach('transp' => 'dupCorep') {
  tie('coreprof_out')
}

#=====equil=======================================
if EVOLVE_EQUIL
  cs.attach('equil' => 'dupEquil') {
    tie('equilibrium_out')
  }
end

#=====turb========================================
if FLUX_TO_DV
  cs.attach('dupCorep' => 'f2dv') {
    tie('corep3','coreprof_in')
  }
  cs.attach('dupEquil' => 'f2dv') {
    tie('equil3','equilibrium_in')
  }
  cs.attach('turb' => 'f2dv') {
    tie('coretransp_out','coretransp_in')
  }
  cs.attach('f2dv' => 'transp') {
    tie('coretransp_out','coretransp_in')
  }
else
  cs.attach('turb' => 'transp') {
    tie('coretransp_out','coretransp_in')
  }
end

#=====dup=========================================
cs.attach('dupCorep' => 'turb') {
  tie('corep1','coreprof_in')
}
cs.attach('dupCorep' => 'transp') {
  tie('corep2','coreprof_in')
}
cs.attach('dupEquil' => 'turb') {
  tie('equil1','equilibrium_in')
}
cs.attach('dupEquil' => 'transp') {
  tie('equil2','equilibrium_in')
}

