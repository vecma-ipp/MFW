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




################################################################################
#     configuration of commands                                                #
################################################################################
#cxa.env["init:command"] = "../kernels/bin/GW/init_ual_kernelB"
cxa.env["init:command"] = "../kernels/bin/GW/continue_kernelB"
cxa.env["transp:command"] = "../kernels/bin/GW/ets_kernelB"
if EVOLVE_EQUIL
#  cxa.env["equil:command"] = "../kernels/bin/GW/bdseq_kernelB"
  cxa.env["equil:command"] = "../kernels/bin/GW/chease_kernelB"
end
cxa.env["turb:command"] = "../kernels/bin/GW/bohmgb_kernelB"
#cxa.env["turb:command"] = "../kernels/bin/GW/gem_kernelB"




################################################################################
#     setting parameters                                                       #
################################################################################

# global <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
cxa.env["time_init"] = 0.0
cxa.env["time_end"] = 0.1#1.0
cxa.env["time_step"] = TAU
cxa.env["step0"] = 0
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
cxa.env["transp:pseudo_conv"] = true
cxa.env["transp:limit_evo"] = false
#
cxa.env["transp:d_max_evo"] = 0.0
cxa.env["transp:d_ceil"] = 20.0
cxa.env["transp:d_floor"] = 0.5
cxa.env["transp:d_limit"] = true
cxa.env["transp:core_d_cst"] = true
cxa.env["transp:edge_d_cst"] = true
#
cxa.env["transp:v_max_evo"] = 0.0
cxa.env["transp:v_ceil"] = 0.0
cxa.env["transp:v_floor"] = 0.0
cxa.env["transp:v_limit"] = true
cxa.env["transp:core_v_cst"] = false
cxa.env["transp:edge_v_cst"] = true
#
cxa.env["transp:end_loop"] = true
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


# bohmgb submodel <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
cxa.env["turb:cus_ftubes"] = true
cxa.env["turb:N_ftubes"] = 10
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>




################################################################################
#     declare kernels                                                          #
################################################################################
cxa.add_kernel('init', 'muscle.core.standalone.NativeKernel')
cxa.add_kernel('transp', 'muscle.core.standalone.NativeKernel')
if EVOLVE_EQUIL
  cxa.add_kernel('equil', 'muscle.core.standalone.NativeKernel')
end
cxa.add_kernel('turb', 'muscle.core.standalone.NativeKernel')
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
cs.attach('turb' => 'transp') {
  tie('coretransp_out','coretransp_in')
}

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
