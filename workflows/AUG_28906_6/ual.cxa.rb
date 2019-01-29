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
cxa.env["init:command"] = "../kernels/bin/GW/init_ual_kernelB"


################################################################################
#     setting parameters                                                       #
################################################################################

# global <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
cxa.env["time_init"] = 0.0
cxa.env["time_end"] = 0.0#1.0
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
cxa.env["init:user"] = "g2olivh" #"g2denka" #"public" #"olivh"
cxa.env["init:machine"] = "aug" #"jet"
cxa.env["init:version"] = "4.10b"
cxa.env["init:shot"] = 28906 #92436 #28906 #87412
cxa.env["init:run"] = 6 #4 #890 #1 #850
cxa.env["init:time"] = 1.0 #49.108 #50.4 #49.1
#=====only for continue kernel====================
cxa.env["init:continue_path"] = "AUG_28906_4/"
#>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>




################################################################################
#     declare kernels                                                          #
################################################################################
cxa.add_kernel('init', 'muscle.core.standalone.NativeKernel')




################################################################################
#     configure connection scheme                                              #
################################################################################
cs = cxa.cs

