################################################################################
# Initialize some variables used in the Makefile.  Some of these might be      #
# redefined in the file included from config                                   #
################################################################################
## Default to compiling on the Gateway
  SYS=amd64_intel_12
## Default version is 4.10b
  UAL_VERSION=4.10b
## which preprocessor to use
  CPP=cpp -traditional
## which C compiler to use
  CC=gcc
## which ranlib to use
  RANLIB=ranlib

################################################################################
# bring in the file obj/SYS if it exists --- this can override the setting of  #
# SYS above                                                                    #
################################################################################
-include obj/SYS

################################################################################
# set the OBJECTCODE variable used in much of the rest of the Makefile         #
################################################################################
OBJECTCODE=obj/${SYS}

################################################################################
# set some more Makefile variables                                             #
################################################################################
## where to find doxygen
  DOXYGEN=bin/linux/doxygen
  DOXYGEN=doxygen
  LIBDPC = /afs/efda-itm.eu/imp3/user/coster/lib  
  LIBDPC = /pfs/home/dpc/public/lib  
#  LIBMATHEVAL = -Wl,--rpath -Wl,${LIBDPC} -L${LIBDPC} -lmatheval
  LIBMATHEVAL = $(shell eval-pkg-config --cflags --libs libmatheval)

################################################################################
# now bring in the configuration for the particular system                     #
################################################################################
include config/${SYS}

################################################################################
# and the instructions for compiling C and fortran code                        #
################################################################################
include obj/compile

################################################################################
# set up the VPATH for searching for source files                              #
################################################################################
VPATH = src/solvers src/ets src/convergence src/types src/analytics src/test 
VPATH += src/workflows src/ets_tools src/neutrals src/sources src/transport src/deltas
VPATH += src/itm_tools src/version
VPATH += src/bdseq src/helena2 src/write_cpo
ifeq ($(ITMTYPES),yes)
else
VPATH += src/itm_types
endif
ifeq ($(ITMCONSTANTS),yes)
else
VPATH += src/itm_constants
endif
ifeq ($(CJK),yes)
else
VPATH += src/itm_shared
endif
ifeq ($(XMLLIB),yes)
else
VPATH += src/xmllib 
endif


################################################################################
# bring in helena if we have it                                                #
################################################################################
ifeq ($(shell [ -d src/helena ] && echo yes || echo no ),yes)
VPATH += src/helena src/helena_lib
DEFINES += -DGOT_HELENA
endif

################################################################################
# bring in chease if we have it                                                #
################################################################################
ifeq ($(shell [ -e ${OBJECTCODE}/libchease.a ] && echo yes || echo no ),yes)
DEFINES += -DGOT_HELENA
LIBCHEASE =  ${OBJECTCODE}/libchease.a
endif

################################################################################
# bring in emeq if we have it                                                  #
################################################################################
ifeq ($(shell [ -d src/emeq ] && echo yes || echo no ),yes)
VPATH += src/emeq
DEFINES += -DGOT_EMEQ
### INCLUDE += -I src/emeq/for
endif

################################################################################
# bring in equilibrium tools if we have it                                     #
################################################################################
ifeq ($(shell [ -d src/equilibrium ] && echo yes || echo no ),yes)
VPATH += src/equilibrium
DEFINES += -DGOT_EQUILIBRIUM
### INCLUDE += -I src/equilibrium/for
endif

################################################################################
# bring in chease if we have it                                                #
################################################################################
ifeq ($(shell [ -e ${OBJECTCODE}/libchease.a  ] && echo yes || echo no ),yes)
DEFINES += -DGOT_CHEASE
CHEASE_LIB = -L${OBJECTCODE} -lchease -linterpos
### -L/afs/efda-itm.eu/user/c/coster/SVN/interpos/trunk/interpos_libs 
### -linterpospgi_wolapack -lblas
endif

################################################################################
# bring in amnsprotolib if we have it                                          #
################################################################################
ifeq ($(AMNS),yes)
DEFINES += -DGOT_AMNSPROTO
else
ifeq ($(shell [ -d src/amnsprotolib ] && echo yes || echo no ),yes)
VPATH += src/amnsprotolib
DEFINES += -DGOT_AMNSPROTO
LIBAMNSPROTO = ${OBJECTCODE}/libamnsproto.a
endif
endif

################################################################################
# more VPATHs for different components                                         #
################################################################################
VPATH += src/neo
VPATH += src/neowes src/etaigb src/gbtransport
VPATH += src/impurity
VPATH += src/cpowork
VPATH += src/perf
VPATH += src/dierckx

################################################################################
# more VPATHs for SPIDER components                                         #
################################################################################
DEFINES += -DGOT_SPIDER
VPATH += src/spider
INCLUDE += -I src/spider
INCLUDE += -I src/spider/include

################################################################################
# bring in neoart if we have it                                                #
################################################################################
ifeq ($(shell [ -d src/neoart ] && echo yes || echo no ),yes)
VPATH += src/neoart
DEFINES += -DGOT_NEOART
LIBNEOART = ${OBJECTCODE}/libneoart.a
INCLUDE += -I src/neoart
endif

################################################################################
# bring in neos if we have it                                                  #
################################################################################
ifeq ($(shell [ -d src/neos ] && echo yes || echo no ),yes)
DEFINES += -DGOT_NEOS
LIBNEOS = -LEXTERNAL/NEOS/ITM -lneos_kepler -L/afs/efda-itm.eu/user/s/sauter/public/interpos/interpos_libs/pgf95 -linterpospgf95
endif

################################################################################
# UAL can have the value                                                       #
#     no: no UAL at all --- use dummy routines                                 #
#    yes: use the ISIP provided UAL routines                                   #
#  local: use a locally compiled version of the UAL                            #
################################################################################
ifeq ($(UAL),yes)
else
ifeq ($(UAL),local)
  VPATH += src/schemas src/UAL
  LIBSCHEMAS = ${OBJECTCODE}/libschemas.a
else
  VPATH += src/schemas src/dummies
  LIBSCHEMAS = ${OBJECTCODE}/libschemas.a
endif
endif

################################################################################
# the CRONOS solver relies on having MUMPS available                           #
################################################################################
ifneq (X$(MUMPS_BASE)X,XX)
DEFINES += -DWANTCOS
INCLUDE += -I ${MUMPS_BASE}/include -I ${MUMPS_BASE}/libseq
LIBS += -L ${MUMPS_BASE}/lib -l dmumps -L ${MUMPS_BASE}/PORD/lib -l pord \
        -L ${MUMPS_BASE}/libseq -l mpiseq  -l blas
endif

################################################################################
# these are the .o files corresponding to main programs that should be in any  #
# library                                                                      #
################################################################################
EXCLUDELIST = ${OBJECTCODE}/solver_test.o ${OBJECTCODE}/eq_test.o \
  ${OBJECTCODE}/ets_workflow.o \
  ${OBJECTCODE}/prepare_input_cpos.o ${OBJECTCODE}/CreateHDF5Model.o \
  ${OBJECTCODE}/profile_check.o \
  ${OBJECTCODE}/scale_current.o ${OBJECTCODE}/wrapper_helena.o \
  ${OBJECTCODE}/wrapper_ual.o ${OBJECTCODE}/printcpo.o ${OBJECTCODE}/sizecpo.o \
  ${OBJECTCODE}/test_equilibrium_augmenter.o ${OBJECTCODE}/run_helena.o  ${OBJECTCODE}/run_chease.o \
  ${OBJECTCODE}/copy_signals.o ${OBJECTCODE}/test_functions.o \
  ${OBJECTCODE}/neo_test.o ${OBJECTCODE}/timescpo.o ${OBJECTCODE}/dump_amns.o \
  ${OBJECTCODE}/run_helena_cpo.o ${OBJECTCODE}/writecpo.o ${OBJECTCODE}/ascii_equilibrium_to_ual
#${OBJECTCODE}/ual_low_level_hdf5.o 

################################################################################
# we build a number of libraries based on the files in various directories     #
# here we set up variables containing the list of .o files in these            #
# directories                                                                  #
################################################################################
ANALYTICS_O = ${filter-out ${EXCLUDELIST},\
  ${addsuffix .o,\
    ${addprefix ${OBJECTCODE}/,\
      ${filter-out *,\
        ${basename \
	  ${notdir \
            ${shell echo src/analytics/*.[fF] src/analytics/*.[fF]90}}}}}}}
CONVERGENCE_O = ${filter-out ${EXCLUDELIST},\
  ${addsuffix .o,\
    ${addprefix ${OBJECTCODE}/,\
      ${filter-out *,\
        ${basename \
	  ${notdir \
            ${shell echo src/convergence/*.[fF] src/convergence/*.[fF]90}}}}}}}
ETS_O = ${filter-out ${EXCLUDELIST},\
  ${addsuffix .o,\
    ${addprefix ${OBJECTCODE}/,\
      ${filter-out *,\
        ${basename \
	  ${notdir \
            ${shell echo src/ets/*.[fF] src/ets/*.[fF]90}}}}}}}
SOLVER_O = ${filter-out ${EXCLUDELIST},\
  ${addsuffix .o,\
    ${addprefix ${OBJECTCODE}/,\
      ${filter-out *,\
        ${basename \
	  ${notdir \
            ${shell echo src/solvers/*.[fF] src/solvers/*.[fF]90}}}}}}}
TEST_O = ${filter-out ${EXCLUDELIST},\
  ${addsuffix .o,\
    ${addprefix ${OBJECTCODE}/,\
      ${filter-out *,\
        ${basename \
	  ${notdir \
            ${shell echo src/test/*.[fF] src/test/*.[fF]90}}}}}}}
WORKFLOWS_O = ${filter-out ${EXCLUDELIST},\
  ${addsuffix .o,\
    ${addprefix ${OBJECTCODE}/,\
      ${filter-out *,\
        ${basename \
	  ${notdir \
            ${shell echo src/workflows/*.[fF] src/workflows/*.[fF]90}}}}}}}
TYPES_O = ${filter-out ${EXCLUDELIST},\
  ${addsuffix .o,\
    ${addprefix ${OBJECTCODE}/,\
      ${filter-out *,\
        ${basename \
	  ${notdir \
            ${shell echo src/types/*.[fF] src/types/*.[fF]90}}}}}}}
SCHEMAS_O = ${filter-out ${EXCLUDELIST},\
  ${addsuffix .o,\
    ${addprefix ${OBJECTCODE}/,\
      ${filter-out *,\
        ${basename \
	  ${notdir \
            ${shell echo src/schemas/*.[fF] src/schemas/*.[fF]90}}}}}}}
DUMMIES_O = ${filter-out ${EXCLUDELIST},\
  ${addsuffix .o,\
    ${addprefix ${OBJECTCODE}/,\
      ${filter-out *,\
        ${basename \
	  ${notdir \
            ${shell echo src/dummies/*.[fF] src/dummies/*.[fF]90}}}}}}}
ifeq ($(XMLLIB),yes)
XMLLIB_O =
else
XMLLIB_O = ${filter-out ${EXCLUDELIST},\
  ${addsuffix .o,\
    ${addprefix ${OBJECTCODE}/,\
      ${filter-out *,\
        ${basename \
	  ${notdir \
            ${shell echo src/xmllib/*.[fF] src/xmllib/*.[fF]90}}}}}}}
endif
ifeq ($(ITMTYPES),yes)
ITM_TYPES_O =
else
ITM_TYPES_O = ${filter-out ${EXCLUDELIST},\
  ${addsuffix .o,\
    ${addprefix ${OBJECTCODE}/,\
      ${filter-out *,\
        ${basename \
	  ${notdir \
            ${shell echo src/itm_types/*.[fF] src/itm_types/*.[fF]90}}}}}}}
endif
ifeq ($(ITMCONSTANTS),yes)
ITM_CONSTANTS_O =
else
ITM_CONSTANTS_O = ${filter-out ${EXCLUDELIST},\
  ${addsuffix .o,\
    ${addprefix ${OBJECTCODE}/,\
      ${filter-out *,\
        ${basename \
	  ${notdir \
            ${shell echo src/itm_constants/*.[fF] src/itm_constants/*.[fF]90}}}}}}}
endif
ifeq ($(CJK),yes)
ITM_SHARED_O = 
else
ITM_SHARED_O = ${filter-out ${EXCLUDELIST},\
  ${addsuffix .o,\
    ${addprefix ${OBJECTCODE}/,\
      ${filter-out *,\
        ${basename \
	  ${notdir \
            ${shell echo src/itm_shared/*.[fF] src/itm_shared/*.[fF]90}}}}}}}
endif
ITM_TOOLS_O = ${filter-out ${EXCLUDELIST},\
  ${addsuffix .o,\
    ${addprefix ${OBJECTCODE}/,\
      ${filter-out *,\
        ${basename \
	  ${notdir \
            ${shell echo src/itm_tools/*.[fF] src/itm_tools/*.[fF]90}}}}}}}
BDSEQ_O = ${filter-out ${EXCLUDELIST},\
  ${addsuffix .o,\
    ${addprefix ${OBJECTCODE}/,\
      ${filter-out *,\
        ${basename \
	  ${notdir \
            ${shell echo src/bdseq/*.[fF] src/bdseq/*.[fF]90}}}}}}}
HELENA_O = ${filter-out ${EXCLUDELIST},\
  ${addsuffix .o,\
    ${addprefix ${OBJECTCODE}/,\
      ${filter-out *,\
        ${basename \
	  ${notdir \
            ${shell echo src/helena/*.[fF] src/helena/*.[fF]90}}}}}}}
HELENA_LIB_O = ${filter-out ${EXCLUDELIST},\
  ${addsuffix .o,\
    ${addprefix ${OBJECTCODE}/,\
      ${filter-out *,\
        ${basename \
	  ${notdir \
            ${shell echo src/helena_lib/*.[fF] src/helena_lib/*.[fF]90}}}}}}}
HELENA2_O = ${filter-out ${EXCLUDELIST},\
  ${addsuffix .o,\
    ${addprefix ${OBJECTCODE}/,\
      ${filter-out *,\
        ${basename \
	  ${notdir \
            ${shell echo src/helena2/*.[fF] src/helena2/*.[fF]90}}}}}}}
NEO_O = ${filter-out ${EXCLUDELIST},\
  ${addsuffix .o,\
    ${addprefix ${OBJECTCODE}/,\
      ${filter-out *,\
        ${basename \
	  ${notdir \
            ${shell echo src/neo/*.[fF] src/neo/*.[fF]90}}}}}}}
EMEQ_O = ${filter-out ${EXCLUDELIST},\
  ${addsuffix .o,\
    ${addprefix ${OBJECTCODE}/,\
      ${filter-out *,\
        ${basename \
	  ${notdir \
            ${shell echo src/emeq/*.[fF] src/emeq/*.[fF]90}}}}}}}
EQUILIBRIUM_O = ${filter-out ${EXCLUDELIST},\
  ${addsuffix .o,\
    ${addprefix ${OBJECTCODE}/,\
      ${filter-out *,\
        ${basename \
	  ${notdir \
            ${shell echo src/equilibrium/*.[fF] src/equilibrium/*.[fF]90}}}}}}}
UAL_O = ${filter-out ${EXCLUDELIST},\
  ${addsuffix .o,\
    ${addprefix ${OBJECTCODE}/,\
      ${filter-out *,\
        ${basename \
	  ${notdir \
            ${shell echo src/UAL/*.c src/UAL/*.cpp}}}}}}}
WRITE_CPO_O = ${filter-out ${EXCLUDELIST},\
  ${addsuffix .o,\
    ${addprefix ${OBJECTCODE}/,\
      ${filter-out *,\
        ${basename \
	  ${notdir \
            ${shell echo src/write_cpo/*.[fF] src/write_cpo/*.[fF]90}}}}}}}
ETS_TOOLS_O = ${filter-out ${EXCLUDELIST},\
  ${addsuffix .o,\
    ${addprefix ${OBJECTCODE}/,\
      ${filter-out *,\
        ${basename \
	  ${notdir \
            ${shell echo src/ets_tools/*.[fF] src/ets_tools/*.[fF]90}}}}}}}
NEUTRALS_O = ${filter-out ${EXCLUDELIST},\
  ${addsuffix .o,\
    ${addprefix ${OBJECTCODE}/,\
      ${filter-out *,\
        ${basename \
	  ${notdir \
            ${shell echo src/neutrals/*.[fF] src/neutrals/*.[fF]90}}}}}}}
SOURCES_O = ${filter-out ${EXCLUDELIST},\
  ${addsuffix .o,\
    ${addprefix ${OBJECTCODE}/,\
      ${filter-out *,\
        ${basename \
	  ${notdir \
            ${shell echo src/sources/*.[fF] src/sources/*.[fF]90}}}}}}}
DELTAS_O = ${filter-out ${EXCLUDELIST},\
  ${addsuffix .o,\
    ${addprefix ${OBJECTCODE}/,\
      ${filter-out *,\
        ${basename \
	  ${notdir \
            ${shell echo src/deltas/*.[fF] src/deltas/*.[fF]90}}}}}}}
TRANSPORT_O = ${filter-out ${EXCLUDELIST},\
  ${addsuffix .o,\
    ${addprefix ${OBJECTCODE}/,\
      ${filter-out *,\
        ${basename \
	  ${notdir \
            ${shell echo src/transport/*.[fF] src/transport/*.[fF]90}}}}}}}
NEOWES_O = ${filter-out ${EXCLUDELIST},\
  ${addsuffix .o,\
    ${addprefix ${OBJECTCODE}/,\
      ${filter-out *,\
        ${basename \
	  ${notdir \
            ${shell echo src/neowes/*.[fF] src/neowes/*.[fF]90}}}}}}}
NEOART_O = ${filter-out ${EXCLUDELIST},\
  ${addsuffix .o,\
    ${addprefix ${OBJECTCODE}/,\
      ${filter-out *,\
        ${basename \
	  ${notdir \
            ${shell echo src/neoart/*.[fF] src/neoart/*.[fF]90}}}}}}}
ETAIGB_O = ${filter-out ${EXCLUDELIST},\
  ${addsuffix .o,\
    ${addprefix ${OBJECTCODE}/,\
      ${filter-out *,\
        ${basename \
	  ${notdir \
            ${shell echo src/etaigb/*.[fF] src/etaigb/*.[fF]90}}}}}}}
GBTRANSP_O = ${filter-out ${EXCLUDELIST},\
  ${addsuffix .o,\
    ${addprefix ${OBJECTCODE}/,\
      ${filter-out *,\
        ${basename \
	  ${notdir \
            ${shell echo src/gbtransport/*.[fF] src/gbtransport/*.[fF]90}}}}}}}
DIERCKX_O = ${filter-out ${EXCLUDELIST},\
  ${addsuffix .o,\
    ${addprefix ${OBJECTCODE}/,\
      ${filter-out *,\
        ${basename \
	  ${notdir \
            ${shell echo src/dierckx/*.[fF] src/dierckx/*.[fF]90}}}}}}}
PERF_O = ${filter-out ${EXCLUDELIST},\
  ${addsuffix .o,\
    ${addprefix ${OBJECTCODE}/,\
      ${filter-out *,\
        ${basename \
	  ${notdir \
            ${shell echo src/perf/*.[fF] src/perf/*.[fF]90}}}}}}}
COPYSIGNALS_O = ${filter-out ${EXCLUDELIST},\
  ${addsuffix .o,\
    ${addprefix ${OBJECTCODE}/,\
      ${filter-out *,\
        ${basename \
	  ${notdir \
            ${shell echo src/cpowork/*.[fF] src/cpowork/*.[fF]90}}}}}}}
IMPURITY_O = ${filter-out ${EXCLUDELIST},\
  ${addsuffix .o,\
    ${addprefix ${OBJECTCODE}/,\
      ${filter-out *,\
        ${basename \
	  ${notdir \
            ${shell echo src/impurity/*.[fF] src/impurity/*.[fF]90}}}}}}}
ifeq ($(AMNS),yes)
AMNSPROTOLIB_O = 
else
AMNSPROTOLIB_O = ${filter-out ${EXCLUDELIST},\
  ${addsuffix .o,\
    ${addprefix ${OBJECTCODE}/,\
      ${filter-out *,\
        ${basename \
	  ${notdir \
            ${shell echo src/amnsprotolib/*.[cfF] src/amnsprotolib/*.[fF]90}}}}}}}
endif
SPIDER_O = ${filter-out ${EXCLUDELIST},\
  ${addsuffix .o,\
    ${addprefix ${OBJECTCODE}/,\
      ${filter-out *,\
        ${basename \
	  ${notdir \
            ${shell echo src/spider/*.[fF] src/spider/*.[fF]90}}}}}}}
ADD_O = 

################################################################################
# now declare the libraries we want, combining some of the directories         #
################################################################################
LIB_ITM_O = ${ITM_TYPES_O} ${ITM_CONSTANTS_O} ${XMLLIB_O} ${ITM_SHARED_O} ${ITM_TOOLS_O}
LIB_ETS_O = ${TYPES_O} ${SOLVER_O} ${ETS_O} ${TEST_O} ${WORKFLOWS_O} ${OBJECTCODE}/ets_version.o
LIB_ANALYTICS_O = ${ANALYTICS_O}
LIB_CONVERGENCE_O = ${CONVERGENCE_O}
LIB_BDSEQ_O = ${BDSEQ_O}
LIB_HELENA_O = ${HELENA_O} ${HELENA_LIB_O}
LIB_HELENA2_O = ${HELENA2_O}
LIB_NEO_O = ${NEO_O}
LIB_EMEQ_O = ${EMEQ_O}
LIB_EQUILIBRIUM_O = ${EQUILIBRIUM_O}
LIB_PERF_O = ${PERF_O}
LIB_ETAIGB_O = ${ETAIGB_O}
LIB_GBTRANSP_O = ${GBTRANSP_O}
LIB_NEOWES_O = ${NEOWES_O}
LIB_NEOART_O = ${NEOART_O}
LIB_ETS_TOOLS_O = ${ETS_TOOLS_O} 
LIB_NEUTRALS_O = ${NEUTRALS_O} 
LIB_SOURCES_O = ${SOURCES_O} 
LIB_DELTAS_O = ${DELTAS_O} 
LIB_TRANSPORT_O = ${TRANSPORT_O} 
LIB_COPYSIGNALS_O = ${COPYSIGNALS_O} 
LIB_IMPURITY_O = ${IMPURITY_O}
LIB_DIERCKX_O = ${DIERCKX_O}
LIB_AMNSPROTO_O = ${AMNSPROTOLIB_O}
LIB_SPIDER_O = ${SPIDER_O}

################################################################################
# the schemas library is only built for the local option from the real sources #
# --- otherwise dummies are used                                               #
################################################################################
ifeq ($(UAL),local)
  LIB_SCHEMAS_O = ${SCHEMAS_O} ${UAL_O}
else
  LIB_SCHEMAS_O = ${SCHEMAS_O} ${DUMMIES_O}
endif

################################################################################
# we have a performance library for some systems                               #
################################################################################
ifeq (${GOT_PERFLIB}, 'yes')
  perflib = -L${OBJECTCODE} -lperf
else
  ADD_O += ${PERF_O}
endif

################################################################################
################################################################################
## and now we define the main programs that can be executed, and how to build ##
## them                                                                       ##
################################################################################
################################################################################

################################################################################
# here is the analytic test case which is also the default program to be run   #
################################################################################
run: ${OBJECTCODE}/solver_test
	@echo 
	@echo
	@echo "*** Now running the code ***"
	@echo 
	@echo
	@mkdir -p data/OUTPUT
	time ${OBJECTCODE}/solver_test
	@echo
	@echo
	@echo "*** Finished running the code ***"
	@echo 
	@echo
	@echo Comparing the current output with an earlier version in data.GW
	@echo
	@echo
	@diff -rwq -x .svn data.GW/ data/
	@echo
	@echo
	@echo No problems should have been reported
	@echo
	@echo

${OBJECTCODE}/solver_test:: check_version
	@echo

${OBJECTCODE}/solver_test:: ${OBJECTCODE}/solver_test.o ${ADD_O} \
  ${OBJECTCODE}/libets.a ${OBJECTCODE}/libanalytics.a \
  ${OBJECTCODE}/libconvergence.a ${OBJECTCODE}/libbdseq.a \
  ${OBJECTCODE}/libitm.a ${LIBSCHEMAS}
	${FC} ${FCOPTS} -o $@ $^ ${perflib} ${LIBS}


################################################################################
# the program to compare the output from various equilibrium codes             #
################################################################################
eq_test: ${OBJECTCODE}/eq_test
	time ${OBJECTCODE}/eq_test ${ARGS}
	gnuplot GNUPLOT/eq_test.gnuplot
	@echo
	@echo
	@echo "Plots in eq_test.ps"
	@echo
	@echo

${OBJECTCODE}/eq_test:: check_version
	@echo

${OBJECTCODE}/eq_test:: ${OBJECTCODE}/eq_test.o   ${ADD_O} ${OBJECTCODE}/ets_version.o \
  ${OBJECTCODE}/libets.a ${OBJECTCODE}/libbdseq.a ${OBJECTCODE}/libhelena.a \
  ${OBJECTCODE}/libhelena2.a ${OBJECTCODE}/libemeq.a ${OBJECTCODE}/libequilibrium.a ${OBJECTCODE}/libspider.a \
  ${OBJECTCODE}/libitm.a ${LIBSCHEMAS}
	${FC} ${FCOPTS} -o $@ $^ ${perflib} ${LIBS} ${CHEASE_LIB}

################################################################################
# the program to compare the output from various neoclassical codes            #
################################################################################
neo_test: ${OBJECTCODE}/neo_test
	-rm sigma_par.dat neowes.dat neos.dat neo.dat itmneoart.dat
	time ${OBJECTCODE}/neo_test
	GNUPLOT/neoclassical

${OBJECTCODE}/neo_test:: ${OBJECTCODE}/neo_test.o ${OBJECTCODE}/libneo.a \
  ${OBJECTCODE}/libets.a ${OBJECTCODE}/libitm.a ${OBJECTCODE}/libneowes.a \
  ${LIBNEOART} ${OBJECTCODE}/libbdseq.a ${LIBSCHEMAS} 
	${FC} ${FCOPTS} -o $@ $^  ${LIBNEOS} ${perflib} ${LIBS}

################################################################################
# the main fortran workflow version of the ETS                                 #
################################################################################
ets_workflow: ${OBJECTCODE}/ets_workflow
	time ${OBJECTCODE}/ets_workflow
	@echo
	@echo
	@echo "Finished"
	@echo
	@echo

compile_ets_workflow: ${OBJECTCODE}/ets_workflow

${OBJECTCODE}/ets_workflow:: check_version
	@echo
	@echo ${FC} ${FCOPTS}

${OBJECTCODE}/ets_workflow::           ${OBJECTCODE}/ets_workflow.o ${ADD_O} \
        ${OBJECTCODE}/libets.a         ${OBJECTCODE}/libconvergence.a \
        ${OBJECTCODE}/libanalytics.a   ${OBJECTCODE}/libequilibrium.a \
        ${OBJECTCODE}/libemeq.a        ${OBJECTCODE}/libets_tools.a \
        ${OBJECTCODE}/libneowes.a      ${OBJECTCODE}/libetaigb.a \
        ${OBJECTCODE}/libgbtransp.a    ${OBJECTCODE}/libbdseq.a \
        ${OBJECTCODE}/libneutrals.a    ${OBJECTCODE}/libsources.a \
        ${OBJECTCODE}/libtransport.a   ${OBJECTCODE}/libdeltas.a \
        ${OBJECTCODE}/libhelena.a      ${LIBCHEASE} \
        ${OBJECTCODE}/libimpurity.a    ${OBJECTCODE}/libspider.a \
        ${OBJECTCODE}/libets.a         ${OBJECTCODE}/libitm.a \
        ${OBJECTCODE}/libdierckx.a     ${LIBAMNSPROTO} \
        ${LIBSCHEMAS}
	${FC} ${FCOPTS} -o $@ $+ ${perflib} ${LIBMATHEVAL} ${LIBS}



################################################################################
# FORCHECK: static check for fortran coding                                    #
################################################################################
SRC_FOR          = forcheck
ITM_LIBRARY_LIST = ual ualtools xmllib itmconstants itmtypes amns
FILTERED_LIST    = spider emeq etaigb  helena2 neowes bdseq gbtransport neo 

COMMA =,
SPACE :=
SPACE +=

FCK_INCS         = $(strip $(subst -I,,$(subst $(SPACE)-I,$(COMMA),$(subst $(SPACE)-I$(SPACE),$(COMMA),$(strip $(INCLUDE))))))
FCK_DEFS         = $(strip $(subst -D,,$(subst $(SPACE)-D,$(COMMA),$(subst $(SPACE)-D$(SPACE),$(COMMA),$(strip $(DEFINES))))))

VPATH_ETS        = $(filter-out $(wildcard $(addprefix src/, $(FILTERED_LIST))), $(wildcard $(VPATH)))
FILTERED_DIR     = $(filter-out $(wildcard $(VPATH_ETS)), $(wildcard $(VPATH)))


#ALL_SOURCES      = $(foreach dir, $(VPATH), $(wildcard $(dir)/*.[fF] $(dir)/*.[fF]90))
ALL_SOURCES      = $(foreach dir, $(VPATH_ETS), $(wildcard $(dir)/*.[fF] $(dir)/*.[fF]90))


UAL_PATH         = /afs/ipp/itm/switm/ual/4.10b.8_R2.1.0
UAL_DIR          = $(UAL_PATH)/fortraninterface
UALTOOLS_DIR     = $(UAL_PATH)/ualtools/src
#UALTOOLS_DIR     = ~olivh/public/ualtools-src
XMLLIB_DIR       = $(ITMLIBDIR)/xmllib/4.10b/src
ITMTYPES_DIR     = $(ITMLIBDIR)/itmtypes/src/f90
ITMCONSTANTS_DIR = $(ITMLIBDIR)/itmconstants/src/f90
AMNS_DIR         = $(ITM_AMNS_DIR)/src

SPIDER_DIR       = src/spider src/types
EMEQ_DIR         = src/emeq src/types
ETAIGB_DIR       = src/etaigb src/types
HELENA2_DIR      = src/helena2 src/types
NEOWES_DIR       = src/neowes src/types
BDSEQ_DIR        = src/bdseq src/types
GBTRANSPORT_DIR  = src/gbtransport src/types
NEO_DIR          = src/neo src/types


UALTOOL_FILES    = $(filter-out $(wildcard $(UALTOOLS_DIR)/diff_structures.[Ff]90),$(wildcard $(UALTOOLS_DIR)/*.[Ff]90))


ITM_FLBS         = $(patsubst %,$(SRC_FOR)/%.flb, $(ITM_LIBRARY_LIST))
FILTERED_FLBS    = $(patsubst %,$(SRC_FOR)/%.flb, $(FILTERED_LIST))



print_files:
	@echo 'VPATH:'
	@echo $(VPATH)
	@echo 'VPATH_ETS:'
	@echo $(VPATH_ETS)
	@echo 'ITM_FLBS:'
	@echo $(ITM_FLBS)
	@echo 'FILTERED_FLBS:'
	@echo $(FILTERED_FLBS)



check: $(ITM_FLBS) $(FILTERED_FLBS) $(ALL_SOURCES)
	forchk -l $(SRC_FOR)/ets.lst -define $(FCK_DEFS) \
        -I $(FCK_INCS) $(ALL_SOURCES) $(ITM_FLBS) $(FILTERED_FLBS)

$(SRC_FOR)/spider.flb: $(SRC_FOR)/itmtypes.flb $(SRC_FOR)/itmconstants.flb $(SRC_FOR)/ual.flb $(SRC_FOR)/xmllib.flb
	yes | forchk -l $(SRC_FOR)/spider.lst $(SPIDER_DIR)/*.[Ff]90 $(SPIDER_DIR)/*.[Ff]  -create $(SRC_FOR)/spider.flb $(SRC_FOR)/itmtypes.flb \
        $(SRC_FOR)/itmconstants.flb $(SRC_FOR)/ual.flb $(SRC_FOR)/xmllib.flb $(SRC_FOR)/ualtools.flb || true

$(SRC_FOR)/emeq.flb: $(SRC_FOR)/itmtypes.flb $(SRC_FOR)/itmconstants.flb $(SRC_FOR)/ual.flb $(SRC_FOR)/xmllib.flb
	yes | forchk -l $(SRC_FOR)/emeq.lst $(EMEQ_DIR)/*.[Ff]90 $(EMEQ_DIR)/*.[Ff]  -create $(SRC_FOR)/emeq.flb $(SRC_FOR)/itmtypes.flb \
        $(SRC_FOR)/itmconstants.flb $(SRC_FOR)/ual.flb $(SRC_FOR)/xmllib.flb $(SRC_FOR)/ualtools.flb || true

$(SRC_FOR)/etaigb.flb: $(SRC_FOR)/itmtypes.flb $(SRC_FOR)/itmconstants.flb $(SRC_FOR)/ual.flb $(SRC_FOR)/xmllib.flb
	yes | forchk -l $(SRC_FOR)/etaigb.lst $(ETAIGB_DIR)/*.[Ff]90 $(ETAIGB_DIR)/*.[Ff]  -create $(SRC_FOR)/etaigb.flb $(SRC_FOR)/itmtypes.flb \
        $(SRC_FOR)/itmconstants.flb $(SRC_FOR)/ual.flb $(SRC_FOR)/xmllib.flb $(SRC_FOR)/ualtools.flb || true

$(SRC_FOR)/helena2.flb: $(SRC_FOR)/itmtypes.flb $(SRC_FOR)/itmconstants.flb $(SRC_FOR)/ual.flb $(SRC_FOR)/xmllib.flb
	yes | forchk -l $(SRC_FOR)/helena2.lst $(HELENA2_DIR)/*.[Ff]90 $(HELENA2_DIR)/*.[Ff]  -create $(SRC_FOR)/helena2.flb $(SRC_FOR)/itmtypes.flb \
        $(SRC_FOR)/itmconstants.flb $(SRC_FOR)/ual.flb $(SRC_FOR)/xmllib.flb $(SRC_FOR)/ualtools.flb || true

$(SRC_FOR)/neowes.flb: $(SRC_FOR)/itmtypes.flb $(SRC_FOR)/itmconstants.flb $(SRC_FOR)/ual.flb $(SRC_FOR)/xmllib.flb
	yes | forchk -l $(SRC_FOR)/neowes.lst $(NEOWES_DIR)/*.[Ff]90 $(NEOWES_DIR)/*.[Ff]  -create $(SRC_FOR)/neowes.flb $(SRC_FOR)/itmtypes.flb \
        $(SRC_FOR)/itmconstants.flb $(SRC_FOR)/ual.flb $(SRC_FOR)/xmllib.flb $(SRC_FOR)/ualtools.flb || true

$(SRC_FOR)/bdseq.flb: $(SRC_FOR)/itmtypes.flb $(SRC_FOR)/itmconstants.flb $(SRC_FOR)/ual.flb $(SRC_FOR)/xmllib.flb
	yes | forchk -l $(SRC_FOR)/bdseq.lst $(BDSEQ_DIR)/*.[Ff]90 $(BDSEQ_DIR)/*.[Ff]  -create $(SRC_FOR)/bdseq.flb $(SRC_FOR)/itmtypes.flb \
        $(SRC_FOR)/itmconstants.flb $(SRC_FOR)/ual.flb $(SRC_FOR)/xmllib.flb $(SRC_FOR)/ualtools.flb || true

$(SRC_FOR)/gbtransport.flb: $(SRC_FOR)/itmtypes.flb $(SRC_FOR)/itmconstants.flb $(SRC_FOR)/ual.flb $(SRC_FOR)/xmllib.flb
	yes | forchk -l $(SRC_FOR)/gbtransport.lst $(GBTRANSPORT_DIR)/*.[Ff]90 $(GBTRANSPORT_DIR)/*.[Ff]  -create $(SRC_FOR)/gbtransport.flb $(SRC_FOR)/itmtypes.flb \
        $(SRC_FOR)/itmconstants.flb $(SRC_FOR)/ual.flb $(SRC_FOR)/xmllib.flb $(SRC_FOR)/ualtools.flb || true

$(SRC_FOR)/neo.flb: $(SRC_FOR)/itmtypes.flb $(SRC_FOR)/itmconstants.flb $(SRC_FOR)/ual.flb $(SRC_FOR)/xmllib.flb
	yes | forchk -l $(SRC_FOR)/neo.lst $(NEO_DIR)/*.[Ff]90 $(NEO_DIR)/*.[Ff]  -create $(SRC_FOR)/spider.flb $(SRC_FOR)/itmtypes.flb \
        $(SRC_FOR)/itmconstants.flb $(SRC_FOR)/ual.flb $(SRC_FOR)/xmllib.flb $(SRC_FOR)/ualtools.flb || true

$(SRC_FOR)/amns.flb: $(SRC_FOR)/itmtypes.flb $(SRC_FOR)/itmconstants.flb $(SRC_FOR)/ual.flb $(SRC_FOR)/xmllib.flb
	yes | forchk -l $(SRC_FOR)/amns.lst $(AMNS_DIR)/*.[Ff]90  -create $(SRC_FOR)/amns.flb $(SRC_FOR)/itmtypes.flb \
        $(SRC_FOR)/itmconstants.flb $(SRC_FOR)/ual.flb $(SRC_FOR)/xmllib.flb $(SRC_FOR)/ualtools.flb || true

$(SRC_FOR)/ual.flb: $(UAL_DIR)/*.f90 
	yes | forchk -l $(SRC_FOR)/ual.lst $(UAL_DIR)/*.f90 -create $(SRC_FOR)/ual.flb || true

$(SRC_FOR)/ualtools.flb: $(UALTOOLS_DIR)/*.f90
	yes | forchk -l $(SRC_FOR)/ualtools.lst $(UALTOOLS_FILES) -create $(SRC_FOR)/ualtools.flb $(SRC_FOR)/ual.flb || true

$(SRC_FOR)/xmllib.flb: $(XMLLIB_DIR)/*.f90 $(SRC_FOR)/ual.flb $(SRC_FOR)/itmtypes.flb
	yes | forchk -l $(SRC_FOR)/xmllib.lst $(XMLLIB_DIR)/*.f90 -create $(SRC_FOR)/xmllib.flb \
        $(SRC_FOR)/ual.flb $(SRC_FOR)/itmtypes.flb || true

$(SRC_FOR)/itmtypes.flb: $(ITMTYPES_DIR)/*.f90
	yes | forchk -l $(SRC_FOR)/itmtypes.lst $(ITMTYPES_DIR)/*.f90 -create $(SRC_FOR)/itmtypes.flb || true

$(SRC_FOR)/itmconstants.flb:  $(ITMCONSTANTS_DIR)/*.f90
	yes | forchk -l $(SRC_FOR)/itmconstants.lst $(ITMCONSTANTS_DIR)/*.f90 -create $(SRC_FOR)/itmconstants.flb $(SRC_FOR)/itmtypes.flb || true




################################################################################
# the program for preparing the input CPOs for the ETS                         #
################################################################################
prepare_input_cpos: ${OBJECTCODE}/prepare_input_cpos
	time ${OBJECTCODE}/prepare_input_cpos ${ARGS}
	@echo
	@echo
	@echo "Finished"
	@echo
	@echo

${OBJECTCODE}/prepare_input_cpos:: check_version
	@echo

${OBJECTCODE}/prepare_input_cpos:: ${OBJECTCODE}/prepare_input_cpos.o \
  ${WRITE_CPO_O} ${OBJECTCODE}/libitm.a ${OBJECTCODE}/libets.a ${OBJECTCODE}/libets_tools.a \
  ${OBJECTCODE}/libbdseq.a ${LIBSCHEMAS} 
	${FC} ${FCOPTS} -o $@ $^ ${perflib} ${LIBS} ${LIBMATHEVAL}

################################################################################
# copy_signals ?????????????????????????????????????????                       #
################################################################################
copy_signals: ${OBJECTCODE}/copy_signals
	time ${OBJECTCODE}/copy_signals
	@echo
	@echo
	@echo "Finished"
	@echo
	@echo

${OBJECTCODE}/copy_signals:: check_version
	@echo

${OBJECTCODE}/copy_signals:: ${OBJECTCODE}/copy_signals.o ${COPYSIGNALS_O} \
  ${OBJECTCODE}/libets.a ${OBJECTCODE}/libitm.a  ${LIBSCHEMAS} 
	${FC} ${FCOPTS} -o $@ $^ ${perflib} ${LIBS}

################################################################################
# Print out the size of CPOs                                                   #
################################################################################
sizecpo: ${OBJECTCODE}/sizecpo
	time ${OBJECTCODE}/sizecpo ${ARGS}

${OBJECTCODE}/sizecpo: ${OBJECTCODE}/sizecpo.o ${OBJECTCODE}/libitm.a \
  ${LIBSCHEMAS}
	${FC} ${FCOPTS} -o $@ $^ ${perflib} ${LIBS}

################################################################################
# Print out the times of CPOs                                                  #
################################################################################
timescpo: ${OBJECTCODE}/timescpo
	time ${OBJECTCODE}/timescpo ${ARGS}

${OBJECTCODE}/timescpo: ${OBJECTCODE}/timescpo.o ${OBJECTCODE}/libitm.a \
  ${LIBSCHEMAS}
	${FC} ${FCOPTS} -o $@ $^ ${perflib} ${LIBS}

################################################################################
# Print out the CPOs                                                           #
################################################################################
writecpo: ${OBJECTCODE}/writecpo
	time ${OBJECTCODE}/writecpo ${ARGS}

${OBJECTCODE}/writecpo: ${OBJECTCODE}/writecpo.o ${OBJECTCODE}/libitm.a \
  ${LIBSCHEMAS}
	${FC} ${FCOPTS} -o $@ $^ ${perflib} ${LIBS}

################################################################################
# Print out CPOs                                                               #
################################################################################
printcpo: ${OBJECTCODE}/printcpo
	time ${OBJECTCODE}/printcpo ${ARGS}

${OBJECTCODE}/printcpo: ${OBJECTCODE}/printcpo.o ${OBJECTCODE}/libitm.a \
  ${LIBSCHEMAS}
	${FC} ${FCOPTS} -o $@ $^ ${perflib} ${LIBS}

################################################################################
# Test the equilibrium_augmenter routines                                      #
################################################################################
test_equilibrium_augmenter: ${OBJECTCODE}/test_equilibrium_augmenter
	time ${OBJECTCODE}/test_equilibrium_augmenter

${OBJECTCODE}/test_equilibrium_augmenter:: check_version
	@echo

${OBJECTCODE}/test_equilibrium_augmenter:: \
  ${OBJECTCODE}/test_equilibrium_augmenter.o \
  ${OBJECTCODE}/libitm.a ${OBJECTCODE}/libdierckx.a ${LIBSCHEMAS}
	${FC} ${FCOPTS} -o $@ $^ ${perflib} ${LIBS}

################################################################################
# run Helena standalone                                                        #
################################################################################
run_helena: ${OBJECTCODE}/run_helena
	time ${OBJECTCODE}/run_helena ${ARGS}

${OBJECTCODE}/run_helena:: ${OBJECTCODE}/run_helena.o \
  ${OBJECTCODE}/libhelena.a ${OBJECTCODE}/libitm.a ${LIBSCHEMAS}
	${FC} ${FCOPTS} -o $@ $^ ${perflib} ${LIBS}

################################################################################
# run Helena standalone                                                        #
################################################################################
run_helena_cpo: ${OBJECTCODE}/run_helena_cpo
	time ${OBJECTCODE}/run_helena_cpo ${ARGS}

${OBJECTCODE}/run_helena_cpo:: ${OBJECTCODE}/run_helena_cpo.o \
  ${OBJECTCODE}/libhelena.a ${OBJECTCODE}/libitm.a ${LIBSCHEMAS}
	${FC} ${FCOPTS} -o $@ $^ ${perflib} ${LIBS}

################################################################################
# run Chease standalone                                                        #
################################################################################
run_chease: ${OBJECTCODE}/run_chease
	time ${OBJECTCODE}/run_chease ${ARGS}

${OBJECTCODE}/run_chease:: ${OBJECTCODE}/run_chease.o \
  ${LIBCHEASE} ${OBJECTCODE}/libitm.a ${LIBSCHEMAS}
	${FC} ${FCOPTS} -o $@ $^ ${perflib} ${LIBS}

################################################################################
# run ascii_equilibrium_to_ual                                                 #
################################################################################
ascii_equilibrium_to_ual: ${OBJECTCODE}/ascii_equilibrium_to_ual
	time ${OBJECTCODE}/ascii_equilibrium_to_ual ${ARGS}

${OBJECTCODE}/ascii_equilibrium_to_ual:: ${OBJECTCODE}/ascii_equilibrium_to_ual.o \
  ${OBJECTCODE}/libitm.a ${LIBSCHEMAS}
	${FC} ${FCOPTS} -o $@ $^ ${perflib} ${LIBS}

################################################################################
# run dump_amns                                                                #
################################################################################
dump_amns: ${OBJECTCODE}/dump_amns
	time ${OBJECTCODE}/dump_amns ${ARGS}

${OBJECTCODE}/dump_amns:: ${OBJECTCODE}/dump_amns.o \
  ${OBJECTCODE}/libitm.a ${LIBSCHEMAS}
	${FC} ${FCOPTS} -o $@ $^ ${perflib} ${LIBS}

################################################################################
# test the libmatheval functions                                               #
################################################################################
test_functions: ${OBJECTCODE}/test_functions
	@${OBJECTCODE}/test_functions ${ARGS}

${OBJECTCODE}/test_functions: ${OBJECTCODE}/test_functions.o \
  ${OBJECTCODE}/libitm.a ${OBJECTCODE}/libets.a ${OBJECTCODE}/libbdseq.a ${LIBSCHEMAS}
	${FC} ${FCOPTS} -o $@ $^ ${LIBMATHEVAL} ${LIBS}

################################################################################
# run the toric test example                                                   #
################################################################################
ifeq ($(TORIC),yes) 

VPATH += src/toric
INCLUDE += -I /afs/efda-itm.eu/imp5/user/bilato/Codes/toric6.0/base/Linux/Ser
DEFINES += -DLNTCC
TORIC = /afs/efda-itm.eu/user/b/bilato/usr/x86_64_rhel5/pgf90

run_toric_example: ${OBJECTCODE}/run_toric_example
	@${OBJECTCODE}/run_toric_example ${ARGS}

${OBJECTCODE}/run_toric_example: ${OBJECTCODE}/run_toric_example.o \
  /afs/efda-itm.eu/user/b/bilato/Codes/toric6.0/base/Linux/Ser/libtoric.a \
  ${OBJECTCODE}/libitm.a ${OBJECTCODE}/libets.a ${OBJECTCODE}/libbdseq.a ${LIBSCHEMAS} \
  ${TORIC}/fftw-2.1.5/lib/libfftw.a \
  ${TORIC}/ntcc/i2mex/lib/libi2mex.a \
  ${TORIC}/ntcc/i2mex/lib/libxplasma.a \
  ${TORIC}/ntcc/i2mex/lib/libnscrunch.a \
  ${TORIC}/ntcc/i2mex/lib/libpspline.a \
  ${TORIC}/ntcc/i2mex/lib/libesc.a \
  ${TORIC}/ntcc/i2mex/lib/libmclib.a \
  ${TORIC}/ntcc/i2mex/lib/libfluxav.a \
  ${TORIC}/ntcc/i2mex/lib/libsmlib.a \
  ${TORIC}/ntcc/i2mex/lib/libcomput.a \
  ${TORIC}/ntcc/i2mex/lib/libportlib.a \
  ${TORIC}/ntcc/i2mex/lib/libmdstransp.a \
  ${TORIC}/ntcc/i2mex/lib/libvaxonly.a \
  ${TORIC}/ntcc/i2mex/lib/libmds_dummy.a \
  ${TORIC}/ntcc/i2mex/lib/libgeneric_dummy.a \
  ${TORIC}/lib/libodepack.a \
  ${TORIC}/lib/libfmt.a \
  ${TORIC}/ntcc/i2mex/lib/libezcdf.a \
  ${TORIC}/ntcc/i2mex/lib/liblsode.a \
  ${TORIC}/netcdf-3.5.1/lib/libnetcdf.a
	${FC} ${FCOPTS}  -o $@ $^ ${LIBS}

endif

################################################################################
# building the libraries                                                       #
################################################################################
${OBJECTCODE}/libitm.a: ${LIB_ITM_O}
	ar r $@ $?
	${RANLIB} $@

${OBJECTCODE}/libschemas.a: ${LIB_SCHEMAS_O}
	ar r $@ $?
	${RANLIB} $@

${OBJECTCODE}/libets.a: ${LIB_ETS_O}
	ar r $@ $?
	${RANLIB} $@

${OBJECTCODE}/libanalytics.a: ${LIB_ANALYTICS_O}
	ar r $@ $?
	${RANLIB} $@

${OBJECTCODE}/libconvergence.a: ${LIB_CONVERGENCE_O}
	ar r $@ $?
	${RANLIB} $@

${OBJECTCODE}/libbdseq.a: ${LIB_BDSEQ_O}
	ar r $@ $?
	${RANLIB} $@

${OBJECTCODE}/libhelena.a: ${LIB_HELENA_O}
	ar r $@ $?
	${RANLIB} $@

${OBJECTCODE}/libhelena2.a: ${LIB_HELENA2_O}
	ar r $@ $?
	${RANLIB} $@

${OBJECTCODE}/libneo.a: ${LIB_NEO_O}
	ar r $@ $?
	${RANLIB} $@

${OBJECTCODE}/libemeq.a: ${LIB_EMEQ_O}
	ar r $@ $?
	${RANLIB} $@

${OBJECTCODE}/libequilibrium.a: ${LIB_EQUILIBRIUM_O}
	ar r $@ $?
	${RANLIB} $@

${OBJECTCODE}/libetaigb.a: ${LIB_ETAIGB_O}
	ar r $@ $?
	${RANLIB} $@

${OBJECTCODE}/libgbtransp.a: ${LIB_GBTRANSP_O}
	ar r $@ $?
	${RANLIB} $@

${OBJECTCODE}/libneowes.a: ${LIB_NEOWES_O}
	ar r $@ $?
	${RANLIB} $@

${OBJECTCODE}/libneutrals.a: ${LIB_NEUTRALS_O}
	ar r $@ $?
	${RANLIB} $@

${OBJECTCODE}/libsources.a: ${LIB_SOURCES_O}
	ar r $@ $?
	${RANLIB} $@

${OBJECTCODE}/libdeltas.a: ${LIB_DELTAS_O}
	ar r $@ $?
	${RANLIB} $@

${OBJECTCODE}/libtransport.a: ${LIB_TRANSPORT_O}
	ar r $@ $?
	${RANLIB} $@

${OBJECTCODE}/libets_tools.a: ${LIB_ETS_TOOLS_O}
	ar r $@ $?
	${RANLIB} $@

${OBJECTCODE}/libdierckx.a: ${LIB_DIERCKX_O}
	ar r $@ $?
	${RANLIB} $@

${OBJECTCODE}/libperfdummy.a: ${LIB_PERF_O}
	ar r $@ $?
	${RANLIB} $@

${OBJECTCODE}/libimpurity.a: ${LIB_IMPURITY_O}
	ar r $@ $?
	ranlib $@

${OBJECTCODE}/libspider.a: ${LIB_SPIDER_O}
	ar r $@ $?
	ranlib $@

${OBJECTCODE}/libamnsproto.a: ${LIB_AMNSPROTO_O}
	ar r $@ $?
	${RANLIB} $@

${OBJECTCODE}/libneoart.a: ${LIB_NEOART_O}
	ar r $@ $?
	${RANLIB} $@

################################################################################
# used for debugging: print out some of the Makefile variables                 #
################################################################################
echo:
	@echo VPATH = ${VPATH}
	@echo DEPPATH = ${DEPPATH}
	@echo DEFINES = ${DEFINES}
	@echo ${OBJECTCODE}/solver_test.o ${ADD_O} ${LIBSCHEMAS} \
              ${OBJECTCODE}/libets.a 
	@echo ${LIBSCHEMAS} ${LIB_SCHEMAS_O}
	@echo ETS_O = ${ETS_O}
	@echo NEUTRALS_O = ${NEUTRALS_O}
	@echo SOLVER_O = ${SOLVER_O}
	@echo BDSEQ_O = ${BDSEQ_O}
	@echo HELENA_O = ${HELENA_O}
	@echo HELENA2_O = ${HELENA2_O}
	@echo NEO_O = ${NEO_O}
	@echo EMEQ_O = ${EMEQ_O}
	@echo EQUILIBRIUM_O = ${EQUILIBRIUM_O}
	@echo LIB_ITM_O = ${LIB_ITM_O}
	@echo ETAIGB_O = ${ETAIGB_O}
	@echo GBTRANSP_O = ${GBTRANSP_O}
	@echo NEOWES_O = ${NEOWES_O}
	@echo NEUTRALS_O = ${NEUTRALS_O}
	@echo ETS_TOOLS_O = ${ETS_TOOLS_O}
	@echo IMPURITY_O = ${IMPURITY_O}
	@echo SPIDER = ${SPIDER_O}
	@echo X${NUMPS_BASE}X
	@echo UAL_O = ${UAL_O}
	@echo LIB_AMNSPROTO_O = ${LIB_AMNSPROTO_O}
	@echo LIB_HELENA_O = ${LIB_HELENA_O}
	@echo LIB_ITM_O = ${LIB_ITM_O} 
	@echo ${OBJECTCODE}/dependencies

################################################################################
# clean up                                                                     #
################################################################################
clean:
	-rm ${OBJECTCODE}/*.o ${OBJECTCODE}/*.${MOD} ${OBJECTCODE}/*.f90 \
            ${OBJECTCODE}/solver_test ${OBJECTCODE}/eq_test \
            ${OBJECTCODE}/ets_workflow ${OBJECTCODE}/prepare_input_cpos \
            ${OBJECTCODE}/printcpo ${OBJECTCODE}/sizecpo \
            ${OBJECTCODE}/test_equilibrium_augmenter ${OBJECTCODE}/run_helena \
            ${OBJECTCODE}/libanalytics.a ${OBJECTCODE}/libconvergence.a \
            ${OBJECTCODE}/libitm.a ${LIBSCHEMAS} ${OBJECTCODE}/libets.a \
            ${OBJECTCODE}/libneo.a ${OBJECTCODE}/libhelena2.a \
            ${OBJECTCODE}/libbdseq.a ${OBJECTCODE}/libhelena.a ${OBJECTCODE}/libemeq.a \
            ${OBJECTCODE}/libequilibrium.a ${OBJECTCODE}/libets_tools.a \
            ${OBJECTCODE}/libneutrals.a ${OBJECTCODE}/libsources.a \
            ${OBJECTCODE}/libtransport.a ${OBJECTCODE}/libdeltas.a \
            ${OBJECTCODE}/libetaigb.a ${OBJECTCODE}/libgbtransp.a \
            ${OBJECTCODE}/libneowes.a  ${OBJECTCODE}/libperfdummy.a \
            ${OBJECTCODE}/libimpurity.a ${OBJECTCODE}/libspider.a \
            ${OBJECTCODE}/timescpo ${OBJECTCODE}/dump_amns \
            ${OBJECTCODE}/writecpo ${OBJECTCODE}/neo_test ${OBJECTCODE}/run_helena_cpo \
            ${OBJECTCODE}/libdierckx.a ${OBJECTCODE}/dependencies*

################################################################################
# clean up the documentation area                                              #
################################################################################
docclean: 
	rm -rf doc/Doxygen/html doc/Doxygen/latex doc/Doxygen/man \
               doc/Doxygen/rtf doc/Doxygen/xml

################################################################################
# create the dependency information                                            #
################################################################################
DEPPATH =  ${filter-out  %/*.[fF] %/*.[fF]90, ${shell echo ${patsubst %,%/*.[fF]90,${VPATH}} ${patsubst %,%/*.[fF],${VPATH}}}}

depend: 
	obj/sfmakedepend -p '$${OBJECTCODE}/' \
	    `echo ${INCLUDE} | sed -e 's:-I:-I :g'` \
	    -f ${OBJECTCODE}/dependencies ${DEPPATH}
	sed -i~ -e '/\/mpes\./d' ${OBJECTCODE}/dependencies
	sed -i~ -e '/ieee_/d' ${OBJECTCODE}/dependencies
ifeq ($(UAL),yes)
	sed -i~ -e '/euitm_schemas/d' ${OBJECTCODE}/dependencies
	sed -i~ -e '/euitm_routines/d' ${OBJECTCODE}/dependencies
endif
ifeq ($(AMNS),yes)
	sed -i~ -e '/amns_module.mod/d' ${OBJECTCODE}/dependencies
	sed -i~ -e '/amns_types.mod/d' ${OBJECTCODE}/dependencies
endif
ifeq ($(CJK),yes)
	sed -i~ -e '/_structures.mod/d' ${OBJECTCODE}/dependencies
endif
ifeq ($(ITMTYPES),yes)
	sed -i~ -e '/itm_types/d' ${OBJECTCODE}/dependencies
endif
ifeq ($(ITMCONSTANTS),yes)
	sed -i~ -e '/itm_constants/d' ${OBJECTCODE}/dependencies
endif
ifeq ($(XMLLIB),yes)
	sed -i~ -e '/euitm_xml_parser/d' -e '/string_manipulation_tools/d' -e '/xml_tools/d' -e '/xml_file_reader/d' ${OBJECTCODE}/dependencies
endif
ifneq ($(shell [ -d src/emeq ] && echo yes || echo no ),yes)
	sed -i~ -e '/emeq/d' ${OBJECTCODE}/dependencies
endif
ifneq ($(shell [ -d src/equilibrium ] && echo yes || echo no ),yes)
	sed -i~ -e '/equilibrium/d' ${OBJECTCODE}/dependencies
endif
ifneq ($(shell [ -d src/etaigb ] && echo yes || echo no ),yes)
	sed -i~ -e '/etaigb/d' ${OBJECTCODE}/dependencies
endif
ifneq ($(shell [ -d src/gbtransport ] && echo yes || echo no ),yes)
	sed -i~ -e '/gbtransport/d' ${OBJECTCODE}/dependencies
endif
ifneq ($(shell [ -d src/neowes ] && echo yes || echo no ),yes)
	sed -i~ -e '/neowes/d' ${OBJECTCODE}/dependencies
endif
ifneq ($(shell [ -d src/neutrals ] && echo yes || echo no ),yes)
	sed -i~ -e '/neutrals/d' ${OBJECTCODE}/dependencies
endif
ifneq ($(shell [ -d src/impurity ] && echo yes || echo no ),yes)
	sed -i~ -e '/impurity/d' ${OBJECTCODE}/dependencies
endif
ifneq ($(shell [ -d src/impurity ] && echo yes || echo no ),yes)
	sed -i~ -e '/transport/d' ${OBJECTCODE}/dependencies
endif
ifneq ($(shell [ -d src/impurity ] && echo yes || echo no ),yes)
	sed -i~ -e '/sources/d' ${OBJECTCODE}/dependencies
endif
ifneq ($(shell [ -d src/impurity ] && echo yes || echo no ),yes)
	sed -i~ -e '/deltas/d' ${OBJECTCODE}/dependencies
endif
ifneq ($(shell [ -d src/spider ] && echo yes || echo no ),yes)
	sed -i~ -e '/spiedr/d' ${OBJECTCODE}/dependencies \
	    src/spider/*.[fF] src/spider/*.[fF]90 
endif
ifneq ($(shell [ -d src/amnsprotolib ] && echo yes || echo no ),yes)
	sed -i~ -e '/amns/d' ${OBJECTCODE}/dependencies 
endif
	sed -i~ -e '/t6_mod_toric.mod/d' ${OBJECTCODE}/dependencies
	sed -i~ -e '/iso_c_binding/d' ${OBJECTCODE}/dependencies
	sed -i~ -e '/mpi.o/d' ${OBJECTCODE}/dependencies

newdepend: 
	~/bin/makedepf90 -b '$${OBJECTCODE}/' ${DEFINES} \
	    src/*/*.[fF] src/*/*.[fF]90 > ${OBJECTCODE}/dependencies


################################################################################
# create the tags used by emacs                                                #
################################################################################
tags:
	rm TAGS ; etags src/*/*.[fFc] src/*/*.[fF]90

################################################################################
# create the dependencies if they don't exist                                  #
################################################################################
${OBJECTCODE}/dependencies:
	mkdir -p ${OBJECTCODE}
	touch ${OBJECTCODE}/dependencies
	${MAKE} depend

################################################################################
# create the Doxygen documentation                                             #
################################################################################
DOC:
	${DOXYGEN} doc/Doxygen/Doxyfile
	cd doc/Doxygen/latex/ && export pool_size=10000000 max_strings=200000 \
	save_size=50000 hash_extra=100000 extra_mem_top=1500000 extra_mem_bot=1500000 \
	pdf_mem_size=500000 obj_tab_size=600000 main_memory=5000000 && ${MAKE}
	rsync -avP doc/Doxygen/latex/refman.pdf doc/Documentation/ETS_Doxygen.pdf

DOC_Update: DOC
	rsync -avP doc/Doxygen/html/. /afs/efda-itm.eu/project/web2/var.www/html/ITM/doxygen/imp3/ets/

################################################################################
# write out version information                                                #
################################################################################
version:
	@echo
	@echo Version of the project = \"`svnversion . repos/ETS/trunk/ETS`\"
	@echo SVN version of the Makefile = \"'$$Id: Makefile 30 2008-08-27 04:32:33Z dpc $$'\"
	@echo

################################################################################
# write out a short form of the version information                            #
################################################################################
status:
	@echo `date +%Y-%m-%d` ${SYS} `svnversion . svn/ets/trunk/ETS` `hostname` `uname -r -p` 

################################################################################
# echo the SYS variable                                                        #
################################################################################
SYS:
	@echo ${SYS}


################################################################################
# bring in the dependencies                                                    #
################################################################################
include ${OBJECTCODE}/dependencies

################################################################################
# entries for creating the version information used in the ETS                 #
################################################################################
current_ets_version = ${shell svnversion src}
labelled_ets_version = \
  ${shell [ -e src/version/ets_version.f90 ] && \
  grep 'version =' src/version/ets_version.f90 | \
  tr "'" " " | \
  awk '{print $$7}'}

.PHONY: check_version

src/version/ets_version.f90 check_version:
	@ echo ${current_ets_version} ${labelled_ets_version}
	@[ "${current_ets_version}" != "${labelled_ets_version}" ] && \
	  rm -f src/version/ets_version.f90 && \
	  ${MAKE} update_version  && \
	  echo Updated src/version/ets_version.f90 || \
	  echo src/version/ets_version.f90 was correct

update_version:
	echo "module ets_version"                                               >  src/version/ets_version.f90
	echo "  implicit none"                                                  >> src/version/ets_version.f90
	echo "  character (len=12), parameter :: version = '`svnversion src`'"  >> src/version/ets_version.f90
	echo "end module ets_version"                                           >> src/version/ets_version.f90


################################################################################
# a set of patches to bdseq so that it works with the ETS                      #
################################################################################
patch_bdseq:
	patch -p0 < src/patches/bdseq

################################################################################
# amns stuff                                                                   #
################################################################################

run_amns_demo: ${OBJECTCODE}/amns_demo
	@echo
	@echo 'Running' $^
	@echo
	@time $^
	@echo
	@echo 'Comparison with the reference (should show no differences!):'
	@echo
	@-diff -sqx .svn out.REF out
	@echo
	@echo 'Produce the graphics comparison'
	@echo
	./test_amns.gnuplot
	@echo
	@echo 'Output should be in "test_amns.ps"'

run_amns_test: ${OBJECTCODE}/test_amns
	@echo
	@echo 'Running' $^
	@echo
	@time $^
	@echo
	@echo 'Produce the graphics comparison'
	@echo
	./test_demo.gnuplot
	@echo
	@echo 'Output should be in "test_demo.ps"'

write_amns: ${OBJECTCODE}/write_amns
	@echo
	@echo 'Running' $^
	@echo
	@time $^
	@echo

read_amns: ${OBJECTCODE}/read_amns
	@echo
	@echo 'Running' $^
	@echo
	@time $^
	@echo

${OBJECTCODE}/amns_demo : ${OBJECTCODE}/amns_demo.o ${LIBSCHEMAS} # ${OBJECTCODE}/libamnsproto.a
	${FC} ${FCOPTS} -o $@ $^ ${LIBS}

${OBJECTCODE}/test_amns : ${OBJECTCODE}/test_amns.o ${OBJECTCODE}/libamnsproto.a
	${FC} ${FCOPTS} -o $@ $^

${OBJECTCODE}/write_amns: src/write_amns.f90
	${FC} ${FCOPTS} ${UAL_inc} $< -o $@ ${UAL_lib}

${OBJECTCODE}/read_amns: src/read_amns.f90
	${FC} ${FCOPTS} ${UAL_inc} $< -o $@ ${UAL_lib}



################################################################################
# help on the Makefile                                                         #
################################################################################
help:
	@echo 'Welcome to the ETS Makefile'
	@echo ''
	@echo 'Common entry points'
	@echo '  run    (default)     compile and run solver_test (ets test framework using an analytical model)'
	@echo '  eq_test              compile and run eq_test (equilibrium test framework)'
	@echo '  compile_ets_workflow compile Fortran based ETS WORKFLOW'
	@echo '  ets_workflow         compile and run Fortran based ETS WORKFLOW'
	@echo '  prepare_input_cpos   prepare input CPOs for eq_ets_test'
	@echo '  run_helena           ARGS=ascii_eq_CPO run helena on an equilibrium from a ASCII CPO'
	@echo '  run_helena_cpo       ARGS=(SHOT RUN_IN RUN_OUT TIME) run helena on an equilibrium from the database'
	@echo '  sizecpo              ARGS=(SHOT RUN)  size of cpos'
	@echo '  printcpo             ARGS=(SHOT RUN)  print cpos'
	@echo '  timescpo             ARGS=(SHOT RUN cpos) print times for cpos'
	@echo '  writecpo             ARGS=(SHOT RUN cpos) print cpos'
	@echo '  depend               refresh the dependency information'
	@echo '  clean                remove various binaries (.o, .mod, .a and executables)'
	@echo '  DOC                  create the Doxygen documentation (in doc/Doxygen/{html,latex,man,rtf,xml})'
	@echo '  docclean             clean the Doxygen documentation area'
	@echo '  tags                 create the TAGS file for emacs'
	@echo '  version              write out the current version based on SVN'
	@echo '  status               writes out date, version, hostname and uname'
	@echo '  SYS                  writes out the SYS variable name'
	@echo '  echo                 output the contents of some Makefile variables (used for debugging the Makefile)'
	@echo '  help                 writes out this information'
	@echo ''
