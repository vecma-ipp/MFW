#/* -*- makefile -*- */

export SYS ?= DEFAULT

ifneq (${MAINMAKE},yes)
 export INC_BDS = ../externals/libbds/include/${SYS}
 export LIB_BDS = ../externals/libbds/lib/${SYS}
 export INC_UAL = ../ual/include/${SYS}/fortran_interface
 export LIB_UAL = ../ual/lib/${SYS}/fortran_interface
 export INC_TYPES = ../ual/include/${SYS}/types
 export LIB_TYPES = ../ual/lib/${SYS}/types
 export INC_CONSTANTS = ../ual/include/${SYS}/constants
 export LIB_CONSTANTS = ../ual/lib/${SYS}/constants
 export INC_XML = ../ual/include/${SYS}/xml
 export LIB_XML = ../ual/lib/${SYS}/xml

 export DATAVERSION = 4.10b.10

 include ../config
endif


#.SUFFIXES:
#.SUFFIXES: obj/.o .f90 .F90

SRCDIR = src
OBJDIR = obj/${SYS}
INCDIR = include/${SYS}
BINDIR = bin/${SYS}


# paths ########################################################################

BDSEQ = ../externals/bdseq/${SYS}
BOHMGB = ../externals/bohmgb/${SYS}
ETS = ../externals/ets/obj/${SYS}
GEM = ../externals/gem/${SYS}
CHEASE = ../externals/chease/src-f90
DFEFI = ../externals/dfefi/${SYS}
IMP4DV = ../externals/imp4dv/${SYS}
GEM0 = ../externals/gem0/${SYS}
ORB5 = ../externals/orb5.git/${SYS}

# set up the VPATH for searching for source files ##############################
VPATH = ${SRCDIR} ${SRCDIR}/tools ${SRCDIR}/tests


# additional flags #############################################################
INCLUDES = -I${INCDIR} -I${INC_UAL} 
#-I${ETS} -I${INC_TYPES} -I${INC_CONSTANTS} -I${INC_BDS}

#--------------------------------------------------------------------#
# use MPI compiler for all Fortran kernel sources (default=yes)      #
ifneq (${MPI},no) 
 FC90 = ${MPIF90}
 LD = ${MPIF90}
 FPPFLAGS += -DMPI -DMPI2
# LDFLAGS += ${FFTW_LIB}
#-L${FFTWHOME}/lib 
endif

#--------------------------------------------------------------------#
# adds time monitoring functions in kernels/wrapper (default=no)     #
ifeq (${PERF},yes)
 FPPFLAGS += -DPERF
endif

#--------------------------------------------------------------------#
# compile kernels dedicated to UAL library (default=if $UAL is set)  #
ifdef UAL
 FPPFLAGS += -DHAS_UAL
 INCLUDES += $(shell pkg-config --cflags ual-${LIBARCH})
 LDUAL = $(shell pkg-config --libs ual-${LIBARCH})
endif

#--------------------------------------------------------------------#
# compile with additional debugging code (default=no)                #
ifeq (${DEBUG},yes)
 FPPFLAGS += -DDEBUG
endif
ifeq (${SAVECORET},yes)
 FPPFLAGS += -DSAVECORET
endif

#--------------------------------------------------------------------#
# add code to limit D&V's evolution (default=yes)                    #
#ifneq (${DVLIMIT},no)
# FPPFLAGS += -DDLIMIT -DVLIMIT 
#endif

#--------------------------------------------------------------------#
# add code to interpolate transport on profile rho_tor (default=yes) #
# NOTE: this might not be optional, should probably be removed...    #
ifneq (${INTERP},no)
 FPPFLAGS += -DINTERP_TRANSP 
endif

#--------------------------------------------------------------------#
# keeps initial transport for following iterations (default=no)      #
ifeq (${TRANSP_EVO},no)
 FPPFLAGS += -DCONST_TRANSP
endif

#--------------------------------------------------------------------#
# add code for transport sigma calculated by SPITZER (default=yes)   #
ifneq (${SPITZER},no)
 FPPFLAGS += -DSIGMA_SPITZER
endif

#--------------------------------------------------------------------#
# compile test program for ETS wrapper (default=no)                  #
ifeq (${TEST},yes)
 FPPFLAGS += -DETS_TEST
endif




#LDFLAGS += -Llib
LDFLAGS += -L${MUSCLE_HOME}/lib


FC90FLAGS += ${INCLUDES} ${MODSPEC}${INCDIR} 
#CCFLAGS += -I${JAVAHOME}/include/ -I${JAVAHOME}/include/linux/ 



# objects files ################################################################
SHARED_O = 	${OBJDIR}/string_binding.o ${OBJDIR}/muscle_fortran.o         \
		${OBJDIR}/c_tools.o ${OBJDIR}/filetools.o ${OBJDIR}/linterp.o \
		${OBJDIR}/c_perf.o ${OBJDIR}/timer.o


INIT_CPO_O = ${SHARED_O} ${OBJDIR}/init_cpo_wrapper.o ${OBJDIR}/init_cpo_kernelB.o
CONTINUE_O = ${SHARED_O} ${OBJDIR}/continue_wrapper.o ${OBJDIR}/continue_kernelB.o
BDSEQ_O = ${SHARED_O} ${OBJDIR}/bdseq_wrapper.o ${OBJDIR}/bdseq_kernelB.o
ETS_O = ${SHARED_O} ${OBJDIR}/ets_wrapper.o ${OBJDIR}/ets_kernelB.o 
BOHMGB_O = ${SHARED_O} ${OBJDIR}/bohmgb_wrapper.o ${OBJDIR}/bohmgb_kernelB.o
GEM_O = ${SHARED_O} ${OBJDIR}/gem_wrapper.o ${OBJDIR}/gem_kernelB.o
CHEASE_O = ${SHARED_O} ${OBJDIR}/chease_wrapper.o ${OBJDIR}/chease_kernelB.o
DFEFI_O = ${SHARED_O} ${OBJDIR}/dfefi_wrapper.o ${OBJDIR}/dfefi_kernelB.o
IMP4DV_O = ${SHARED_O} ${OBJDIR}/imp4dv_wrapper.o ${OBJDIR}/imp4dv_kernelB.o
GEM0_O = ${SHARED_O} ${OBJDIR}/gem0_wrapper.o ${OBJDIR}/gem0_kernelB.o
#NOTRANSP_O = ${SHARED_O} ${OBJDIR}/notransp_wrapper.o ${OBJDIR}/notransp_kernelB.o
BYPASS_O = ${SHARED_O} ${OBJDIR}/bypass_kernelB.o
DV2FLUX_O = ${SHARED_O} ${OBJDIR}/dv2flux_wrapper.o ${OBJDIR}/dv2flux_kernelB.o
ORB5_O = ${SHARED_O} ${OBJDIR}/orb5_wrapper.o ${OBJDIR}/orb5_kernelB.o
ifdef UAL
  INIT_UAL_O = ${SHARED_O} ${OBJDIR}/init_ual_wrapper.o ${OBJDIR}/init_ual_kernelB.o
  WRITE_UAL_O = ${SHARED_O} ${OBJDIR}/write_ual_wrapper.o ${OBJDIR}/write_ual_kernelB.o
endif

KERNELS = INIT_CPO_K CONTINUE_K BDSEQ_K BOHMGB_K ETS_K CHEASE_K GEM_K \
	DFEFI_K IMP4DV_K GEM0_K DV2FLUX_K BYPASS_K 
#ORB5_K
#NOTRANSP_K 
#ifeq (${TEST},yes)
#  KERNELS = TEST_K
#endif
ifdef UAL
  KERNELS += INIT_UAL_K WRITE_UAL_K
endif

#TESTS = ORB5_TEST


INIT_EXE = ${BINDIR}/init_cpo_kernelB
CONTINUE_EXE = ${BINDIR}/continue_kernelB
BDSEQ_EXE = ${BINDIR}/bdseq_kernelB
ETS_EXE = ${BINDIR}/ets_kernelB
BOHMGB_EXE = ${BINDIR}/bohmgb_kernelB
GEM_EXE = ${BINDIR}/gem_kernelB
CHEASE_EXE = ${BINDIR}/chease_kernelB
DFEFI_EXE = ${BINDIR}/dfefi_kernelB
IMP4DV_EXE = ${BINDIR}/imp4dv_kernelB
GEM0_EXE = ${BINDIR}/gem0_kernelB
DV2FLUX_EXE = ${BINDIR}/dv2flux_kernelB
#NOTRANSP_EXE = ${BINDIR}/notransp_kernelB
BYPASS_EXE = ${BINDIR}/bypass_kernelB
ORB5_EXE = ${BINDIR}/orb5_kernelB
ifdef UAL
  INIT_UAL_EXE = ${BINDIR}/init_ual_kernelB
  WRITE_UAL_EXE = ${BINDIR}/write_ual_kernelB
endif



#.PHONY: all clean
# rules ########################################################################

all: ${SYS} ${KERNELS} ${TESTS}


${OBJDIR}/%.o: %.f90
	${FC90} -o $@ -c $< $(FC90FLAGS)


${OBJDIR}/%.o: %.c
	${CC} -o $@ -c $< ${CCFLAGS}


%.f90: %.F90
	$(FPP) $(FPPFLAGS) $< $@ 




INIT_CPO_K: ${INIT_CPO_O}
	$(LD) -o ${INIT_EXE} $^ -L${LIB_UAL} -L${LIB_CONSTANTS} -L${LIB_TYPES} -lmuscle2 -litmtypes -litmconstants -lual ${LDFLAGS} 

BDSEQ_K: ${BDSEQ_O}
	$(LD) -o ${BDSEQ_EXE} $^ -L${LIB_UAL} -L${LIB_CONSTANTS} -L${LIB_TYPES} -L${LIB_XML} -L${LIB_BDS} -L${BDSEQ} -lmuscle2 -lbdseq -lbds -lxmllib -litmconstants -litmtypes -lual ${LDFLAGS} 

ETS_K: ${ETS_O}
	$(LD) -o ${ETS_EXE} $^ -L${ETS} -L${LIB_BDS} -L${LIB_CONSTANTS} -L${LIB_TYPES} -L${LIB_XML} -L${LIB_UAL} -lmuscle2 -lets -lbds -lxmllib -litmconstants -litmtypes -lual ${LDFLAGS} ${LIBLAPACK}

BOHMGB_K: ${BOHMGB_O}
	$(LD) -o ${BOHMGB_EXE} $^ -L${BOHMGB} -L${LIB_BDS} -L${LIB_CONSTANTS} -L${LIB_TYPES} -L${LIB_XML} -L${LIB_UAL} -lmuscle2 -lbohmgb -lbds -lxmllib -litmconstants -litmtypes -lual ${LDFLAGS} 

GEM0_K: ${GEM0_O}
	$(LD) -o ${GEM0_EXE} $^ -L${GEM0} -L${LIB_BDS} -L${LIB_CONSTANTS} -L${LIB_TYPES} -L${LIB_XML} -L${LIB_UAL} -lmuscle2 -lgem0 -lbds -lxmllib -litmconstants -litmtypes -lual ${LDFLAGS} 

CONTINUE_K: ${CONTINUE_O}
	$(LD) -o ${CONTINUE_EXE} $^ -L${LIB_UAL} -L${LIB_CONSTANTS} -L${LIB_TYPES} -lmuscle2 -litmconstants -litmtypes -lual ${LDFLAGS} 

GEM_K: ${GEM_O}
	$(LD) -o ${GEM_EXE} $^ -L${GEM} -L${LIB_BDS} -L${LIB_CONSTANTS} -L${LIB_TYPES} -L${LIB_XML} -L${LIB_UAL} -lmuscle2 -lgem -lbds -lxmllib -litmconstants -litmtypes -lual ${LDFLAGS} ${FFTW_LIB}

CHEASE_K: ${CHEASE_O}
	$(LD) -o ${CHEASE_EXE} $^ -L${CHEASE} -L${LIB_UAL} -L/usr/lib64 -lmuscle2 -lchease_muscle -lual -lxml2  ${LDFLAGS} ${LIBLAPACK}

DFEFI_K: ${DFEFI_O}
	$(LD) -o ${DFEFI_EXE} $^ -L${DFEFI} -L${LIB_BDS} -L${LIB_CONSTANTS} -L${LIB_TYPES} -L${LIB_XML} -L${LIB_UAL} -lmuscle2 -ldfefi -lbds -lxmllib -litmconstants -litmtypes -lual ${LDFLAGS} ${FFTW_LIB}

IMP4DV_K: ${IMP4DV_O}
	$(LD) -o ${IMP4DV_EXE} $^ -L${IMP4DV} -L${LIB_BDS} -L${LIB_CONSTANTS} -L${LIB_TYPES} -L${LIB_UAL} -lmuscle2 -limp4dv -lbds -litmconstants -litmtypes -lual ${LDFLAGS} 

INIT_UAL_K: ${INIT_UAL_O}
	$(LD) -o ${INIT_UAL_EXE} $^ -L${LIB_CONSTANTS} -L${LIB_TYPES} -lmuscle2 -litmconstants -litmtypes ${LDFLAGS} ${LDUAL}

WRITE_UAL_K: ${WRITE_UAL_O}
	$(LD) -o ${WRITE_UAL_EXE} $^ -L${LIB_CONSTANTS} -L${LIB_TYPES} -lmuscle2 -litmconstants -litmtypes ${LDFLAGS} ${LDUAL}

DV2FLUX_K: ${DV2FLUX_O}
	$(LD) -o ${DV2FLUX_EXE} $^ -L${LIB_CONSTANTS} -L${LIB_TYPES} -L${LIB_UAL} -lmuscle2 -litmconstants -litmtypes -lual ${LDFLAGS} 

#NOTRANSP_K: ${NOTRANSP_O}
#	$(LD) -o ${NOTRANSP_EXE} $^ -L${LIB_UAL} -L${LIB_BDS} -L${LIB_CONSTANTS} -L${LIB_TYPES} -lmuscle2 -lbds -litmconstants -litmtypes -lual ${LDFLAGS}
#TEST_K: ${OBJDIR}/string_binding.o ${OBJDIR}/c_tools.o ${OBJDIR}/filetools.o ${OBJDIR}/ets_wrapper.o 
#	$(LD) -o ${BINDIR}/ets_test_wrapper $^ ${ETS}/libets.a ${ITM_UAL}/libitm.a ${LDFLAGS} ${LIBLAPACK}

BYPASS_K: ${BYPASS_O}
	$(LD) -o ${BYPASS_EXE} $^ -lmuscle2 ${LDFLAGS}

ORB5_K: ${ORB5_O}
	$(LD) -o ${ORB5_EXE} $^ -L${ORB5}/lib_compat -L${LIB_UAL} -L${LIB_BDS} -lmuscle2 -lorb5_compat -lual -lbds ${LDFLAGS} ${FFTW_LIB} ${ORB5_LDDIRS} ${ORB5_LDFLAGS}

ORB5_TEST: ${OBJDIR}/orb5_test.o
	$(LD) -o ${BINDIR}/orb5_test.exe $^ -L${ORB5}/lib_compat -L${LIB_UAL} -L${LIB_BDS} -lmuscle2 -lorb5_compat -lual -lbds ${LDFLAGS} ${FFTW_LIB} ${ORB5_LDDIRS} ${ORB5_LDFLAGS}

${SYS}: 
	@mkdir -p ${OBJDIR}
	@mkdir -p ${INCDIR}
	@mkdir -p ${BINDIR}

clean:
	rm -f src/*~ src/*_wrapper.f90 src/*_kernelB.f90 src/*_test.f90 *~ 
	rm -f ${INIT_EXE} ${BDSEQ_EXE} ${ETS_EXE} ${BOHMGB_EXE} ${CONTINUE_EXE} ${INIT_UAL_EXE} \
		${NOTRANSP_EXE} ${GEM_EXE} ${CHEASE_EXE} ${DFEFI_EXE} ${IMP4DV_EXE} ${GEM0_EXE} \
		${DV2FLUX_EXE} ${BYPASS_EXE} ${ORB5_EXE}
	rm -rf ${OBJDIR} ${INCDIR} ${BINDIR} 



####### special rules (due to clash of versions with chease) #######
${OBJDIR}/linterp.o: src/tools/linterp.f90
	${FC90} -o $@ -c $< $(FC90FLAGS) -I${INC_TYPES}

${OBJDIR}/bdseq_wrapper.o: src/bdseq_wrapper.f90
	${FC90} -o $@ -c $< $(FC90FLAGS) -I${INC_BDS} -I${INC_XML}

${OBJDIR}/bohmgb_wrapper.o: src/bohmgb_wrapper.f90
	${FC90} -o $@ -c $< $(FC90FLAGS) -I${INC_BDS} -I${INC_XML}

${OBJDIR}/gem0_wrapper.o: src/gem0_wrapper.f90
	${FC90} -o $@ -c $< $(FC90FLAGS) -I${INC_BDS} -I${INC_XML}

${OBJDIR}/notransp_wrapper.o: src/notransp_wrapper.f90
	${FC90} -o $@ -c $< $(FC90FLAGS) -I${INC_BDS} -I${INC_XML}

${OBJDIR}/ets_wrapper.o: src/ets_wrapper.f90
	${FC90} -o $@ -c $< $(FC90FLAGS) -I${ETS} -I${INC_CONSTANTS} -I${INC_TYPES} -I${INC_XML} 

${OBJDIR}/chease_wrapper.o: src/chease_wrapper.f90
	${FC90} -o $@ -c $< $(FC90FLAGS) -I${CHEASE} -I/usr/include/libxml2

${OBJDIR}/gem_wrapper.o: src/gem_wrapper.f90
	${FC90} -o $@ -c $< $(FC90FLAGS) -I${INC_BDS} -I${INC_XML} 

${OBJDIR}/dfefi_wrapper.o: src/dfefi_wrapper.f90
	${FC90} -o $@ -c $< $(FC90FLAGS) -I${INC_BDS} -I${INC_XML} 

${OBJDIR}/imp4dv_wrapper.o: src/imp4dv_wrapper.f90
	${FC90} -o $@ -c $< $(FC90FLAGS) -I${INC_BDS}  

${OBJDIR}/orb5_wrapper.o: src/orb5_wrapper.f90
	${FC90} -o $@ -c $< $(FC90FLAGS) -I${ORB5}/obj_compat -I${INC_BDS}

${OBJDIR}/orb5_kernelB.o: src/orb5_kernelB.f90
	${FC90} -o $@ -c $< $(FC90FLAGS) -I${ORB5}/obj_compat 

${OBJDIR}/orb5_test.o: src/orb5_test.f90
	${FC90} -o $@ -c $< $(FC90FLAGS) -I${ORB5}/obj_compat 
