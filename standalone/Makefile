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

# Additional Flags for sources_standalone and test
ifeq (${FC90},gfortran)
# GCC: gfortran and FC90
export FC90FLAGS_SOURCES = -fbacktrace -fcheck=all -fstack-check 
else
# Intel: ifort and mpifort
export FC90FLAGS_SOURCES = -traceback -check all -fp-stack-check 
endif

#.SUFFIXES:
#.SUFFIXES: obj/.o .f90 .F90

SRCDIR = src
OBJDIR = obj/${SYS}
INCDIR = include/${SYS}
BINDIR = bin/${SYS}


# Paths ########################################################################

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
# use MPI compiler for all Fortran kernel sources (default=no)       #
ifeq (${MPI},yes) 
 FC90 = ${MPIF90}
 LD = ${MPIF90}
 FPPFLAGS += -DMPI -DMPI2
endif

#--------------------------------------------------------------------#
# adds time monitoring functions in kernels/wrapper (default=no)     #
ifeq (${PERF},yes)
 FPPFLAGS += -DPERF
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


FC90FLAGS += ${INCLUDES} ${MODSPEC}${INCDIR} 
MPIFLAGS = ${FC90FLAGS} #${INCLUDES} ${MODSPEC}${INCDIR} 


# Objects files ################################################################

SHARED_O = ${OBJDIR}/linterp.o ${OBJDIR}/string_binding.o \
	   ${OBJDIR}/c_tools.o ${OBJDIR}/filetools.o 

ETS_O = ${OBJDIR}/ets_standalone.o 
EQUILUPDATE_O = ${OBJDIR}/equilupdate_standalone.o  
CHEASE_O = ${OBJDIR}/chease_standalone.o 
IMP4DV_O = ${OBJDIR}/imp4dv_standalone.o
SOURCES_O = ${OBJDIR}/sources_standalone.o
BOHMGB_O = ${OBJDIR}/bohmgb_standalone.o
GEM0_O = ${OBJDIR}/gem0_standalone.o
GEM_O = ${OBJDIR}/gem_standalone.o

UNIT_TESTS = ETS_TEST SOURCES_TEST EQUILUPDATE_TEST CHEASE_TEST IMP4DV_TEST BOHMGB_TEST GEM0_TEST GEM_TEST
LOOP_TESTS = LOOP_BGB LOOP_GEM0 LOOP_GEM ETS_CHEASE ETS_EQUILUPDATE LOOP_GEM_NOTRANSP

ETS_EXE = ${BINDIR}/ets_test
SOURCES_EXE = ${BINDIR}/sources_test
EQUILUPDATE_EXE = ${BINDIR}/equilupdate_test
CHEASE_EXE = ${BINDIR}/chease_test
IMP4DV_EXE = ${BINDIR}/imp4dv_test
BOHMGB_EXE = ${BINDIR}/bohmgb_test
GEM0_EXE = ${BINDIR}/gem0_test
GEM_EXE = ${BINDIR}/gem_test

LOOP_BGB_EXE = ${BINDIR}/loop_bgb
LOOP_GEM0_EXE = ${BINDIR}/loop_gem0
LOOP_GEM_EXE = ${BINDIR}/loop_gem
ETS_CHEASE_EXE = ${BINDIR}/ets_chease_test
ETS_EQUILUPDATE_EXE = ${BINDIR}/ets_equilupdate_test
LOOP_GEM_NOTRANSP_EXE = ${BINDIR}/loop_gem_notransp

# ROLAND CODE
ROP_EXE = ${BINDIR}/rop_test


### Rules ######################################################################
all: ${SYS} ${UNIT_TESTS} ${LOOP_TESTS} ${TEST_GEM_CHEASE}

${OBJDIR}/%.o: %.f90
	${FC90} -o $@ -c $< $(FC90FLAGS)

${OBJDIR}/%.o: %.c
	${CC} -o $@ -c $< ${CCFLAGS}

%.f90: %.F90
	$(FPP) $(FPPFLAGS) $< $@ 

###  EXECUTABLES #################################################################

ETS_TEST: ${SHARED_O} ${ETS_O} ${OBJDIR}/ets_test.o
	$(FC90) -o ${ETS_EXE} $^ -L${ETS} -L${LIB_BDS} -L${LIB_CONSTANTS} -L${LIB_TYPES} -L${LIB_XML} -L${LIB_UAL} -lets -lbds -lxmllib -litmconstants -litmtypes -lual ${LDFLAGS} ${LIBLAPACK}

SOURCES_TEST: ${SHARED_O} ${SOURCES_O} ${OBJDIR}/sources_test.o
	$(LD) -o ${SOURCES_EXE} $^ -L${ETS} -L${LIB_BDS} -L${LIB_CONSTANTS} -L${LIB_TYPES} -L${LIB_UAL} -L${LIB_XML} -lets -lbds -litmconstants -litmtypes -lual -lxmllib ${LDFLAGS} ${LIBLAPACK}

EQUILUPDATE_TEST: ${SHARED_O} ${EQUILUPDATE_O} ${OBJDIR}/equilupdate_test.o
	$(FC90) -o ${EQUILUPDATE_EXE} $^ -L${ETS} -L${LIB_BDS} -L${LIB_CONSTANTS} -L${LIB_TYPES} -L${LIB_UAL} -lets -lbds -litmconstants -litmtypes -lual ${LDFLAGS} ${LIBLAPACK}

CHEASE_TEST: ${SHARED_O} ${CHEASE_O} ${OBJDIR}/chease_test.o 
	$(FC90) -o ${CHEASE_EXE} $^ -L${CHEASE} -L${LIB_XML} -L${LIB_UAL} -lchease_muscle -lual -lxmllib -lxml2 ${LDFLAGS} ${LIBLAPACK}

IMP4DV_TEST: ${SHARED_O} ${IMP4DV_O} ${OBJDIR}/imp4dv_test.o 
	$(LD) -o ${IMP4DV_EXE} $^ -L${IMP4DV} -L${LIB_BDS} -L${LIB_CONSTANTS} -L${LIB_TYPES} -L${LIB_UAL} -limp4dv -lbds -litmconstants -litmtypes -lual ${LDFLAGS} ${LIBLAPACK}

BOHMGB_TEST:  ${SHARED_O} ${BOHMGB_O} ${OBJDIR}/bohmgb_test.o
	$(LD) -o ${BOHMGB_EXE} $^ -L${BOHMGB} -L${LIB_BDS} -L${LIB_CONSTANTS} -L${LIB_TYPES} -L${LIB_XML} -L${LIB_UAL} -lbohmgb -lbds -litmconstants -litmtypes -lxmllib -lual ${LDFLAGS} ${LIBLAPACK}

GEM0_TEST:  ${SHARED_O} ${GEM0_O} ${OBJDIR}/gem0_test.o
	$(LD) -o ${GEM0_EXE} $^ -L${GEM0} -L${LIB_BDS} -L${LIB_CONSTANTS} -L${LIB_TYPES} -L${LIB_XML} -L${LIB_UAL} -lgem0 -lbds -litmconstants -litmtypes -lxmllib -lual ${LDFLAGS} 

GEM_TEST:  ${SHARED_O} ${GEM_O} ${OBJDIR}/gem_test.o
	$(MPIF90) -o ${GEM_EXE} $^ -L${GEM} -L${LIB_BDS} -L${LIB_CONSTANTS} -L${LIB_TYPES} -L${LIB_XML} -L${LIB_UAL} -lgem -lbds -litmconstants -litmtypes -lxmllib -lual ${LDFLAGS} ${FFTW_LIB}

LOOP_BGB: ${SHARED_O} ${ETS_O} ${EQUILUPDATE_O} ${CHEASE_O} ${BOHMGB_O} ${SOURCES_O} ${OBJDIR}/loop_bgb.o
	$(LD) -o ${LOOP_BGB_EXE} $^ -L${ETS} -L${CHEASE} -L${BOHMGB} -L${LIB_BDS} -L${LIB_CONSTANTS} -L${LIB_TYPES} -L${LIB_XML} -L${LIB_UAL} -lets -lchease_muscle -lbohmgb -lbds -litmconstants -litmtypes -lual -lxmllib -lxml2 ${LDFLAGS} ${LIBLAPACK}

LOOP_GEM0: ${SHARED_O} ${ETS_O} ${EQUILUPDATE_O} ${CHEASE_O} ${GEM0_O} ${IMP4DV_O} ${SOURCES_O} ${OBJDIR}/loop_gem0.o
	$(LD) -o ${LOOP_GEM0_EXE} $^ -L${ETS} -L${CHEASE} -L${GEM0} -L${IMP4DV} -L${LIB_BDS} -L${LIB_CONSTANTS} -L${LIB_TYPES} -L${LIB_XML} -L${LIB_UAL} -lets -lchease_muscle -lgem0 -limp4dv -lbds -litmconstants -litmtypes -lual -lxmllib -lxml2 ${LDFLAGS} ${LIBLAPACK}

LOOP_GEM: ${SHARED_O} ${ETS_O} ${EQUILUPDATE_O} ${CHEASE_O} ${GEM_O} ${IMP4DV_O} ${SOURCES_O} ${OBJDIR}/loop_gem.o
	$(MPIF90) -o ${LOOP_GEM_EXE} $^ -L${ETS} -L${CHEASE} -L${GEM} -L${IMP4DV} -L${LIB_BDS} -L${LIB_CONSTANTS} -L${LIB_TYPES} -L${LIB_UAL} -L${LIB_XML} -lets -lchease_muscle -lgem -limp4dv -lbds -litmconstants -litmtypes -lual -lxmllib -lxml2  ${LDFLAGS} ${FFTW_LIB} ${LIBLAPACK}

ETS_CHEASE: ${SHARED_O} ${ETS_O} ${EQUILUPDATE_O} ${CHEASE_O} ${SOURCES_O} ${OBJDIR}/ets_chease_test.o
	$(LD) -o ${ETS_CHEASE_EXE} $^ -L${ETS} -L${CHEASE} -L${LIB_BDS} -L${LIB_CONSTANTS} -L${LIB_TYPES} -L${LIB_XML} -L${LIB_UAL} -lets -lchease_muscle -lbds -litmconstants -litmtypes -lual -lxmllib -lxml2 ${LDFLAGS} ${LIBLAPACK}

ETS_EQUILUPDATE: ${SHARED_O} ${ETS_O} ${EQUILUPDATE_O} ${SOURCES_O} ${OBJDIR}/ets_equilupdate_test.o
	$(LD) -o ${ETS_EQUILUPDATE_EXE} $^ -L${ETS} -L${LIB_BDS} -L${LIB_CONSTANTS} -L${LIB_TYPES} -L${LIB_XML} -L${LIB_UAL} -lets -lbds -litmconstants -litmtypes -lual -lxmllib ${LDFLAGS} ${LIBLAPACK}

#ROP_TEST: ${OBJDIR}/uq_modules.o ${OBJDIR}/rop_test.o
#	$(FC90) -o  ${ROP_EXE} $^  -L${LIB_BDS} -L${LIB_CONSTANTS} -L${LIB_TYPES} -L${LIB_XML} -L${LIB_UAL} -lbds -lxmllib -litmconstants -litmtypes -lual ${LDFLAGS} ${LIBLAPACK}

LOOP_GEM_NOTRANSP: ${SHARED_O} ${ETS_O} ${EQUILUPDATE_O} ${CHEASE_O} ${GEM_O} ${IMP4DV_O} ${SOURCES_O} ${OBJDIR}/loop_gem_notransp.o
	$(MPIF90) -o ${LOOP_GEM_NOTRANSP_EXE} $^ -L${ETS} -L${CHEASE} -L${GEM} -L${IMP4DV} -L${LIB_BDS} -L${LIB_CONSTANTS} -L${LIB_TYPES} -L${LIB_UAL} -L${LIB_XML} -lets -lchease_muscle -lgem -limp4dv -lbds -litmconstants -litmtypes -lual -lxmllib -lxml2  ${LDFLAGS} ${FFTW_LIB} ${LIBLAPACK}

##################################################################################

${SYS}: 
	@mkdir -p ${OBJDIR}
	@mkdir -p ${INCDIR}
	@mkdir -p ${BINDIR}

clean:
	rm -f src/*~  *~ 
	rm -rf ${OBJDIR} ${INCDIR} ${BINDIR} 


####### special rules (due to clash of versions with chease) #######
${OBJDIR}/linterp.o: src/tools/linterp.f90
	${FC90} -o $@ -c $< $(FC90FLAGS) -I${INC_TYPES}

${OBJDIR}/string_binding.o: src/tools/string_binding.f90
	${FC90} -o $@ -c $< $(FC90FLAGS) 

${OBJDIR}/ets_standalone.o: src/ets_standalone.f90
	${FC90} -o $@ -c $< $(FC90FLAGS) -I${ETS} -I${INC_CONSTANTS} -I${INC_TYPES} -I${INC_XML} 

${OBJDIR}/sources_standalone.o: src/sources_standalone.f90
	${FC90} -o $@ -c $< $(FC90FLAGS) $(FC90FLAGS_SOURCES) -I${ETS} -I${INC_XML} -I${INC_CONSTANTS} -I${INC_TYPES} 

${OBJDIR}/sources_test.o: src/sources_test.f90
	${FC90} -o $@ -c $< $(FC90FLAGS) $(FC90FLAGS_SOURCES) -I${ETS} -I${INC_TYPES} 

${OBJDIR}/ets_test.o: src/ets_test.f90
	${FC90} -o $@ -c $< $(FC90FLAGS) -I${INC_TYPES}  

${OBJDIR}/equilupdate_standalone.o: src/equilupdate_standalone.f90
	${FC90} -o $@ -c $< $(FC90FLAGS) -I${ETS} -I${INC_CONSTANTS} -I${INC_TYPES} 

${OBJDIR}/equilupdate_test.o: src/equilupdate_test.f90
	${FC90} -o $@ -c $< $(FC90FLAGS) -I${INC_TYPES}  

${OBJDIR}/chease_standalone.o: src/chease_standalone.f90
	${FC90} -o $@ -c $< $(FC90FLAGS) -I${CHEASE} -I${INC_UAL} -I${INC_XML} -I/usr/include/libxml2

${OBJDIR}/chease_test.o: src/chease_test.f90
	${FC90} -o $@ -c $< $(FC90FLAGS) -I${CHEASE} -I${INC_UAL} -I${INC_TYPES} 

${OBJDIR}/imp4dv_standalone.o: src/imp4dv_standalone.f90
	${FC90} -o $@ -c $< $(FC90FLAGS) -I${INC_BDS}  

${OBJDIR}/imp4dv_test.o: src/imp4dv_test.f90
	${FC90} -o $@ -c $< $(FC90FLAGS) -I${INC_BDS}  

${OBJDIR}/bohmgb_standalone.o: src/bohmgb_standalone.f90
	${FC90} -o $@ -c $< $(FC90FLAGS) -I${INC_BDS} -I${INC_XML} 

${OBJDIR}/bohmgb_test.o: src/bohmgb_test.f90
	${FC90} -o $@ -c $< $(FC90FLAGS) -I${INC_BDS}

${OBJDIR}/gem0_standalone.o: src/gem0_standalone.f90
	${FC90} -o $@ -c $< $(FC90FLAGS) -I${INC_BDS} -I${INC_XML} 

${OBJDIR}/gem0_test.o: src/gem0_test.f90
	${FC90} -o $@ -c $< $(FC90FLAGS) -I${INC_BDS}

${OBJDIR}/gem_standalone.o: src/gem_standalone.f90
	${MPIF90} -o $@ -c $< $(MPIFLAGS) -I${INC_BDS} -I${INC_XML}

${OBJDIR}/gem_test.o: src/gem_test.f90
	${MPIF90} -o $@ -c $< $(MPIFLAGS)

${OBJDIR}/loop_bgb.o: src/loop_bgb.f90 
	${FC90} -o $@ -c $< $(FC90FLAGS) -I${INC_TYPES} 

${OBJDIR}/loop_gem0.o: src/loop_gem0.f90 
	${FC90} -o $@ -c $< $(FC90FLAGS) -I${INC_TYPES} 

${OBJDIR}/loop_gem.o: src/loop_gem.f90 
	${MPIF90} -o $@ -c $< $(MPIFLAGS) -I${INC_TYPES}

${OBJDIR}/ets_chease_test.o: src/ets_chease_test.f90 
	${FC90} -o $@ -c $< $(FC90FLAGS) -I${INC_TYPES} 

${OBJDIR}/ets_equilupdate_test.o: src/ets_equilupdate_test.f90 
	${FC90} -o $@ -c $< $(FC90FLAGS) -I${INC_TYPES} 

#${OBJDIR}/rop_test.o: src/rop/rop_test.f90
#	${FC90} -o $@ -c $< $(FC90FLAGS) -I${INC_CONSTANTS} -I${INC_TYPES} -I${INC_XML}
 
${OBJDIR}/loop_gem_notransp.o: src/loop_gem_notransp.f90
	${MPIF90} -o $@ -c $< $(MPIFLAGS) -I${INC_TYPES}

