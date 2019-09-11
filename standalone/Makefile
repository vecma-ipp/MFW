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
MPIF90FLAGS += ${INCLUDES} ${MODSPEC}${INCDIR} 


# Objects files ################################################################

SHARED_O = ${OBJDIR}/linterp.o ${OBJDIR}/string_binding.o ${OBJDIR}/csv_io.o ${OBJDIR}/spl.o

ETS_O = ${OBJDIR}/ets_standalone.o 
EQUILUPDATE_O = ${OBJDIR}/equilupdate_standalone.o  
CHEASE_O = ${SHARED_O} ${OBJDIR}/chease_standalone.o 
IMP4DV_O = ${SHARED_O} ${OBJDIR}/imp4dv_standalone.o
SOURCES_O = ${SHARED_O} ${OBJDIR}/sources_standalone.o
BOHMGB_O = ${SHARED_O} ${OBJDIR}/bohmgb_standalone.o

TESTS =  ETS_TEST EQUILUPDATE_TEST CHEASE_TEST IMP4DV_TEST COMBINED_TEST SOURCES_TEST BOHMGB_TEST LOOP_TEST1 LOOP_TEST2

ETS_EXE = ${BINDIR}/ets_test
EQUILUPDATE_EXE = ${BINDIR}/equilupdate_test
CHEASE_EXE = ${BINDIR}/chease_test
IMP4DV_EXE = ${BINDIR}/imp4dv_test
COMBINED_EXE = ${BINDIR}/combined_test
SOURCES_EXE = ${BINDIR}/sources_test
BOHMGB_EXE = ${BINDIR}/bohmgb_test
LOOP1_EXE = ${BINDIR}/loop_test1
LOOP2_EXE = ${BINDIR}/loop_test2


### Rules ######################################################################

all: ${SYS} ${TESTS} ${RUNS}


${OBJDIR}/%.o: %.f90
	${FC90} -o $@ -c $< $(FC90FLAGS)

${OBJDIR}/%.o: %.c
	${CC} -o $@ -c $< ${CCFLAGS}

%.f90: %.F90
	$(FPP) $(FPPFLAGS) $< $@ 

###  EXECUTABLES #################################################################

ETS_TEST: ${SHARED_O} ${ETS_O} ${EQUILUPDATE_O} ${OBJDIR}/ets_test.o
	$(MPIF90) -o  ${ETS_EXE} $^ -L${ETS} -L${LIB_BDS} -L${LIB_CONSTANTS} -L${LIB_TYPES} -L${LIB_XML} -L${LIB_UAL} -lets -lbds -lxmllib -litmconstants -litmtypes -lual ${LDFLAGS} ${LIBLAPACK}

EQUILUPDATE_TEST:${SHARED_O} ${EQUILUPDATE_O} ${OBJDIR}/equilupdate_test.o
	$(LD) -o ${EQUILUPDATE_EXE} $^ -L${ETS} -L${LIB_BDS} -L${LIB_CONSTANTS} -L${LIB_TYPES} -L${LIB_UAL} -lets -lbds -litmconstants -litmtypes -lual ${LDFLAGS} ${LIBLAPACK}

CHEASE_TEST: ${SHARED_O}  ${CHEASE_O} ${OBJDIR}/chease_test.o 
	$(MPIF90) -o ${CHEASE_EXE} $^ -L${CHEASE} -L${LIB_XML} -L${LIB_UAL} -lchease_muscle -lual -lxmllib -lxml2 ${LDFLAGS} ${LIBLAPACK}

IMP4DV_TEST:  ${SHARED_O} ${IMP4DV_O} ${OBJDIR}/imp4dv_test.o 
	$(LD) -o ${IMP4DV_EXE} $^ -L${IMP4DV} -L${LIB_BDS} -L${LIB_CONSTANTS} -L${LIB_TYPES} -L${LIB_UAL} -limp4dv -lbds -litmconstants -litmtypes -lual ${LDFLAGS} ${LIBLAPACK}

COMBINED_TEST: ${SHARED_O} ${ETS_O} ${IMP4DV_O} ${OBJDIR}/combined_test.o
	$(LD) -o ${COMBINED_EXE} $^ -L${IMP4DV} -L${ETS} -L${LIB_BDS} -L${LIB_CONSTANTS} -L${LIB_TYPES} -L${LIB_XML} -L${LIB_UAL} -limp4dv -lets -lbds -litmconstants -litmtypes -lxmllib -lual ${LDFLAGS} ${LIBLAPACK}

SOURCES_TEST:  ${SHARED_O} ${SOURCES_O} ${OBJDIR}/sources_test.o
	$(LD) -o ${SOURCES_EXE} $^ -L${ETS} -L${LIB_BDS} -L${LIB_CONSTANTS} -L${LIB_TYPES} -L${LIB_UAL} -L${LIB_XML} -lets -lbds -litmconstants -litmtypes -lual -lxmllib ${LDFLAGS} ${LIBLAPACK}

BOHMGB_TEST:  ${SHARED_O} ${BOHMGB_O} ${OBJDIR}/bohmgb_test.o
	$(LD) -o ${BOHMGB_EXE} $^ -L${BOHMGB} -L${LIB_BDS} -L${LIB_CONSTANTS} -L${LIB_TYPES} -L${LIB_XML} -L${LIB_UAL} -lbohmgb -lbds -litmconstants -litmtypes -lxmllib -lual ${LDFLAGS} ${LIBLAPACK}

LOOP_TEST1: ${SHARED_O} ${ETS_O} ${EQUILUPDATE_O} ${CHEASE_O} ${BOHMGB_O} ${OBJDIR}/loop_test1.o
	$(LD) -o ${LOOP1_EXE} $^ -L${ETS} -L${CHEASE} -L${BOHMGB} -L${LIB_BDS} -L${LIB_CONSTANTS} -L${LIB_TYPES} -L${LIB_XML} -L${LIB_UAL} -lets -lchease_muscle -lbohmgb -lbds -litmconstants -litmtypes -lual -lxmllib -lxml2 ${LDFLAGS} ${LIBLAPACK}

LOOP_TEST2: ${SHARED_O} ${ETS_O} ${EQUILUPDATE_O} ${CHEASE_O} ${BOHMGB_O} ${OBJDIR}/loop_test2.o
	$(LD) -o ${LOOP2_EXE} $^ -L${ETS} -L${CHEASE} -L${BOHMGB} -L${LIB_BDS} -L${LIB_CONSTANTS} -L${LIB_TYPES} -L${LIB_XML} -L${LIB_UAL} -lets -lchease_muscle -lbohmgb -lbds -litmconstants -litmtypes -lual -lxmllib -lxml2 ${LDFLAGS} ${LIBLAPACK}


##################################################################################

${SYS}: 
	@mkdir -p ${OBJDIR}
	@mkdir -p ${INCDIR}
	@mkdir -p ${BINDIR}

clean:
	rm -f src/*~  *~ 
	rm -f ${ETS_EXE} 
	rm -rf ${OBJDIR} ${INCDIR} ${BINDIR} 


####### special rules (due to clash of versions with chease) #######
${OBJDIR}/linterp.o: src/tools/linterp.f90
	${FC90} -o $@ -c $< $(FC90FLAGS) -I${INC_TYPES}

${OBJDIR}/csv_io.o: src/tools/csv_io.f90
	${FC90} -o $@ -c $< $(FC90FLAGS) 

${OBJDIR}/spl.o: src/tools/spl.f90
	${MPIF90} -o $@ -c $< $(FC90FLAGS) -traceback -check all -fp-stack-check -I${MKL_LIB} 

#${OBJDIR}/uq_modules.o: src/rop/uq_modules.f90
#	${MPIF90} -o $@ -c $< $(FC90FLAGS) -I${ETS} -I${INC_CONSTANTS} -I${INC_TYPES} -I${INC_XML} 

${OBJDIR}/ets_standalone.o: src/ets_standalone.f90
	${MPIF90} -o $@ -c $< $(FC90FLAGS) -I${ETS} -I${INC_CONSTANTS} -I${INC_TYPES} -I${INC_XML} 

${OBJDIR}/ets_test.o: src/ets_test.f90
	${MPIF90} -o $@ -c $< $(MPIF90FLAGS) -I${INC_TYPES}  

${OBJDIR}/equilupdate_standalone.o: src/equilupdate_standalone.f90
	${FC90} -o $@ -c $< $(FC90FLAGS) -I${ETS} -I${INC_CONSTANTS} -I${INC_TYPES} 

${OBJDIR}/equilupdate_test.o: src/equilupdate_test.f90
	${MPIF90} -o $@ -c $< $(MPIF90FLAGS) -I${INC_TYPES}  

${OBJDIR}/chease_standalone.o: src/chease_standalone.f90
	${MPIF90} -o $@ -c $< $(MPIF90FLAGS) -I${CHEASE} -I${INC_UAL} -I${INC_XML} -I/usr/include/libxml2

${OBJDIR}/chease_test.o: src/chease_test.f90
	${MPIF90} -o $@ -c $< $(MPIF90FLAGS) -I${CHEASE} -I${INC_UAL} -I${INC_TYPES} 

${OBJDIR}/imp4dv_standalone.o: src/imp4dv_standalone.f90
	${FC90} -o $@ -c $< $(FC90FLAGS) -I${INC_BDS}  

${OBJDIR}/combined_test.o: src/combined_test.f90
	${FC90} -o $@ -c $< $(FC90FLAGS) -I${INC_TYPES}  

${OBJDIR}/sources_standalone.o: src/sources_standalone.f90
	${FC90} -o $@ -c $< $(FC90FLAGS) -traceback -check all -fp-stack-check -I${ETS} -I${INC_XML} -I${INC_CONSTANTS} -I${INC_TYPES} 

${OBJDIR}/sources_test.o: src/sources_test.f90
	${MPIF90} -o $@ -c $< $(MPIF90FLAGS) -traceback -check all -fp-stack-check -I${ETS} -I${INC_TYPES} 

${OBJDIR}/bohmgb_standalone.o: src/bohmgb_standalone.f90
	${FC90} -o $@ -c $< $(FC90FLAGS) -I${INC_BDS} -I${INC_XML} 

${OBJDIR}/loop_test1.o: src/loop_test1.f90 
	${FC90} -o $@ -c $< $(FC90FLAGS) -I${INC_TYPES} 

${OBJDIR}/loop_test2.o: src/loop_test2.f90 
	${FC90} -o $@ -c $< $(FC90FLAGS) -I${INC_TYPES} 
