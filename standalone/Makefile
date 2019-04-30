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


# objects files ################################################################
SHARED_O = ${OBJDIR}/linterp.o ${OBJDIR}/string_binding.o ${OBJDIR}/csv_io.o  ${OBJDIR}/uq_modules.o ${OBJDIR}/splfit.o

ETS_O = ${SHARED_O} ${OBJDIR}/ets_standalone.o ${OBJDIR}/ets_test.o 
CHEASE_O = ${SHARED_O} ${OBJDIR}/chease_standalone.o ${OBJDIR}/chease_test.o 
IMP4DV_O = ${SHARED_O} ${OBJDIR}/imp4dv_standalone.o ${OBJDIR}/imp4dv_test.o 
EQUILUPDATE_O = ${SHARED_O} ${OBJDIR}/equilupdate_standalone.o ${OBJDIR}/equilupdate_test.o 
SOURCES_O = ${SHARED_O} ${OBJDIR}/sources_standalone.o ${OBJDIR}/sources_test.o

#TESTS = ETS_TEST CHEASE_TEST 
TESTS =  ETS_TEST IMP4DV_TEST CHEASE_TEST COMBINED_TEST EQUILUPDATE_TEST SOURCES_TEST
ETS_EXE = ${BINDIR}/ets_test
CHEASE_EXE = ${BINDIR}/chease_test
IMP4DV_EXE = ${BINDIR}/imp4dv_test
COMBINED_EXE = ${BINDIR}/combined_test
EQUILUPDATE_EXE = ${BINDIR}/equilupdate_test
SOURCES_EXE = ${BINDIR}/sources_test

#.PHONY: all clean
# rules ########################################################################

all: ${SYS} ${TESTS}


${OBJDIR}/%.o: %.f90
	${FC90} -o $@ -c $< $(FC90FLAGS)


${OBJDIR}/%.o: %.c
	${CC} -o $@ -c $< ${CCFLAGS}


%.f90: %.F90
	$(FPP) $(FPPFLAGS) $< $@ 


ETS_TEST: ${ETS_O}
	$(MPIF90) -o ${ETS_EXE} $^ -L${ETS} -L${LIB_BDS} -L${LIB_CONSTANTS} -L${LIB_TYPES} -L${LIB_XML} -L${LIB_UAL} -lets -lbds -lxmllib -litmconstants -litmtypes -lual ${LDFLAGS} ${LIBLAPACK}

IMP4DV_TEST: ${IMP4DV_O}
	$(LD) -o ${IMP4DV_EXE} $^ -L${IMP4DV} -L${LIB_BDS} -L${LIB_CONSTANTS} -L${LIB_TYPES} -L${LIB_UAL} -limp4dv -lbds -litmconstants -litmtypes -lual ${LDFLAGS} 

CHEASE_TEST: ${CHEASE_O}
	$(MPIF90) -o ${CHEASE_EXE} $^ -L${CHEASE} -L${LIB_XML} -L${LIB_UAL} -lchease_muscle -lual -lxmllib -lxml2 ${LDFLAGS} ${LIBLAPACK}

COMBINED_TEST: ${SHARED_O} ${OBJDIR}/ets_standalone.o ${OBJDIR}/imp4dv_standalone.o ${OBJDIR}/combined_test.o
	$(LD) -o ${COMBINED_EXE} $^ -L${IMP4DV} -L${ETS} -L${LIB_BDS} -L${LIB_CONSTANTS} -L${LIB_TYPES} -L${LIB_XML} -L${LIB_UAL} -limp4dv -lets -lbds -litmconstants -litmtypes -lxmllib -lual ${LDFLAGS} ${LIBLAPACK}

EQUILUPDATE_TEST: ${EQUILUPDATE_O}
	$(LD) -o ${EQUILUPDATE_EXE} $^ -L${ETS} -L${LIB_BDS} -L${LIB_CONSTANTS} -L${LIB_TYPES} -L${LIB_UAL} -lets -lbds -litmconstants -litmtypes -lual ${LDFLAGS} ${LIBLAPACK}

SOURCES_TEST: ${SOURCES_O}
	$(LD) -o ${SOURCES_EXE} $^ -L${ETS} -L${LIB_BDS} -L${LIB_CONSTANTS} -L${LIB_TYPES} -L${LIB_UAL} -L${LIB_XML} -lets -lbds -litmconstants -litmtypes -lual -lxmllib ${LDFLAGS} 



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

${OBJDIR}/splfit.o: src/tools/splfit.f90
	${FC90} -o $@ -c $< $(FC90FLAGS) 

${OBJDIR}/uq_modules.o: src/uq_modules.f90
	${MPIF90} -o $@ -c $< $(FC90FLAGS) -I${ETS} -I${INC_CONSTANTS} -I${INC_TYPES} -I${INC_XML} 

${OBJDIR}/ets_standalone.o: src/ets_standalone.f90
	${MPIF90} -o $@ -c $< $(FC90FLAGS) -I${ETS} -I${INC_CONSTANTS} -I${INC_TYPES} -I${INC_XML} 

${OBJDIR}/ets_test.o: src/ets_test.f90
	${MPIF90} -o $@ -c $< $(MPIF90FLAGS) -I${INC_TYPES}  

${OBJDIR}/imp4dv_standalone.o: src/imp4dv_standalone.f90
	${FC90} -o $@ -c $< $(FC90FLAGS) -I${INC_BDS}  

${OBJDIR}/chease_standalone.o: src/chease_standalone.f90
	${MPIF90} -o $@ -c $< $(MPIF90FLAGS) -I${CHEASE} -I${INC_UAL} -I${INC_XML} -I/usr/include/libxml2

${OBJDIR}/chease_test.o: src/chease_test.f90
	${MPIF90} -o $@ -c $< $(MPIF90FLAGS) -I${CHEASE} -I${INC_UAL} -I${INC_TYPES} 

${OBJDIR}/combined_test.o: src/combined_test.f90
	${FC90} -o $@ -c $< $(FC90FLAGS) -I${INC_TYPES}  

${OBJDIR}/equilupdate_standalone.o: src/equilupdate_standalone.f90
	${FC90} -o $@ -c $< $(FC90FLAGS) -I${ETS} -I${INC_CONSTANTS} -I${INC_TYPES} 

${OBJDIR}/sources_standalone.o: src/sources_standalone.f90
	${FC90} -o $@ -c $< $(FC90FLAGS) -I${ETS} -I${INC_XML} -I${INC_CONSTANTS} -I${INC_TYPES} 

${OBJDIR}/sources_test.o: src/sources_test.f90
	${MPIF90} -o $@ -c $< $(MPIF90FLAGS) -I${ETS} -I${INC_TYPES} 
