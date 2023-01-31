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

STDA_INC = ../standalone/include/${SYS}
STDA_LIB = ../standalone/lib/${SYS}
STDA_OBJ = ../standalone/obj/${SYS}

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
VPATH = ${SRCDIR} 


# additional flags #############################################################
INCLUDES = -I${INCDIR} -I${INC_UAL} -I${STDA_INC}
MUSCLE_INCLUDES = $(shell pkg-config --cflags libmuscle_fortran ymmsl_fortran  ) 
MUSCLE_MPI_INCLUDES = $(shell pkg-config --cflags libmuscle_mpi_fortran ymmsl_fortran  )	#OL 

#--------------------------------------------------------------------#
# use MPI compiler for all Fortran kernel sources (default=no)       #
ifeq (${MPI},yes) 
 FC90 = ${MPIF90}
 LD = ${MPIF90}
 FPPFLAGS += -DMPI -DMPI2
endif

MUSCLE_LDFLAGS += $(shell pkg-config --libs libmuscle_fortran ymmsl_fortran  )
MUSCLE_MPI_LDFLAGS += $(shell pkg-config --libs libmuscle_mpi_fortran ymmsl_fortran  )	#OL
#NB: be careful with additional explicit flag: may be not needed

# testing out a linkage issue
RPATHLINK_FLAG = -Wl,-rpath-link,/u/yyudin/muscle3/lib
MUSCLE_LDFLAGS += ${RPATHLINK_FLAG}

FC90FLAGS += ${INCLUDES} ${MODSPEC}${INCDIR}
MPI_FC90FLAGS = ${FC90FLAGS} ${MUSCLE_MPI_INCLUDES}
FC90FLAGS += ${MUSCLE_INCLUDES}

# objects files ################################################################
STDA_SHARED_O = ${STDA_OBJ}/linterp.o ${STDA_OBJ}/string_binding.o \
	   ${STDA_OBJ}/c_tools.o ${STDA_OBJ}/filetools.o 

INIT_O = ${STDA_SHARED_O} ${OBJDIR}/init_M3.o
STOP_O = ${STDA_SHARED_O} ${OBJDIR}/stop_M3.o
ETS_O = ${STDA_SHARED_O} ${STDA_OBJ}/ets_standalone.o ${STDA_OBJ}/equilupdate_standalone.o ${OBJDIR}/ets_M3.o 
CHEASE_O = ${STDA_SHARED_O} ${STDA_OBJ}/chease_standalone.o ${OBJDIR}/chease_M3.o
GEM0_O = ${STDA_SHARED_O} ${STDA_OBJ}/gem0_standalone.o ${STDA_OBJ}/imp4dv_standalone.o ${OBJDIR}/gem0_M3.o
DUPLICATE_O = ${OBJDIR}/duplicate_M3.o
CPDUP_O = ${OBJDIR}/cpdup_M3.o
#EQUILUPDATE_O = ${STDA_SHARED_O} ${STDA_OBJ}/equilupdate_standalone.o ${OBJDIR}/equilupdate_M3.o 
IMP4DV_O = ${STDA_SHARED_O} ${STDA_OBJ}/imp4dv_standalone.o ${OBJDIR}/imp4dv_M3.o
GEM_O = ${STDA_SHARED_O} ${STDA_OBJ}/gem_standalone.o ${OBJDIR}/gem_M3.o
BOHMGB_O = ${STDA_SHARED_O} ${STDA_OBJ}/bohmgb_standalone.o ${OBJDIR}/bohmgb_M3.o

MODELS = INIT_M3 STOP_M3 ETS_M3 CHEASE_M3 GEM0_M3 DUPLICATE_M3 GEM_M3 BOHMGB_M3 IMP4DV_M3 CPDUP_M3
#EQUILUPDATE_M3

INIT_EXE = ${BINDIR}/init_M3
STOP_EXE = ${BINDIR}/stop_M3
ETS_EXE = ${BINDIR}/ets_M3
CHEASE_EXE = ${BINDIR}/chease_M3
GEM0_EXE = ${BINDIR}/gem0_M3
DUPLICATE_EXE = ${BINDIR}/duplicate_M3
CPDUP_EXE = ${BINDIR}/cpdup_M3
#EQUILUPDATE_EXE = ${BINDIR}/equilupdate_M3
IMP4DV_EXE = ${BINDIR}/imp4dv_M3
GEM_EXE = ${BINDIR}/gem_M3
BOHMGB_EXE = ${BINDIR}/bohmgb_M3


#.PHONY: all clean
# rules ########################################################################

all: ${SYS} ${MODELS} 


${OBJDIR}/%.o: %.f90
	${FC90} -o $@ -c $< $(FC90FLAGS)


${OBJDIR}/%.o: %.c
	${CC} -o $@ -c $< ${CCFLAGS}


${OBJDIR}/gem_standalone.o: gem_standalone.f90
	${FC90} -o $@ -c $< $(MPI_FC90FLAGS)


${OBJDIR}/gem_M3.o: gem_M3.f90
	${FC90} -o $@ -c $< $(MPI_FC90FLAGS)


#%.f90: %.F90
#	$(FPP) $(FPPFLAGS) $< $@ 

INIT_M3: ${INIT_O} 
	$(FC90) -o ${INIT_EXE} $^ -L${LIB_TYPES} -L${LIB_UAL} -litmtypes -lual ${MUSCLE_LDFLAGS}

STOP_M3: ${STOP_O} 
	$(FC90) -o ${STOP_EXE} $^ -L${LIB_TYPES} -L${LIB_UAL} -litmtypes -lual ${MUSCLE_LDFLAGS} 

ETS_M3: ${ETS_O} 
	$(FC90) -o ${ETS_EXE} $^ -L${ETS} -L${LIB_BDS} -L${LIB_CONSTANTS} -L${LIB_TYPES} -L${LIB_XML} -L${LIB_UAL} -lets -lbds -lxmllib -litmconstants -litmtypes -lual ${MUSCLE_LDFLAGS} ${LIBLAPACK}

CHEASE_M3: ${CHEASE_O} 
	$(FC90) -o ${CHEASE_EXE} $^ -L${CHEASE} -L${LIB_XML} -L${LIB_UAL} -lchease_muscle -lual -lxmllib -lxml2 ${MUSCLE_LDFLAGS} ${LIBLAPACK}

GEM0_M3: ${GEM0_O} 
	$(FC90) -o ${GEM0_EXE} $^ -L${GEM0} -L${IMP4DV} -L${LIB_BDS} -L${LIB_CONSTANTS} -L${LIB_TYPES} -L${LIB_XML} -L${LIB_UAL} -lgem0 -limp4dv -lbds -litmconstants -litmtypes -lxmllib -lual ${MUSCLE_LDFLAGS} 

DUPLICATE_M3: ${DUPLICATE_O} 
	$(FC90) -o ${DUPLICATE_EXE} $^ ${MUSCLE_LDFLAGS} 

CPDUP_M3: ${CPDUP_O} 
	$(FC90) -o ${CPDUP_EXE} $^ ${MUSCLE_LDFLAGS}

#EQUILUPDATE_M3: ${EQUILUPDATE_O} 
#	$(FC90) -o ${EQUILUPDATE_EXE} $^ -L${ETS} -L${LIB_BDS} -L${LIB_CONSTANTS} -L${LIB_TYPES} -L${LIB_UAL} -lets -lbds -litmconstants -litmtypes -lual ${MUSCLE_LDFLAGS} ${LIBLAPACK}
IMP4DV_M3: ${IMP4DV_O} 
	$(FC90) -o ${IMP4DV_EXE} $^ -L${IMP4DV} -L${LIB_BDS} -L${LIB_CONSTANTS} -L${LIB_TYPES} -L${LIB_UAL} -limp4dv -lbds -litmconstants -litmtypes -lual ${MUSCLE_LDFLAGS} 

GEM_M3: ${GEM_O} 
	$(FC90) -o ${GEM_EXE} $^ -L${GEM} -L${LIB_BDS} -L${LIB_CONSTANTS} -L${LIB_TYPES} -L${LIB_XML} -L${LIB_UAL} -lgem -lbds -litmconstants -litmtypes -lxmllib -lual ${FFTW_LIB} ${MUSCLE_MPI_LDFLAGS}

BOHMGB_M3: ${BOHMGB_O}
	$(FC90) -o ${BOHMGB_EXE} $^ -L${BOHMGB} -L${LIB_BDS} -L${LIB_CONSTANTS} -L${LIB_TYPES} -L${LIB_XML} -L${LIB_UAL} -lbohmgb -lbds -litmconstants -litmtypes -lxmllib -lual ${MUSCLE_LDFLAGS}


${SYS}: 
	@mkdir -p ${OBJDIR}
	@mkdir -p ${INCDIR}
	@mkdir -p ${BINDIR}

clean:
	@rm -f src/*~ *~ 
	@rm -f ${CHEASE_EXE} ${GEM0_EXE} ${ETS_EXE} ${INIT_EXE} ${STOP_EXE} ${DUPLICATE_EXE} ${GEM_EXE} ${BOHMGB_EXE} ${IMP4DV_EXE} ${CPDUP_EXE}
	@rm -rf ${OBJDIR} ${INCDIR} ${BINDIR} 



####### special rules (due to clash of versions with chease) #######
${OBJDIR}/chease_M3.o: src/chease_M3.f90
	${FC90} -o $@ -c $< $(FC90FLAGS) -I${CHEASE} -I${INC_UAL} -I${INC_TYPES} -I${STDA_INC}
