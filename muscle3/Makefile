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
INCLUDES += $(shell pkg-config --cflags libmuscle_fortran)

LDFLAGS += $(shell pkg-config --libs libmuscle_fortran)


FC90FLAGS += ${INCLUDES} ${MODSPEC}${INCDIR} 


# objects files ################################################################
STDA_SHARED_O = ${STDA_OBJ}/linterp.o ${STDA_OBJ}/string_binding.o \
	   ${STDA_OBJ}/c_tools.o ${STDA_OBJ}/filetools.o 

#ETS_O = ${SHARED_O} ${OBJDIR}/ets_wrapper.o ${OBJDIR}/ets_kernelB.o 
#BOHMGB_O = ${SHARED_O} ${OBJDIR}/bohmgb_wrapper.o ${OBJDIR}/bohmgb_kernelB.o
CHEASE_O = ${STDA_SHARED_O} ${STDA_OBJ}/chease_standalone.o ${OBJDIR}/chease_M3.o 
#IMP4DV_O = ${SHARED_O} ${OBJDIR}/imp4dv_wrapper.o ${OBJDIR}/imp4dv_kernelB.o
#GEM0_O = ${SHARED_O} ${OBJDIR}/gem0_wrapper.o ${OBJDIR}/gem0_kernelB.o

MODELS = CHEASE_M3

CHEASE_EXE = ${BINDIR}/chease_M3


#.PHONY: all clean
# rules ########################################################################

all: ${SYS} ${MODELS} #${TESTS}


${OBJDIR}/%.o: %.f90
	${FC90} -o $@ -c $< $(FC90FLAGS)


${OBJDIR}/%.o: %.c
	${CC} -o $@ -c $< ${CCFLAGS}


#%.f90: %.F90
#	$(FPP) $(FPPFLAGS) $< $@ 


CHEASE_M3: ${CHEASE_O} 
	$(FC90) -o ${CHEASE_EXE} $^ -L${CHEASE} -L${LIB_XML} -L${LIB_UAL} -lchease_muscle -lual -lxmllib -lxml2 ${LDFLAGS} ${LIBLAPACK}


${SYS}: 
	@mkdir -p ${OBJDIR}
	@mkdir -p ${INCDIR}
	@mkdir -p ${BINDIR}

clean:
	@rm -f src/*~ *~ 
	@rm -f ${CHEASE_EXE} 
	@rm -rf ${OBJDIR} ${INCDIR} ${BINDIR} 



####### special rules (due to clash of versions with chease) #######
${OBJDIR}/chease_M3.o: src/chease_M3.f90
	${FC90} -o $@ -c $< $(FC90FLAGS) -I${CHEASE} -I${INC_UAL} -I${INC_TYPES} -I${STDA_INC}
