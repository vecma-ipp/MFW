#/* -*- makefile -*- */
include ../../config

#  UAL
export INC_UAL				=	../../ual/include/${SYS}/fortran_interface
export LIB_UAL				=	../../ual/lib/${SYS}/fortran_interface
export INC_TYPES			=	../../ual/include/${SYS}/types
export LIB_TYPES			= ../../ual/lib/${SYS}/types
export INC_CONSTANTS	= ../../ual/include/${SYS}/constants
export LIB_CONSTANTS	= ../../ual/lib/${SYS}/constants
export INC_XML				= ../../ual/include/${SYS}/xml
export LIB_XML				= ../../ual/lib/${SYS}/xml

# Externals
export ETS 		= ../../externals/ets/obj/${SYS}
export CHEASE = ../../externals/chease/src-f90

# Standalone
export OBJ_STD = ../obj/${SYS}
export INC_STD = ../include/${SYS}

TOOLS_STD  = ${INC_STD} ${OBJ_STD}/linterp.o  ${OBJ_STD}/csv_io.o
ETS_STD    = ${INC_STD} ${OBJ_STD}/ets_standalone.o ${OBJ_STD}/equilupdate_standalone.o 
CHEASE_STD = ${INC_STD} ${OBJ_STD}/chease_standalone.o 

# Object files 
OBJDIR = ../obj/${SYS}
INCDIR = ../include/${SYS}
BINDIR = ../bin/${SYS}

RUN_ETS_EXE    = ${BINDIR}/ets_run
RUN_CHEASE_EXE = ${BINDIR}/chease_run
RUN_FULL_EXE   = ${BINDIR}/full_run

RUNS = ETS_RUN CHEASE_RUN FULL_RUN

# Rules
all: ${RUNS}

ETS_RUN: ${OBJDIR}/ets_run.o
	$(MPIF90) -o ${RUN_ETS_EXE} $^ -I${ETS_STD} -I${TOOLS_STD} -L${ETS} -L${LIB_UAL} -L${LIB_TYPES} -L${LIB_XML} -L${LIB_CONSTANTS} -lets -lual -litmtypes -lxmllib -litmconstants ${LDFLAGS} ${LIBLAPACK}

${OBJDIR}/ets_run.o: ets_run.f90
	${MPIF90} -o $@ -c $< $(FC90FLAGS) -I${INC_STD} -I${ETS} -I${INC_UAL} -I${INC_TYPES} -I${INC_XML} -I${INC_CONSTANTS} 

CHEASE_RUN: ${OBJDIR}/chease_run.o
	$(MPIF90) -o ${RUN_CHEASE_EXE} $^ -I${CHEASE_STD} -I${TOOLS_STD} -L${CHEASE} -L${LIB_UAL} -lchease_muscle -lual -lxml2 ${LDFLAGS} ${LIBLAPACK}

${OBJDIR}/chease_run.o: chease_run.f90
	${MPIF90} -o $@ -c $< $(FC90FLAGS) -I${INC_STD} -I${CHEASE} -I${INC_UAL} -I${INC_TYPES}

# TODO check link-editing  between chease and ets (xml dependencies?)
FULL_RUN: ${OBJDIR}/full_run.o
	$(MPIF90) -o ${RUN_FULL_EXE} $^ -I${ETS_STD} -I${CHEASE_STD} -I${TOOLS_STD} -L${ETS} -L${CHEASE} -L${LIB_UAL} -L${LIB_TYPES} -L${LIB_XML} -L${LIB_CONSTANTS} -lets -lchease_muscle -lxml2 -lual -litmtypes -lxmllib -litmconstants ${LDFLAGS} ${LIBLAPACK}

${OBJDIR}/full_run.o: full_run.f90
	${MPIF90} -o $@ -c $< $(FC90FLAGS) -I${INC_STD} -I${ETS} -I${CHEASE} -I/usr/include/libxml2 -I${INC_UAL} -I${INC_TYPES} -I${INC_CONSTANTS} 

# 
clean:
	rm -rf ${OBJDIR}/ets_run.o ${INCDIR}/ets_run.mod ${BINDIR}/ets_run 
	rm -rf ${OBJDIR}/chease_run.o ${INCDIR}/chease_run.mod ${BINDIR}/chease_run 
	rm -rf ${OBJDIR}/full_run.o ${INCDIR}/full_run.mod ${BINDIR}/full_run 
