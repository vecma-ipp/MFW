#/* -*- makefile -*- */
include ../../config

# ual
export INC_UAL				=	../../ual/include/${SYS}/fortran_interface
export LIB_UAL				=	../../ual/lib/${SYS}/fortran_interface
export INC_TYPES			=	../../ual/include/${SYS}/types
export LIB_TYPES			= ../../ual/lib/${SYS}/types
export INC_CONSTANTS	= ../../ual/include/${SYS}/constants
export LIB_CONSTANTS	= ../../ual/lib/${SYS}/constants
export INC_XML				= ../../ual/include/${SYS}/xml
export LIB_XML				= ../../ual/lib/${SYS}/xml

# externals
export ETS = ../../externals/ets/obj/${SYS}

# standalone
export OBJ_STD = ../obj/${SYS}
export INC_STD = ../include/${SYS}
STD = I${INC_STD} ${OBJ_STD}/ets_standalone.o ${OBJ_STD}/linterp.o  ${OBJ_STD}/csv_io.o

# additional flags 
# TODO

# object files 
OBJDIR = ../obj/${SYS}
INCDIR = ../include/${SYS}
BINDIR = ../bin/${SYS}

RUN_ETS_EXE = ${BINDIR}/ets_run
RUNS = ETS_RUN

# rules
all: ${RUNS}

ETS_RUN: ${OBJDIR}/ets_run.o
	$(MPIF90) -o ${RUN_ETS_EXE} $^ -${STD} -L${ETS} -lets -L${LIB_UAL} -lual -L${LIB_TYPES} -litmtypes -L${LIB_XML} -lxmllib -L${LIB_CONSTANTS} -litmconstants ${LDFLAGS} ${LIBLAPACK}

${OBJDIR}/ets_run.o: ets_run.f90
	${MPIF90} -o $@ -c $< $(FC90FLAGS) -I${INC_STD} -I${ETS} -I${INC_UAL} -I${INC_TYPES} -I${INC_XML} -I${INC_CONSTANTS}  

clean:
	rm -rf ${OBJDIR}/ets_run.o ${INCDIR}/ets_rum.mod ${BINDIR}/ets_run 
