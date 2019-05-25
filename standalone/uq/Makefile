#/* -*- makefile -*- */
include ../../config

#  UAL
export INC_BDS = ../../externals/libbds/include/${SYS}
export LIB_BDS = ../../externals/libbds/lib/${SYS}
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
export BOHMGB = ../../externals/bohmgb/${SYS}

# Standalone
export OBJ_STD = ../obj/${SYS}
export INC_STD = ../include/${SYS}

TOOLS_STD  = ${INC_STD} ${OBJ_STD}/linterp.o  ${OBJ_STD}/csv_io.o  ${OBJ_STD}/spl.o
ETS_STD    = ${INC_STD} ${OBJ_STD}/ets_standalone.o ${OBJ_STD}/equilupdate_standalone.o 
CHEASE_STD = ${INC_STD} ${OBJ_STD}/chease_standalone.o 
SRC_STD 	 = ${INC_STD} ${OBJ_STD}/sources_standalone.o 
BOHMGB_STD = ${INC_STD} ${OBJDIR}/bohmgb_standalone.o

# Object files 
OBJDIR = ../obj/${SYS}
INCDIR = ../include/${SYS}
BINDIR = ../bin/${SYS}

RUN_ETS_EXE		 = ${BINDIR}/ets_run
RUN_CHEASE_EXE = ${BINDIR}/chease_run
RUN_BC_EXE   	 = ${BINDIR}/bc_run
RUN_SRC_EXE    = ${BINDIR}/gauss_src_run
RUN_BC_SRC_EXE = ${BINDIR}/bc_src_run

RUNS = ETS_RUN CHEASE_RUN BC_RUN GAUSS_SRC_RUN BC_SRC_RUN

# Rules
all: ${RUNS}

# ETS
ETS_RUN: ${OBJDIR}/ets_run.o
	$(MPIF90) -o ${RUN_ETS_EXE} $^ -I${ETS_STD} -I${TOOLS_STD} -L${ETS} -L${LIB_UAL} -L${LIB_TYPES} -L${LIB_XML} -L${LIB_CONSTANTS} -lets -lual -litmtypes -lxmllib -litmconstants ${LDFLAGS} ${LIBLAPACK}

${OBJDIR}/ets_run.o: ets_run.f90
	${MPIF90} -o $@ -c $< $(FC90FLAGS) -traceback -check all -fp-stack-check  -I${INC_STD} -I${ETS} -I${INC_UAL} -I${INC_TYPES} -I${INC_XML} -I${INC_CONSTANTS} 

# CHEASE
CHEASE_RUN: ${OBJDIR}/chease_run.o
	$(MPIF90) -o ${RUN_CHEASE_EXE} $^ -I${CHEASE_STD} -I${TOOLS_STD} -L${CHEASE} -L${LIB_XML} -L${LIB_UAL} -lchease_muscle -lual -lxmllib -lxml2 ${LDFLAGS} ${LIBLAPACK}

${OBJDIR}/chease_run.o: chease_run.f90
	${MPIF90} -o $@ -c $< $(FC90FLAGS) -I${INC_STD} -I${CHEASE} -I${INC_XML} -I/usr/include/libxml2 -I${INC_UAL} -I${INC_TYPES}

# LOOP BC: ETS + CHEASE + BOHMGB
BC_RUN: ${OBJDIR}/bc_run.o
	$(MPIF90) -o ${RUN_BC_EXE} $^ -I${ETS_STD} -I${CHEASE_STD} -I${BOHMGB_STD} -I${TOOLS_STD} -L${ETS} -L${CHEASE} -L${BOHMGB} -L${LIB_BDS} -L${LIB_UAL} -L${LIB_TYPES} -L${LIB_XML} -L${LIB_CONSTANTS} -lets -lchease_muscle -lbohmgb -lbds -lual -litmtypes -lxmllib -lxml2 -litmconstants ${LDFLAGS} ${LIBLAPACK}

${OBJDIR}/bc_run.o: bc_run.f90
	${MPIF90} -o $@ -c $< $(FC90FLAGS) -I${INC_STD} -I${ETS} -I${CHEASE} -I${BOHMGB} -I${INC_BDS} -I${INC_XML} -I/usr/include/libxml2 -I${INC_UAL} -I${INC_TYPES} -I${INC_CONSTANTS} 
	
# LOOP SRC: ETS + CHEASE + BOHMGB. UQ in Sources
GAUSS_SRC_RUN: ${OBJDIR}/gauss_src_run.o
	$(MPIF90) -o ${RUN_SRC_EXE} $^ -I${SRC_STD} -I${ETS_STD} -I${CHEASE_STD} -I${BOHMGB_STD} -I${TOOLS_STD} -L${ETS} -L${CHEASE} -L${BOHMGB} -L${LIB_BDS} -L${LIB_UAL} -L${LIB_TYPES} -L${LIB_XML} -L${LIB_CONSTANTS} -lets -lchease_muscle -lbohmgb -lbds -lual -litmtypes -lxmllib -lxml2 -litmconstants ${LDFLAGS} ${LIBLAPACK}

${OBJDIR}/gauss_src_run.o: gauss_src_run.f90
	${MPIF90} -o $@ -c $< $(FC90FLAGS) -I${INC_STD} -I${ETS} -I${CHEASE} -I${BOHMGB} -I${INC_BDS} -I${INC_XML} -I/usr/include/libxml2 -I${INC_UAL} -I${INC_TYPES} -I${INC_CONSTANTS} 

# LOOP SRC: ETS + CHEASE + BOHMGB: UQ in Sources and BC
BC_SRC_RUN: ${OBJDIR}/bc_src_run.o
	$(MPIF90) -o ${RUN_BC_SRC_EXE} $^ -I${SRC_STD} -I${ETS_STD} -I${CHEASE_STD} -I${BOHMGB_STD} -I${TOOLS_STD} -L${ETS} -L${CHEASE} -L${BOHMGB} -L${LIB_BDS} -L${LIB_UAL} -L${LIB_TYPES} -L${LIB_XML} -L${LIB_CONSTANTS} -lets -lchease_muscle -lbohmgb -lbds -lual -litmtypes -lxmllib -lxml2 -litmconstants ${LDFLAGS} ${LIBLAPACK}

${OBJDIR}/bc_src_run.o: bc_src_run.f90
	${MPIF90} -o $@ -c $< $(FC90FLAGS) -I${INC_STD} -I${ETS} -I${CHEASE} -I${BOHMGB} -I${INC_BDS} -I${INC_XML} -I/usr/include/libxml2 -I${INC_UAL} -I${INC_TYPES} -I${INC_CONSTANTS} 


# 
clean:
	rm -rf ${OBJDIR}/ets_run.o ${INCDIR}/ets_run.mod ${BINDIR}/ets_run 
	rm -rf ${OBJDIR}/chease_run.o ${INCDIR}/chease_run.mod ${BINDIR}/chease_run 
	rm -rf ${OBJDIR}/bc_run.o ${INCDIR}/bc_run.mod ${BINDIR}/bc_run 
	rm -rf ${OBJDIR}/gauss_src_run.o ${INCDIR}/gauss_src_run.mod ${BINDIR}/gauss_src_run 
	rm -rf ${OBJDIR}/bc_src_run.o ${INCDIR}/bc_src_run.mod ${BINDIR}/bc_src_run 
