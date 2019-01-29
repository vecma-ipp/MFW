#/* -*- makefile -*- */

# For f2py build with mkl lapack (used by EST):
# export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${MKL_HOME}/lib/intel64 
# => to be added to .bashrc, or source /path/to/file/mklvars.sh intel64

export ETS						=	../../externals/ets/obj/${SYS}
export INC_STD				= ../include/${SYS}
export OBJ_STD				= ../obj/${SYS}
export INC_UAL				=	../../ual/include/${SYS}/fortran_interface
export LIB_UAL				=	../../ual/lib/${SYS}/fortran_interface
export INC_TYPES			=	../../ual/include/${SYS}/types
export LIB_TYPES			= ../../ual/lib/${SYS}/types
export INC_CONSTANTS	= ../../ual/include/${SYS}/constants
export LIB_CONSTANTS	= ../../ual/lib/${SYS}/constants
export INC_XML				= ../../ual/include/${SYS}/xml
export LIB_XML				= ../../ual/lib/${SYS}/xml
export MKL_LIB				= ${MKL_HOME}/lib/intel64 
#include ../../config

MODULE = wrappers
SUFFIX = $(shell python3-config --extension-suffix)

LSTD = -I${INC_STD} ${OBJ_STD}/ets_standalone.o ${OBJ_STD}/linterp.o 
LETS = -I${ETS} -L${ETS} -lets
LUAL = -I${INC_UAL} -L${LIB_UAL} -lual -I${INC_TYPES} -L${LIB_TYPES} -litmtypes -I${INC_XML} -L${LIB_XML} -lxmllib -I${LIB_CONSTANTS} -L${LIB_CONSTANTS} -litmconstants
LLAPACK = -I${MKL_LIB} -L${MKL_LIB} -lmkl_sequential -lmkl_intel_lp64 -lmkl_core  # to be modified: depend of the compiler

all: ${MODULE}.so

F2PY = f2py
F2PY_FLAGS  = --fcompiler=intelem
F2PY_FLAGS += --f90flags='-g3 -O2' 

${MODULE}.so: 
	${F2PY} -c ${F2PY_FLAGS} ${MODULE}.pyf ${MODULE}.f90 ${LSTD} ${LETS} ${LUAL} ${LLAPACK} 
	mv ${MODULE}${SUFFIX} $@	

clean:
#	rm -r __pycache__ *.py[co]
	rm ${MODULE}.so

# rebuild the signature file if args are changed in f90 file
pyf:
	${F2PY} -m ${MODULE} -h ${MODULE}.pyf ${MODULE}.f90

clean-pyf:
	rm  ${MODULE}.pyf
