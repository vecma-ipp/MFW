#/* -*- makefile -*- */

# For f2py build with mkl lapack (used by EST):
# export LD_LIBRARY_PATH=${LD_LIBRARY_PATH}:${MKL_HOME}/lib/intel64 
# => to be added to .bashrc, or source /path/to/file/mklvars.sh intel64

export INC_UAL				=	../../ual/include/DEFAULT/fortran_interface
export LIB_UAL				=	../../ual/lib/DEFAULT/fortran_interface
export INC_TYPES			=	../../ual/include/DEFAULT/types
export LIB_TYPES			= ../../ual/lib/DEFAULT/types
export INC_CONSTANTS	= ../../ual/include/DEFAULT/constants
export LIB_CONSTANTS	= ../../ual/lib/DEFAULT/constants
export INC_XML				= ../../ual/include/DEFAULT/xml
export LIB_XML				= ../../ual/lib/DEFAULT/xml
#include ../../config

MODULE = wrappers
SUFFIX = $(shell python3-config --extension-suffix)

LUAL = -I${INC_UAL} -L${LIB_UAL} -lual -I${INC_TYPES} -L${LIB_TYPES} -litmtypes -I${INC_XML} -L${LIB_XML} -lxmllib -I${LIB_CONSTANTS} -L${LIB_CONSTANTS} -litmconstants

all: ${MODULE}.so

F2PY = f2py
F2PY_FLAGS  = --fcompiler=gnu95
F2PY_FLAGS += --f90flags='-g3 -O2' 

${MODULE}.so: 
	${F2PY} -c ${F2PY_FLAGS} ${MODULE}.pyf ${MODULE}.f90  ${LUAL}
	mv ${MODULE}${SUFFIX} $@	

clean:
#	rm -r __pycache__ *.py[co]
	rm ${MODULE}.so

# rebuild the signature file if args are changed in f90 file
pyf:
	${F2PY} -m ${MODULE} -h ${MODULE}.pyf ${MODULE}.f90

clean-pyf:
	rm  ${MODULE}.pyf
