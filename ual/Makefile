#/* -*- makefile -*- */


# paths ########################################################################
LIBDIR = lib/${SYS}
INCDIR = include/${SYS}

TARGET = TYPES CONSTANTS FORTRAN XML

# rules ########################################################################
.PHONY: ${SYS} clean


all: ${SYS} ${TARGET}


TYPES:
	@echo -e "\033[36m\033[1m    itmtypes \033[0m"; \
	(mkdir -p ${INCDIR}/types; mkdir -p ${LIBDIR}/types; \
	$(MAKE) --no-print-directory -C types) \
	|| echo -e "\033[31m\033[1m    FAIL \033[0m"


CONSTANTS: TYPES
	@echo -e "\033[36m\033[1m    itmconstants \033[0m"; \
	(mkdir -p ${INCDIR}/constants; mkdir -p ${LIBDIR}/constants; \
	$(MAKE) --no-print-directory -C constants) \
	|| echo -e "\033[31m\033[1m    FAIL \033[0m"


FORTRAN: 
	@echo -e "\033[36m\033[1m    fortran interface \033[0m"; \
	(mkdir -p ${INCDIR}/fortran_interface; mkdir -p ${LIBDIR}/fortran_interface; \
	$(MAKE) --no-print-directory -C fortran_interface) \
	|| echo -e "\033[31m\033[1m    FAIL \033[0m"



XML: TYPES FORTRAN 
	@echo -e "\033[36m\033[1m    xmllib \033[0m"; \
	(mkdir -p ${INCDIR}/xml; mkdir -p ${LIBDIR}/xml; \
	$(MAKE) --no-print-directory -C xml) \
	|| echo -e "\033[31m\033[1m    FAIL \033[0m"



${SYS}:
	@mkdir -p ${INCDIR}
	@mkdir -p ${LIBDIR}


clean: 
	$(MAKE) --no-print-directory -C types clean
	$(MAKE) --no-print-directory -C constants clean
	$(MAKE) --no-print-directory -C fortran_interface clean
	$(MAKE) --no-print-directory -C xml clean
	rm -rf ${LIBDIR} ${INCDIR}



