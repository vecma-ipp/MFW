#/* -*- makefile -*- */


# paths ########################################################################
SRCDIR = 
LIBDIR = ../lib/${SYS}/types
INCDIR = ../include/${SYS}/types


# set up the VPATH for searching for source files ##############################
VPATH = ${SRCDIR}


# additional flags #############################################################
INCLUDES = 
FC90FLAGS += ${INCLUDES} ${MODSPEC}${INCDIR} 


# objects files ################################################################
OBJ = 	itm_types.o 


# rules ########################################################################
.PHONY: clean


%.o: %.f90
	$(FC90) -o $@ -c $< $(FC90FLAGS)


all: ${LIBDIR}/libitmtypes.a


${LIBDIR}/libitmtypes.a: ${OBJ}
	ar -r $@ $^ 
	ranlib $@


clean:
	rm -f ${LIBDIR}/libitmtypes.a ${INCDIR}/*.mod *.o *~


