#/* -*- makefile -*- */


# paths ########################################################################
SRCDIR = 
LIBDIR = ../lib/${SYS}/constants
INCDIR = ../include/${SYS}


# set up the VPATH for searching for source files ##############################
VPATH = ${SRCDIR} 


# additional flags #############################################################
INCLUDES = -I${INCDIR}/types 
FC90FLAGS += ${INCLUDES} ${MODSPEC}${INCDIR}/constants 


# objects files ################################################################
OBJ = 	itm_constants.o                    


# rules ########################################################################
.PHONY: clean


%.o: %.f90
	$(FC90) -o $@ -c $< $(FC90FLAGS)


all: ${LIBDIR}/libitmconstants.a


${LIBDIR}/libitmconstants.a: ${OBJ}
	ar -r $@ $^ 
	ranlib $@


clean:
	rm -f ${LIBDIR}/libitmconstants.a ${INCDIR}/constants/*.mod *.o *~



