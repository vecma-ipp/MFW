#/* -*- makefile -*- */


# paths ########################################################################
SRCDIR = 
LIBDIR = ../lib/${SYS}/fortran_interface
INCDIR = ../include/${SYS}/fortran_interface


# set up the VPATH for searching for source files ##############################
VPATH = ${SRCDIR}


# additional flags #############################################################
INCLUDES = -I${INCDIR} 
FC90FLAGS += ${INCLUDES} ${MODSPEC}${INCDIR} 


# objects files ################################################################
OBJ = 	euitm_ual_defs.o euitm_schemas.o copy_structures.o             \
	deallocate_structures.o is_set_structures.o read_structures.o  \
	size_of_structures.o write_structures.o 



# rules ########################################################################
.PHONY: clean


%.o: %.f90
	$(FC90) -o $@ -c $< $(FC90FLAGS)


all: ${LIBDIR}/libual.a


${LIBDIR}/libual.a: ${OBJ}
	ar -r $@ $^ 
	ranlib $@


clean:
	rm -f ${LIBDIR}/libual.a ${INCDIR}/*.mod *.o *~



# dependencies #################################################################
euitm_schemas.o: euitm_ual_defs.o
copy_structures.o: euitm_schemas.o
deallocate_structures.o: euitm_schemas.o
is_set_structures.o: euitm_schemas.o
read_structures.o: euitm_schemas.o
write_structures.o: euitm_schemas.o
