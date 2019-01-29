#/* -*- makefile -*- */


# paths ########################################################################
SRCDIR = 
LIBDIR = ../lib/${SYS}/xml
INCDIR = ../include/${SYS}


# set up the VPATH for searching for source files ##############################
VPATH = ${SRCDIR}


# additional flags #############################################################
INCLUDES = -I${INCDIR}/types -I${INCDIR}/fortran_interface -I${INCDIR}/xml  
FC90FLAGS += ${INCLUDES} ${MODSPEC}${INCDIR}/xml 


# objects files ################################################################
OBJ = 	string_manipulation_tools.o euitm_xml_parser.o xml_file_reader.o \
	xml_tools.o xml2eg_c.o xml2eg.o xml_path.o xmllib_pathquery.o 


# rules ########################################################################
.PHONY: clean


%.o: %.f90
	$(FC90) -o $@ -c $< $(FC90FLAGS)


%.o: %.c
	$(CC) -c -o $@ $^ $(CCFLAGS) -I/usr/include/libxml2 


all: ${LIBDIR}/libxmllib.a


${LIBDIR}/libxmllib.a: ${OBJ}
	ar -r $@ $^ 
	ranlib $@


clean:
	rm -f ${LIBDIR}/libxmllib.a ${INCDIR}/xml/*.mod *.o *~



