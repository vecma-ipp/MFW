PREFIX=$(HOME)

F90 = mpif90
OPT = -g -traceback  -CB
F90FLAGS = $(OPT) -I${HDF5}/lib -I${HDF5}/include
CC = cc
CFLAGS = -O2
LDFLAGS = $(OPT) -L. -i-dynamic -L${HDF5}/lib
LIBS =  -lfutils -lhdf5_fortran -lhdf5 -lz -lgcc_s -lcrypt -lssl
LIBS =  -lfutils -lhdf5_fortran -lhdf5 -lz

.SUFFIXES:
.SUFFIXES: .o .c .f90

.f90.o:
	$(F90) $(F90FLAGS) -c $<

lib: libfutils.a getfile

getfile: getfile.o
	$(F90) $(LDFLAGS) -o $@ $<  $(LIBS)

libfutils.a: futils.o cutils.o buffer.o
	ar r $@ $?
	ranlib $@

futils.o: append.tpl zappend.tpl \
          putarr.tpl cputarr.tpl putarrnd.tpl cputarrnd.tpl \
          getarr.tpl cgetarr.tpl getarrnd.tpl cgetarrnd.tpl

buffer.o: futils.o

test:
	make -C ../examples test_s test_p

install: debug opt

debug:
	make distclean
	make "OPT = -g -traceback  -CB" libfutils.a
	mkdir -p $(PREFIX)/{lib,include}/g
	cp -p libfutils.a $(PREFIX)/lib/g
	cp -p *.mod $(PREFIX)/include/g

opt:
	make distclean
	make "OPT = -O3 -axPW" libfutils.a
	mkdir -p $(PREFIX)/{lib,include}/O
	cp -p libfutils.a $(PREFIX)/lib/O
	cp -p *.mod $(PREFIX)/include/O

uninstall:
	rm -f $(PREFIX)/include{O,g}/futils.mod
	rm -f $(PREFIX)/include{O,g}/hashtable.mod
	rm -f $(PREFIX)/lib{O,g}/libfutils.a

getfile.o:	libfutils.a

clean:
	rm -f *.o *~ a.out fort.* *.h5*

distclean: clean
	rm -f getfile *.a *.mod TAGS
