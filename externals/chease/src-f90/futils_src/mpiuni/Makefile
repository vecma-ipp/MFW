HDF5=/usr/local/hdf5_serial

FC = ifort
FFLAGS = -g -CB -traceback 
F90 = $(FC)
F90FLAGS = $(FFLAGS)
CC = icc
CFLAGS = -O2 -w

.SUFFIXES: .o .c .f90

.f90.o:
	$(F90) $(F90FLAGS) -c $<

lib: libmpiuni.a

libmpiuni.a: mpi_stubsf.o second.o
	ar r $@ $?
	ranlib $@
	cp -a $@ ../

clean:
	rm -f *.o *~ a.out
distclean: clean
	rm -f *.a
