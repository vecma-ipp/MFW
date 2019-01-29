
OBJCODE ?= $(ITM_INTEL_OBJECTCODE)
INSTALL ?= $(PWD)


include config/${OBJCODE}


UALINC = ${shell eval-pkg-config --cflags ual-${OBJCODE}}
TYPESINC = ${shell eval-pkg-config --cflags itmtypes-${OBJCODE}}
CONSTINC = ${shell eval-pkg-config --cflags itmconstants-${OBJCODE}}
FFTW3INC = ${shell eval-pkg-config --cflags fftw3-${OBJCODE}}


ifeq ($(DEBUG),yes)
  FFLAGS += $(DBGFLAGS) 
else
  FFLAGS += $(OPTFLAGS) 
endif 
FFLAGS += $(UALINC) $(TYPESINC) $(CONSTINC) ${FFTW3INC}


OBJ = obj/phys_constants.o obj/l3interps.o \
      obj/gammax.o obj/besselx.o obj/rans.o obj/fft.o \
      obj/mod_turb.o obj/mod_neo.o 



.PHONY: clean


all: 
	make clean; make OBJCODE=$(ITM_INTEL_OBJECTCODE) install
	make clean; make OBJCODE=$(ITM_GFORTRAN_OBJECTCODE) install
	make clean; make OBJCODE=$(ITM_G95_OBJECTCODE) install


obj/%.o: src/%.f90
	$(FC) $(FFLAGS) -o $@ -c $< 


lib/libbds.a: ${OBJ}
	ar -r $@ $^
	ranlib $@


install: lib/libbds.a
	install -d $(INSTALL)/include/$(OBJCODE)
	install -d $(INSTALL)/lib/$(OBJCODE)
#	install -d $(INSTALL)/src/$(OBJCODE)
	install -m 644 include/*.mod $(INSTALL)/include/$(OBJCODE)
	install -m 644 lib/*.a $(INSTALL)/lib/$(OBJCODE)
#	install -m 644 src/*.f90 $(INSTALL)/src/$(OBJCODE)


clean:
	rm -f obj/*.o include/*.mod lib/*.a *~ 

