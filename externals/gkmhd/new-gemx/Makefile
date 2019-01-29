#
# gkmhd/Makefile
#
# NB:
# (1) assumes $(SYS) will be set before entry by system
# 
# you do need a Makefile for Makefile.$(SYS), but it can be blank
#        however, the preprocessing rules are unlikely to work!
#        you can use the other Makefile.* files to get an idea of
#        what is expected
#
# First created :	bds	6.6.1998
# Last modification :	bds	21.6.2008
#

default: library

# .SECONDARY:

NDIR	= new3 new2 new1 new0 new 
NDIR	= new-gemx new 

SOLVER	= PETSC

ITM_CPOs = true

INDIR	= include $(HOME)/src $(HOME)/src/plotting 
VPATH	= $(NDIR) $(INDIR) main starts aux plots 
IDIRS	= $(NDIR) $(INDIR)

IFLAGS	= $(foreach IDIR, $(IDIRS), -I$(IDIR) )

HDR	= dim.h90 coeff.h90 vars.h90 vconv.h90 

PHDR	= $(HDR) pvars.h90 varsfl.h90 plotins.h90 

MAINS	= dw testvor
STARTS	= bath blob blobs vortex vbath vflow vflow0 
PLOTS	= dgdw pgdw pgstat pgspec wtranspdf flxpdf plfxt pfdist zonalphi \
		probe-dist probe-coher probe-cory \
		probe-spec probe-skw probe-skw-lines \
		pldw plsnap plsnap1 plreyn plequil plprof profavg \
		plcuts plcut plcuty \
		plpol plpol1 plpol2 plpolc \
		plctr plctrc plctr1 \
		wtrans wtranss wtrans2 wtranss2 plsnap2 pldw2 \
		plcorx plcory plcort plcorxt plcoryt \
		phasdist plcoher pladist \
		plcmp plfft
SNAPS	= snap plsplit plspldp dgsplit plclean dgclean double snapascii

MOBJ	= $(MAINS:%=%.o)
POBJ	= $(PLOTS:%=%.o)
BOBJ	= $(STARTS:%=%.o)
SOBJ	= $(SNAPS:%=%.o)

OBJ	= psnaps.o bndys.o metric.o \
		polarise.o neighbors.o delsq2d.o mg2d.o 

AUXOBJ	= psnapsin.o metric.o strip.o bndys.o \
		plotrout.o rcontr.o rcontr2.o 

AUXOBJ	+= yshift.o 

BAUXOBJ	= metric.o bndys.o tsnaps.o ran1.o 

SAUXOBJ	= plotrout.o psnapsin.o tsnaps.o

# machine dependent stuff, default filenames override

FPPFLAGS = $(IFLAGS) -P -D$(SYS) 

include	Makefile.$(SYS)

DW	= ./dw
TFILE	= tdw.dat
PFILE	= pdw.dat
DFILE	= ddw.dat

ifeq ($(ITM_CPOs),true)
NDIR	+= $(ITM_UAL)
FPPFLAGS += -DITMCPOs
# FFLAGS	+= -fPIC
OBJ	+= assign_equil_parameters.o dw.o 
LFLAGS	= -L. -lgkmhd -L$(ITM_UAL) -litm $(LDFLAGS) 
PFLAGS	+= -L$(ITM_UAL) -litm
DW	= ./wrapper
endif

# dependencies

library: $(OBJ)
	$(ARCHIVE) libgkmhd.a $(OBJ) $(HDR:%.h90=%.o) 
	ranlib libgkmhd.a 

wrapper: library wrapper.o
	$(FC90) -o $@ wrapper.o $(LFLAGS) 

plsnap.o: getefl.h90 getespec.h90

metric.o: mset.h90 mprofs.h90

step.o: visc.h90

# plot routines, should not be misled by any presence of *.f90

plotrout.f90: plotrout.F90

# the dims header module, to deal with CPOs and preprocessor

dim.h90: dim.F90
	$(FPP) $(FPPFLAGS) $< $@

# the FFT module

fft.h90: fft.F90
	$(FPP) $(FPPFLAGS) $< $@

# combinations

AOBJ	= $(MOBJ) $(BOBJ) $(OBJ) $(BAUXOBJ)

APOBJ	= $(POBJ) $(SOBJ) $(AUXOBJ) $(SAUXOBJ)

$(MOD): $(HDR)
$(PMOD): $(PHDR)

$(AOBJ): $(MOD)
$(APOBJ): $(PMOD)

$(MAINS): %: %.o $(OBJ) 
	$(FC90) -o $@ $< $(OBJ) $(LFLAGS)

$(STARTS): %: %.o $(BAUXOBJ)
	$(FC90) -o $@ $< $(BAUXOBJ) $(LFLAGS)

$(PLOTS): %: %.o $(AUXOBJ)
	$(FC90) -o $@ $< $(AUXOBJ) $(PFLAGS)

$(SNAPS): %: %.o $(SAUXOBJ)
	$(FC90) -o $@ $< $(SAUXOBJ) $(PFLAGS)

# foreground targets, including clean

mvcomp:
	mv `find . -type f -perm 755 -size +20 -print` $(RSER)

recomp:
	rm -f *.o *.mod *.M *.[fhFH]90 *.a 
	rm -f `find . -type f -perm 755 -size +20 -print`

clean: recomp
	rm -f idls/*.out
	rm -f GKMHD* gkprofs.dat fout_000 
	rm -f *.log *.out *.ps core cascade crashed stopped
	rm -f ?1.dat [tpd]??.dat

wrap: clean
	- mv h*dat dats
	- mv *jcl jcls

taridls:
	mv *.out idls

PCODES	= dgdw pldw plreyn
PCODES	= dgdw plcorx plcory phasdist plcoher pladist pldw plreyn
PCODES	= dgdw 
PCODES	= dgdw plsnap plcuts plctrc plctr plprof 
PCODES	= plprof
PCODES	= 
PCODES	= plequil plpol plprof 
PCODES	= plpol 


$(PFILE): $(DW) hdw.dat
	if [ ! -f $(PFILE) ] ; then $(DW) ; fi

pdw: $(PFILE) $(PCODES) 
	rm -f d1.dat p1.dat 
	ln -s $(DFILE) d1.dat
	ln -s $(PFILE) p1.dat
	$(foreach PCODE, $(PCODES), ./$(PCODE); )
	$(MAKE) taridls

plotpdw: $(PCODES) $(TCODES) 
#	cp -p $(RSER)/hdw.dat .
	rm -f [dp][1-9].dat [dp][1-9][0-9].dat 
	ln -s $(RSER)/$(DFILE) d1.dat
	ln -s $(RSER)/$(PFILE) p1.dat
	$(foreach PCODE, $(PCODES), ./$(PCODE); )
	rm -f p[1-9].dat p[1-9][0-9].dat 
	ln -s $(RSER)/$(TFILE) p1.dat
	$(foreach TCODE, $(TCODES), ./$(TCODE); )
	$(MAKE) taridls

again: $(DW)
	rm -f stopped cascade crashed idls/*.out *.out *.ps
	rm -f $(DFILE) $(PFILE)
	$(MAKE) pdw

