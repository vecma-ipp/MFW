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

# default: library

.SECONDARY:

NDIR	= new 

NDIR	+= $(ITM_UAL)

SOLVER	= CG

INDIR	= include 
VPATH	= $(NDIR) $(INDIR) main starts aux plots 
IDIRS	= $(NDIR) $(INDIR)

HDR	= dim.h90 coeff.h90 vars.h90 vconv.h90 

PHDR	= $(HDR) pvars.h90 varsfl.h90 plotins.h90 

PLOTS	= dgdw pgdw pgstat pgspec wtranspdf flxpdf plfxt pfdist zonalphi \
		probe-dist probe-coher probe-cory \
		probe-spec probe-skw probe-skw-lines \
		pldw plsnap plsnap1 plreyn plequil plprof profavg \
		plcuts plcut plcuty \
		plpol plpol1 plpol2 plpolc plshear plderiv \
		plctr plctrc plctr1 \
		wtrans wtranss wtrans2 wtranss2 plsnap2 pldw2 \
		plcorx plcory plcort plcorxt plcoryt \
		phasdist plcoher pladist \
		plcmp plfft

POBJ	= $(PLOTS:%=%.o)

OBJ	= psnaps.o bndys.o metric.o \
		polarise.o neighbors.o delsq2d.o mg2d.o 

OBJ	+= dwaux.o putequil.o

OBJ	+= assign_equil_parameters.o dw.o 

LIB	= libgkmhd.a

AUXOBJ	= psnapsin.o metric.o strip.o bndys.o \
		plotrout.o rcontr.o rcontr2.o 

# AUXOBJ	+= neighbors.o

include	Makefile.actors

FPPFLAGS += -DITMCPOs

ifeq ($(SOLVER),CG)
  NDIR	+= CG
endif

ifeq ($(SOLVER),GAUSS)
  NDIR	+= gauss
endif

# dependencies

plsnap.o: getefl.h90 getespec.h90

metric.o: mset.h90 mprofs.h90

# plot routines, should not be misled by any presence of *.f90

plotrout.f90: plotrout.F90

# the dims header module, to deal with CPOs and preprocessor

dim.h90: dim.F90
	$(FPP) $(FPPFLAGS) $< $@

# combinations

$(MOD): $(HDR)
$(PMOD): $(PHDR)

$(OBJ): $(MOD)
$(POBJ): $(PMOD)

$(PLOTS): %: %.o $(AUXOBJ)
	$(FC90) -o $@ $< $(AUXOBJ) $(PFLAGS)

# foreground targets

PFILE	= pdw.dat

taridls:
	mv *.out idls

PCODES	= plshear 
PCODES	= plequil plpol plprof 


pdw: $(PCODES) 
	rm -f p1.dat
	ln -s $(PFILE) p1.dat
	$(foreach PCODE, $(PCODES), ./$(PCODE); )
	$(MAKE) taridls

