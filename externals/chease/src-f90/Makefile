PROG          = chease
PROG_HDF5     = chease_hdf5
PROG_ITM      = chease_itm
PROG_IMAS     = chease_imas
PROG_ITM_HDF5 = chease_itm_hdf5
PROG_KEPLER   = libchease_kepler
PROG_KEPLER_withlapack = libchease_kepler_lapack
PROG_KEPLER_IMAS   = libchease_kepler_imas
PROG_TEST_CHEASE = test_chease
PROG_TEST_CHEASE_ITM = test_chease_itm
PROG_TEST_CHEASE_IMAS = test_chease_imas

# Define variable CHEASE_F90 which will then be used to defined the compilation flags automatically

## these tests worked:
##DOMAINNAME=$(shell echo "$(HOSTNAME)" | cut -c 9-)
##DOMAINNAME=$(findstring efda-itm.eu,$(HOSTNAME))
#ifeq ($(DOMAINNAME),efda-itm.eu)
# CHEASE_F90 = GATEWAY
#endif
## but the easiest is directly:
#ifneq (,$(findstring efda-itm.eu,$(HOSTNAME)))
# CHEASE_F90 = GATEWAY2
#endif

HOSTNAME = $(shell uname -n)

# Defines CHEASE_F90 in Makefile.define_CHEASEF90 and CHEASE_MACHINE in Makefile.define_MACHINE
# if you define: CHEASE_F90=xxx; export CHEASE_F90 (or setenv CHEASE_F90 xxx) in your shell, then this value is used to choose compiler options
# Note: you can define these variables in shell or uncomment the following 2 lines and define them explicitely here:

# CHEASE_F90=ifort
# CHEASE_MACHINE=lac

ifndef CHEASE_F90
 include Makefile.define_CHEASEF90
endif
ifndef CHEASE_MACHINE
 include Makefile.define_MACHINE
endif

# variables to be defined below depending on CHEASE_F90 and CHEASE_MACHINE variables
include Makefile.define_FLAGS

export HDF5 F90 F90FLAGS LDFLAGS LIBS LIBS_HDF5 INCL_HDF5 CCFLAGS

ifeq ($(LIBSlibxml),-lxml2)
	SRCS_0 = chease_new.f90
	SRCS_1 = chease_prog_new.f90
	SRCS_f90_xml_interface = xml_file_reader.f90 xml2eg.f90 assign_chease_codeparameters_choices_xml2eg.f90
	SRCS_c_xml_interface = xml2eg_c.c
	INCLUDE_Clibxml = -I/usr/include/libxml2
else
	SRCS_0 = chease.f90
	SRCS_1 = chease_prog.f90
	SRCS_f90_xml_interface = 
	SRCS_c_xml_interface = 
	INCLUDE_Clibxml = 
endif

# SRCS_0 chease subroutine in chease.f90
#SRCS_0 =	chease.f90
SRCS_0_IMAS =	chease_imas.f90	copy_ids_to_itm_equilibrium.f90 copy_itm_to_ids_equilibrium.f90

SRCS_all =	a_chease.f90 acopy.f90 aldlt.f90 apcoef.f90 apcoef2.f90 \
	assign_chease_codeparameters_reflist.f90 atcoef.f90 auxval.f90 \
	away.f90 ballit.f90 baloon.f90 basis1.f90 basis2.f90 basis3.f90 \
	basis4.f90 blines.f90 bltest.f90 bndspl.f90 bndfit.f90 bound.f90 bsexpeq.f90 \
	bsfunc.f90 bstnzpro.f90 ccopy.f90 center.f90 check.f90 \
	chipsi.f90 chipsimetrics.f90 cint.f90 cocos.f90 conver.f90 copyap.f90 copyapp.f90 \
	copyat.f90 cotrol.f90 cubrt.f90 curent.f90 cvzero.f90 \
	direct.f90 drhodp.f90 dwy.f90 energy.f90 eqchease_mksa.f90 eqdim.f90 erdata.f90 \
	errorch.f90 evlate.f90 four1.f90 fourfft.f90 fourier.f90 g_0.f90 \
	g_1.f90 g_2.f90 g_3.f90 gauss.f90 gchi.f90 gdataext.f90 genout.f90 \
	gijlin.f90 gloadd.f90 globals_init.f90 \
	guess.f90 iarray.f90 identa.f90 identb.f90 indexx.f90 initia.f90 \
	iodisk.f90 isamin.f90 ismax.f90 ismin.f90 isofind.f90 \
	isofun.f90 isrchfge.f90 issum.f90 itipr.f90 ivar.f90 jnovaw.f90 \
	labrun.f90 limita.f90 limitb.f90 ltxw.f90 lyv.f90 magaxe.f90 \
	matrix.f90 mesage.f90 mesh.f90 metrictoitm_afterfpp.f90 msplcy.f90 \
	mspline.f90 nerat.f90 nonlin.f90 norept.f90 ntridg.f90 oarray.f90 \
	oldeq.f90 oldnew.f90 outgload.f90 outmksa.f90 outnvw.f90 outpen.f90 \
	output.f90 outxt.f90 packme.f90 packmep.f90 page.f90 polyfun.f90 polynm.f90 \
	ppbstr.f90 pprime.f90 pprm.f90 ppspln.f90 ppspln2.f90 premap.f90 \
	preset.f90 prfunc.f90 priqqu.f90 prnorm.f90 profile.f90 psibox.f90 \
	psicel.f90 psvol.f90 qplacs.f90 rarray.f90 realft.f90 reseti.f90 \
	resetr.f90 resppr.f90 rmrad.f90 rscale.f90 runtim.f90 rvar.f90 rvar2.f90 \
	rzbound.f90 scopyr.f90 setupa.f90 \
	setupb.f90 shave.f90 smooth.f90 solovev.f90 solvit.f90 sort3.f90 \
	splcy.f90 splcyp.f90 splifft.f90 spline.f90  ssum.f90 \
	stchps.f90 stepon.f90 subsz.f90 surface.f90 surfadd.f90 surfrz.f90 surf_metrics_onaxis.f90 \
	tcase.f90 test.f90 tetare.f90 tpsi.f90 tricyc.f90 tricycm.f90 \
	tridagm.f90 tshift.f90 vacufft.f90 vacuum.f90 vlion.f90 vzero.f90 \
	whtext.f90 witext.f90 wrtext.f90 wrtplot.f90 wrtmat.f90 wrtbin.f90 xtinit.f90 \
	hamada.f90 neoart.f90 outgyro.f90 bscoeff.f90 outelit.f90 outastro.f90 ogyropsi.f90 \
	prof2d_rz_to_fluxtheta.f90 interpos2d_cartesian.f90

SRCS_0_lapackpart = dpgbtrf_s.f90

# hdf5 interface for outputs
SRCS_0_NOHDF5 = write_ogyropsi.f90
SRCS_0_HDF5   =	write_ogyropsi_hdf5.f90

# SRCS_1 chease_prog.f90 program wrapper needed if not in kepler
#SRCS_1 = chease_prog.f90
SRCS_1_IMAS = chease_prog_imas.f90 copy_ids_to_itm_equilibrium.f90 copy_itm_to_ids_equilibrium.f90
SRCS_1_IMAS = chease_prog_imas_ids.f90 

# SRCS_2 get and put routines to read/write to ITM data structure, dummy routines if not needed
SRCS_2     = gloqua.f90 mappin.f90 load_itm_dummy.f90 write_itm_dummy.f90
SRCS_2_ITM = gloqua.f90 mappin.f90 load_itm_with_rout.f90 write_itm_with_rout.f90
SRCS_2_IMAS = gloqua_imas.f90 mappin_imas.f90 load_imas_with_rout.f90 write_imas_with_rout.f90
#SRCS_2_IMAS = load_imas_dummy.f90 write_imas_dummy.f90

# SRCS_interpos to make interpos library
# from: 
# svn export http://crppsvn.epfl.ch/repos/interpos/trunk/interpos_libs interpos_libs --force
#
# DIR_interpos not used at this stage, insert dummy
DIR_interpos = /tmp
#DIR_interpos = /afs/efda-itm.eu/user/s/sauter/public/interpos/interpos_libs/pgf95
#DIR_interpos = /afs/efda-itm.eu/user/s/sauter/public/interpos_develop_new/interpos_libs/pgf95
MODS_interpos =	interpos_source.f90

# For various targets:
# chease (for backward compatibilty includes lapack part when using the program SRCS_1)
SRCS		= $(SRCS_1) $(SRCS_0) $(SRCS_all) $(SRCS_0_NOHDF5) $(SRCS_2) $(SRCS_0_lapackpart)
SRCS_f90	= $(SRCS_f90_xml_interface) $(SRCS_1) $(SRCS_0) $(SRCS_all) $(SRCS_0_NOHDF5) $(SRCS_2) $(SRCS_0_lapackpart)
SRCS_c		= $(SRCS_c_xml_interface)
# chease_HDF5
SRCS_HDF5	= $(SRCS_1) $(SRCS_0) $(SRCS_all) $(SRCS_0_HDF5) $(SRCS_2) $(SRCS_0_lapackpart)
# chease_ITM
SRCS_ITM	= $(SRCS_1) $(SRCS_0) $(SRCS_all) $(SRCS_0_NOHDF5) $(SRCS_2_ITM) $(SRCS_0_lapackpart)
SRCS_ITM_f90	=  $(SRCS_f90_xml_interface) $(SRCS_1) $(SRCS_0) $(SRCS_all) $(SRCS_0_NOHDF5) $(SRCS_2_ITM)
SRCS_ITM_c	= $(SRCS_c_xml_interface)
# chease_IMAS
SRCS_IMAS	= $(SRCS_1_IMAS) $(SRCS_0_IMAS) $(SRCS_all) $(SRCS_0_NOHDF5) $(SRCS_2_IMAS) $(SRCS_0_lapackpart)
# chease_ITM_HDF5
SRCS_ITM_HDF5	= $(SRCS_1) $(SRCS_0) $(SRCS_all) $(SRCS_0_HDF5) $(SRCS_2_ITM) $(SRCS_0_lapackpart)
# libchease_kepler
SRCS_KEPLER	= $(SRCS_0) $(SRCS_all) $(SRCS_0_NOHDF5) $(SRCS_2_ITM)
SRCS_KEPLER_f90	= $(SRCS_f90_xml_interface) $(SRCS_0) $(SRCS_all) $(SRCS_0_NOHDF5) $(SRCS_2_ITM)
SRCS_KEPLER_c	= $(SRCS_c_xml_interface)
SRCS_KEPLER_withlapack	= $(SRCS_0) $(SRCS_all) $(SRCS_0_NOHDF5) $(SRCS_2_ITM) $(SRCS_0_lapackpart)
# libchease_kepler_imas
SRCS_KEPLER_IMAS	= $(SRCS_0_IMAS) $(SRCS_all) $(SRCS_0_NOHDF5) $(SRCS_2_IMAS) $(SRCS_0_lapackpart)

#INCS =	BNDIND.inc COMDAT.inc HERMIT.inc SOLOV.inc mdslib.inc
INCS =	BNDIND.inc COMDAT.inc HERMIT.inc SOLOV.inc

# make ln -s euitm_schemas_407b.f90 euitm_schemas.f90 before, not needed for euitm_routines.f90
MODS     = itm_types.f90 euitm_schemas.f90 neobscoeffmod.f90 globals.f90 interpol.f90 prec_const.f90 \
	   sigmaneomod.f90 string_manipulation_tools.f90 euitm_xml_parser.f90
MODS_ITM = itm_types.f90 neobscoeffmod.f90 globals.f90 interpol.f90 prec_const.f90 \
	   sigmaneomod.f90 string_manipulation_tools.f90 euitm_xml_parser.f90
MODS_IMAS = itm_types.f90 euitm_schemas.f90 neobscoeffmod.f90 globals.f90 interpol.f90 prec_const.f90 \
	   sigmaneomod.f90 string_manipulation_tools.f90 euitm_xml_parser.f90 itm_ids_utilities.f90

#OBJS     = $(MODS:.f90=.o) $(MODS_interpos:.f90=.o) $(SRCS:.f90=.o)
OBJS     = $(SRCS_c:.c=.o) $(MODS:.f90=.o) $(MODS_interpos:.f90=.o) $(SRCS_f90:.f90=.o)
OBJS_HDF5     = $(MODS:.f90=.o) $(MODS_interpos:.f90=.o) $(SRCS_HDF5:.f90=.o)
OBJS_ITM = $(SRCS_ITM_c:.c=.o) $(MODS_ITM:.f90=.o) $(MODS_interpos:.f90=.o) $(SRCS_ITM_f90:.f90=.o)
OBJS_IMAS = $(MODS_IMAS:.f90=.o) $(MODS_interpos:.f90=.o) $(SRCS_IMAS:.f90=.o)
OBJS_ITM_HDF5 = $(MODS_ITM:.f90=.o) $(MODS_interpos:.f90=.o) $(SRCS_ITM_HDF5:.f90=.o)
# for KEPLER should not include the libinterpos since otherwise might be included twice. Should also not have lapack
OBJS_KEPLER = $(MODS_ITM:.f90=.o) $(MODS_interpos:.f90=.o) $(SRCS_KEPLER_f90:.f90=.o) $(SRCS_KEPLER_c:.c=.o)
OBJS_KEPLER_withlapack = $(MODS_ITM:.f90=.o) $(MODS_interpos:.f90=.o) $(SRCS_KEPLER_withlapack:.f90=.o)
OBJS_KEPLER_IMAS = $(MODS_IMAS:.f90=.o) $(MODS_interpos:.f90=.o) $(SRCS_KEPLER_IMAS:.f90=.o)

.PRECIOUS:	$(SRCS) $(SRCS_1) $(SRCS_IMAS) $(INCS) $(MODS) $(MODS_ITM) $(MODS_interpos) $(MODS_IMAS) 


# 
VERSION=V90_12_1_`svnversion -n .. |  sed 's/.*://'`
#FPPFLAGS+=-DSVNVERSION="\"`svnversion .. | sed 's/.*://'`\""
SVNREVISION="`svnversion .. | sed 's/.*://'`"

# could be useful:
##ifeq (exists, $(shell [ -d directory_name ] ) && echo exists )
##$(warning hi... I did it)
##endif
#

.PHONY: checkCHEASE_F90 checkCHEASE_MACHINE

all: $(PROG)

$(PROG): checkCHEASE_F90 checkCHEASE_MACHINE $(OBJS)
# because of recursive call in euitm_xml_parser.f90 using pointers, 
# need compilation without automatic deallocation at this stage at least (June 2008)
#	@echo $(VERSION)
#	@echo "CHEASE_MACHINE: $(CHEASE_MACHINE)"
	$(F90) $(F90FLAGS_parser) -c euitm_xml_parser.f90
	$(F90) $(F90FLAGS_parser) -c assign_chease_codeparameters_reflist.f90
	$(F90) $(F90FLAGS_parser_nor8) -c xml2eg.f90
	$(F90) $(LDFLAGS) -o $@ $(OBJS) $(LIBS) $(LIBSlibxml)

$(PROG_HDF5): checkCHEASE_F90 checkCHEASE_MACHINE $(FUTILS)/lib$(futils).a $(OBJS_HDF5)
# because of recursive call in euitm_xml_parser.f90 using pointers, 
# need compilation without automatic deallocation at this stage at least (June 2008)
	$(F90) $(F90FLAGS_parser) -c euitm_xml_parser.f90
	$(F90) $(F90FLAGS_parser) -c assign_chease_codeparameters_reflist.f90
	$(F90) $(LDFLAGS) -o $@ $(OBJS_HDF5) $(LIBS) $(LIBS_HDF5)

$(PROG_ITM): checkCHEASE_F90 checkCHEASE_MACHINE $(OBJS_ITM)
# so far chease_ITM can only be compiled on gateway, on which no need for special compilation for parser at this stage
	$(F90) $(F90FLAGS_parser) -c euitm_xml_parser.f90
	$(F90) $(F90FLAGS_parser) -c assign_chease_codeparameters_reflist.f90
	$(F90) $(LDFLAGS) -o $@ $(OBJS_ITM) $(LIBS)  $(LIBSlibxml) $(OBJS_euitm)

$(PROG_IMAS): checkCHEASE_F90 checkCHEASE_MACHINE $(OBJS_IMAS)
# so far chease_IMAS can only be compiled on hpc-login4, on which no need for special compilation for parser at this stage
	$(F90) $(LDFLAGS) -o $@ $(OBJS_IMAS) $(LIBS) $(OBJS_IMASlocal)

$(PROG_ITM_HDF5): checkCHEASE_F90 checkCHEASE_MACHINE $(FUTILS)/lib$(futils).a $(OBJS_ITM_HDF5)
# so far chease_ITM can only be compiled on gateway, on which no need for special compilation for parser at this stage
	$(F90) $(LDFLAGS) -o $@ $(OBJS_ITM_HDF5) $(LIBS) $(LIBS_HDF5) $(OBJS_euitm)

$(PROG_KEPLER): checkCHEASE_F90 checkCHEASE_MACHINE $(OBJS_KEPLER)
# so far chease_ITM can only be compiled on gateway, on which no need for special compilation for parser at this stage
	@echo "CHEASE_F90: $(CHEASE_F90)"
	rm -f $(PROG_KEPLER).a
	@ar -rv $(PROG_KEPLER).a $(OBJS_KEPLER) $(OBJS_euitm)

$(PROG_KEPLER_withlapack): checkCHEASE_F90 checkCHEASE_MACHINE $(OBJS_KEPLER_withlapack)
# so far chease_ITM can only be compiled on gateway, on which no need for special compilation for parser at this stage
	@echo "CHEASE_F90: $(CHEASE_F90)"
	rm -f $(PROG_KEPLER_withlapack).a
	@ar -rv $(PROG_KEPLER_withlapack).a $(OBJS_KEPLER_withlapack) $(OBJS_euitm)

$(PROG_KEPLER_IMAS): checkCHEASE_F90 checkCHEASE_MACHINE $(OBJS_KEPLER_IMAS)
# so far chease_ITM can only be compiled on gateway, on which no need for special compilation for parser at this stage
#	@echo "CHEASE_F90: $(CHEASE_F90)"
#	@echo "$(OBJS_KEPLER_IMAS)"
	rm -f $(PROG_KEPLER_IMAS).a
	@ar -rv $(PROG_KEPLER_IMAS).a $(OBJS_KEPLER_IMAS) $(OBJS_IMASlocal)

$(FUTILS)/lib$(futils).a : 
	# cp $(FUTILS)/$(Makefile_futils) $(FUTILS)/Makefile
	@cd $(FUTILS); \
	make -f $(Makefile_futils)

$(PROG_TEST_CHEASE): 
	../WK/TESTCASES/NIDEAL10/script_test_chease_from_src-f90

$(PROG_TEST_CHEASE_ITM): 
	../WK/TESTCASES/NIDEAL10/script_test_chease_itm_from_src-f90

$(PROG_TEST_CHEASE_IMAS): 
	../WK/TESTCASES/NIDEAL10/script_test_chease_imas_from_src-f90

# Here are make targets that are called by continuous integration service (Bamboo) which should run tests for CHEASE
# with a stringency according to release, develop or feature branch.
# FIXME: these are place holders for the developer to provide actual tests. The return code of make will flag a pass or a fail.
test-release:
	# insert code here
	@echo "Release-branch tests passed."

test-develop:
	# insert code here
	@echo "Develop-branch tests passed."

test-feature:
	# insert code here
	@echo "Feature-branch tests passed."

segldr: 
	$(F90) $(LDFLAGS) -o chease $(OBJS) $(LIBS)

clean: checkCHEASE_F90 checkCHEASE_MACHINE
	rm -f  *.a $(PROG) $(PROG_HDF5) $(PROG_ITM) $(PROG_IMAS) $(PROG_ITM_HDF5) $(PROG_KEPLER) $(PROG_KEPLER_IMAS) *.kmo *.mod *.M *.o $(FUTILS)/*.o $(FUTILS)/*.mod $(FUTILS)/*.a

tar:
	gtar zcvf ../$(PROG).tar.gz $(SRCS) $(INCS) $(MODS) Makefile

source:	metrictoitm_afterfpp.f90
	cat $(MODS) $(MODS_interpos) $(INCS) $(SRCS) > ../chease_$(VERSION).f90

source_itm:	metrictoitm_afterfpp.f90
	cat $(MODS_ITM) $(MODS_interpos) $(INCS) $(SRCS_ITM) > ../chease_itm_$(VERSION).f90

source_imas:	metrictoitm_afterfpp.f90
	cat $(MODS_IMAS) $(MODS_interpos) $(INCS) $(SRCS_IMAS) > ../chease_imas_$(VERSION).f90

checkCHEASE_F90:
ifndef CHEASE_F90
	echo "CHEASE_F90 not defined"
	exit 2
else
	@echo "CHEASE_F90 defined to: $(CHEASE_F90)"
endif

checkCHEASE_MACHINE:
ifndef CHEASE_MACHINE
	echo "CHEASE_MACHINE not defined"
	exit 2
else
	@echo "CHEASE_MACHINE defined to: $(CHEASE_MACHINE)"
endif


.SUFFIXES: $(SUFFIXES) .f90

metrictoitm_afterfpp.f90: metrictoitm.f90
#	@echo $(FPPFLAGS)
	@echo $(SVNREVISION)
# use sed instead of fpp or cpp since cpp removes concatenation // characters and fpp not always present
	sed "s/SVNREVISION/\"$(SVNREVISION)\"/" metrictoitm.f90 > metrictoitm_afterfpp.f90
# use cpp instead of fpp since seems more generic on various platforms
#	@fpp $(FPPFLAGS) metrictoitm.f90 metrictoitm_afterfpp.f90

.f90.o:
	$(F90) $(F90FLAGS) -c $< $(INCL_FUTILS) $(INCL_HDF5)

.f90.mod:
	@touch $*.mod
#	$(F90) $(F90FLAGS) -c $<

.c.o:
	$(CC) $(CCFLAGS) -c $< $(INCLUDE_Clibxml)

.mod.o:
	$(F90) $(F90FLAGS) -c $*.f90

acopy.o: globals.o
aldlt.o: prec_const.o
apcoef.o: globals.o
apcoef2.o: globals.o
#assign_code_parameters.o: prec_const.o euitm_schemas.o euitm_xml_parser.o globals.o
assign_chease_codeparameters_reflist.o: prec_const.o euitm_xml_parser.o globals.o
assign_chease_codeparameters_choices_xml2eg.o: prec_const.o globals.o xml2eg.o
xml2eg.o: xml2eg.f90
	$(F90) $(F90FLAGS_parser_nor8) -c xml2eg.f90
atcoef.o: globals.o
auxval.o: globals.o
away.o: globals.o BNDIND.inc
ballit.o: globals.o
baloon.o: globals.o
basis1.o: globals.o HERMIT.inc
basis2.o: globals.o HERMIT.inc
basis3.o: globals.o HERMIT.inc
basis4.o: globals.o HERMIT.inc
blines.o: globals.o
bltest.o: globals.o interpol.o
bndspl.o: globals.o
bndfit.o: prec_const.o interpos_source.o
bound.o: globals.o
bscoeff.o: globals.o neobscoeffmod.o
bsexpeq.o: globals.o
bsfunc.o: globals.o interpol.o
bstnzpro.o: globals.o
ccopy.o: globals.o
center.o: globals.o
check.o: globals.o BNDIND.inc
chease.o: globals.o
chipsi.o: globals.o
chipsimetrics.o: globals.o
cint.o: globals.o
cocos.o: 
conver.o: globals.o
copyap.o: globals.o interpol.o
copyapp.o: globals.o interpol.o
copyat.o: globals.o interpol.o
cotrol.o: globals.o
cubrt.o: globals.o
curent.o: globals.o
cvzero.o: globals.o
direct.o: globals.o
dpgbtrf_s.o: prec_const.o
drhodp.o: globals.o
dwy.o: prec_const.o
energy.o: globals.o
eqchease_mksa.o: prec_const.o globals.o
eqdim.o: globals.o
erdata.o: globals.o
errorch.o: globals.o
chease_prog.o: SOLOV.inc COMDAT.inc
#euitm_schemas.o: 
#euitm_routines.o: euitm_schemas.o
euitm_xml_parser.o: itm_types.o string_manipulation_tools.o
#euitm_utilities.o: euitm_schemas.o
evlate.o: globals.o
four1.o: prec_const.o
fourfft.o: globals.o
fourier.o: globals.o
g_0.o: globals.o
g_1.o: globals.o
g_2.o: globals.o
g_3.o: globals.o
gauss.o: globals.o
gchi.o: globals.o
gdataext.o: prec_const.o
genout.o: prec_const.o
gijlin.o: globals.o
gloadd.o: globals.o interpol.o interpos_source.o
globals.o: prec_const.o
globals_init.o: globals.o
gloqua.o: globals.o interpol.o interpos_source.o
gloqua_imas.o: globals.o interpol.o interpos_source.o
guess.o: globals.o interpol.o
iarray.o: globals.o
identa.o: globals.o BNDIND.inc
identb.o: globals.o
indexx.o: prec_const.o
initia.o: globals.o
interpol.o: prec_const.o
interpos2d_cartesian.o: itm_types.o interpos_source.o
iodisk.o: globals.o interpol.o COMDAT.inc interpos_source.o
isamin.o: globals.o
ismax.o: globals.o
ismin.o: globals.o
isofind.o: globals.o
isofun.o: globals.o
isrchfge.o: globals.o
issum.o: globals.o
itipr.o: globals.o
ivar.o: globals.o
jnovaw.o: globals.o
labrun.o: globals.o
limita.o: globals.o
limitb.o: globals.o
load_itm_dummy.o: globals.o
load_itm_with_rout.o: globals.o
ltxw.o: prec_const.o
lyv.o: prec_const.o
magaxe.o: globals.o
mappin.o: globals.o interpol.o sigmaneomod.o
mappin_imas.o: globals.o interpol.o sigmaneomod.o
matrix.o: globals.o
mesage.o: globals.o
mesh.o: globals.o
metrictoitm_afterfpp.o: globals.o interpos_source.o
msplcy.o: globals.o
mspline.o: globals.o
nerat.o: globals.o interpol.o
nonlin.o: globals.o
norept.o: globals.o interpol.o
ntridg.o: globals.o
oarray.o: globals.o
ogyropsi.o: globals.o
oldeq.o: globals.o
oldnew.o: globals.o
outgload.o: globals.o interpol.o
outmetric.o: globals.o
outmksa.o: globals.o
outnvw.o: globals.o interpol.o
outpen.o: globals.o
output.o: globals.o interpol.o COMDAT.inc interpos_source.o
outxt.o: globals.o
packme.o: globals.o interpol.o
packmep.o: globals.o
page.o: globals.o
polyfun.o: globals.o
polynm.o: globals.o interpol.o
ppbstr.o: globals.o interpol.o
pprime.o: globals.o interpol.o interpos_source.o
pprm.o: globals.o
ppspln.o: globals.o prec_const.o
ppspln2.o: prec_const.o
premap.o: globals.o interpol.o
preset.o: globals.o
prfunc.o: globals.o
priqqu.o: globals.o
prnorm.o: globals.o
profile.o: globals.o interpol.o
prof2d_rz_to_fluxtheta.o: itm_types.o interpos_source.o
psibox.o: globals.o interpol.o interpos_source.o
psicel.o: globals.o
psvol.o: globals.o interpol.o
qplacs.o: globals.o interpol.o
rarray.o: globals.o
realft.o: prec_const.o
reseti.o: globals.o
resetr.o: globals.o
resppr.o: globals.o
rmrad.o: globals.o
rscale.o: globals.o
rvar.o: globals.o
rvar2.o: globals.o
rzbound.o: globals.o
runtim.o: prec_const.o
scopyr.o: prec_const.o
setupa.o: globals.o
setupb.o: globals.o
shave.o: globals.o
smooth.o: globals.o
solovev.o: globals.o SOLOV.inc
solvit.o: globals.o
sort3.o: prec_const.o
splcy.o: globals.o
splcyp.o: globals.o
splifft.o: prec_const.o
spline.o: prec_const.o interpol.o
ssum.o: globals.o
stchps.o: globals.o
stepon.o: globals.o interpol.o interpos_source.o
string_manipulation_tools.o: prec_const.o
subsz.o: globals.o
surface.o: globals.o interpol.o sigmaneomod.o interpos_source.o
surf_metrics_onaxis.o : globals.o interpol.o interpos_source.o
surfadd.o: globals.o interpol.o
surfrz.o: globals.o
tcase.o: globals.o
test.o: globals.o SOLOV.inc
tetare.o: globals.o
tpsi.o: globals.o
tricyc.o: prec_const.o
tricycm.o: prec_const.o
tridagm.o: prec_const.o
tshift.o: globals.o
vacufft.o: globals.o
vacuum.o: globals.o
vlion.o: globals.o
vzero.o: prec_const.o
write_itm_dummy.o: globals.o
write_itm_with_rout.o: globals.o
write_ogyropsi.o: prec_const.o
write_ogyropsi_hdf5.o: prec_const.o $(FUTILS)/lib$(futils).a
wrtext.o: globals.o
wrtplot.o: globals.o interpol.o COMDAT.inc
wrtbin.o: globals.o interpol.o COMDAT.inc interpos_source.o
wrtasc.o: globals.o interpol.o COMDAT.inc
wrtmat.o: globals.o interpol.o COMDAT.inc
xtinit.o: globals.o
outgyro.o: globals.o interpol.o
outelit.o: globals.o interpos_source.o
outastro.o: globals.o interpol.o
outgyropsi.o: globals.o
outgyropsi_hdf5.o:globals.o
hamada.o: globals.o interpol.o interpos_source.o
neoart.o: globals.o interpos_source.o
interpos_source.o: prec_const.o
