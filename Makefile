################################################################################
# system choice (specific compilers/path/options in config file)               #
################################################################################

SHELL:=/bin/bash

export SYS ?= DEFAULT

export INC_BDS = ${PWD}/externals/libbds/include/${SYS}
export LIB_BDS = ${PWD}/externals/libbds/lib/${SYS}

export INC_UAL = ${PWD}/ual/include/${SYS}/fortran_interface
export LIB_UAL = ${PWD}/ual/lib/${SYS}/fortran_interface

export INC_TYPES = ${PWD}/ual/include/${SYS}/types
export LIB_TYPES = ${PWD}/ual/lib/${SYS}/types

export INC_CONSTANTS = ${PWD}/ual/include/${SYS}/constants
export LIB_CONSTANTS = ${PWD}/ual/lib/${SYS}/constants

export INC_XML = ${PWD}/ual/include/${SYS}/xml
export LIB_XML = ${PWD}/ual/lib/${SYS}/xml

export DATAVERSION = 4.10b.10

include config

export MAINMAKE=yes



################################################################################
# global rules                                                                 #
################################################################################
#.SUFFIXES:
#.SUFFIXES: obj/.o .f90 .F90

.PHONY: clean all ual libbds codes 




# whole project ################################################################
all: kernels  
	@echo -e "\033[35m\033[1m ==== Full Build Completed ==== \033[0m"


codes: libbds bdseq bohmgb ets gem chease dfefi imp4dv gem0 
#orb5


kernels: ual codes
	@echo -e "\033[36m\033[1m ++++ Build MUSCLE kernels ++++ \033[0m"; \
	($(MAKE) --no-print-directory -C kernels \
	&& echo -e "\033[32m\033[1m -- OK -- \033[0m") \
	|| echo -e "\033[31m\033[1m -- FAIL -- \033[0m"

clean-kernels:
	@echo -e "\033[36m\033[1m ++++ Clean MUSCLE kernels ++++ \033[0m"; \
	($(MAKE) --no-print-directory -C kernels clean \
	&& echo -e "\033[32m\033[1m -- OK -- \033[0m") \
	|| echo -e "\033[31m\033[1m -- FAIL -- \033[0m"


standalone: ual ets imp4dv chease
	@echo -e "\033[36m\033[1m ++++ Build standalone wrapper/program ++++ \033[0m"; \
	($(MAKE) --no-print-directory -C standalone \
	&& echo -e "\033[32m\033[1m -- OK -- \033[0m") \
	|| echo -e "\033[31m\033[1m -- FAIL -- \033[0m"

clean-standalone:
	@echo -e "\033[36m\033[1m ++++ Clean standalone wrapper/program ++++ \033[0m"; \
	($(MAKE) --no-print-directory -C standalone clean \
	&& echo -e "\033[32m\033[1m -- OK -- \033[0m") \
	|| echo -e "\033[31m\033[1m -- FAIL -- \033[0m"




# cleaning #####################################################################
clean: clean-kernels clean-ual clean-libbds clean-ets clean-bohmgb \
	clean-bdseq clean-gem clean-chease clean-dfefi clean-imp4dv \
	clean-gem0 clean-standalone
#clean-orb5
	rm -f *~
	@echo -e "\033[35m\033[1m ==== Full Clean Completed ==== \033[0m"




################################################################################
# code specific rules                                                          #
################################################################################


# ual+tools ####################################################################
ual:
	@echo -e "\033[36m\033[1m ++++ Build UAL and TOOLS ++++ \033[0m"; \
	($(MAKE) --no-print-directory -C ual \
	&& echo -e "\033[32m\033[1m -- OK -- \033[0m") \
	|| echo -e "\033[31m\033[1m -- FAIL -- \033[0m"

clean-ual:
	@echo -e "\033[36m\033[1m ++++ Clean UAL and TOOLS ++++ \033[0m"; \
	($(MAKE) --no-print-directory -C ual clean \
	&& echo -e "\033[32m\033[1m -- OK -- \033[0m") \
	|| echo -e "\033[31m\033[1m -- FAIL -- \033[0m"



# ets ##########################################################################
ets: ual libbds
	@echo -e "\033[36m\033[1m ++++ Build ETS ++++ \033[0m"; \
	($(MAKE) --no-print-directory -C externals -f Makefile.ets \
	&& echo -e "\033[32m\033[1m -- OK -- \033[0m") \
	|| echo -e "\033[31m\033[1m -- FAIL -- \033[0m"

clean-ets:
	@echo -e "\033[36m\033[1m ++++ Clean ETS ++++ \033[0m"; \
	($(MAKE) --no-print-directory -C externals -f Makefile.ets clean \
	&& echo -e "\033[32m\033[1m -- OK -- \033[0m") \
	|| echo -e "\033[31m\033[1m -- FAIL -- \033[0m"



# libbds #######################################################################
libbds: ual
	@echo -e "\033[36m\033[1m ++++ Build LIBBDS ++++ \033[0m"; \
	($(MAKE) --no-print-directory -C externals -f Makefile.libbds \
	&& echo -e "\033[32m\033[1m -- OK -- \033[0m") \
	|| echo -e "\033[31m\033[1m -- FAIL -- \033[0m"

clean-libbds:
	@echo -e "\033[36m\033[1m ++++ Clean LIBBDS ++++ \033[0m"; \
	($(MAKE) --no-print-directory -C externals -f Makefile.libbds clean \
	&& echo -e "\033[32m\033[1m -- OK -- \033[0m") \
	|| echo -e "\033[31m\033[1m -- FAIL -- \033[0m"



# bohmgb #######################################################################
bohmgb: ual libbds
	@echo -e "\033[36m\033[1m ++++ Build BOHMGB ++++ \033[0m"; \
	($(MAKE) --no-print-directory -C externals -f Makefile.bohmgb \
	&& echo -e "\033[32m\033[1m -- OK -- \033[0m") \
	|| echo -e "\033[31m\033[1m -- FAIL -- \033[0m"

clean-bohmgb:
	@echo -e "\033[36m\033[1m ++++ Clean BOHMGB ++++ \033[0m"; \
	($(MAKE) --no-print-directory -C externals -f Makefile.bohmgb clean \
	&& echo -e "\033[32m\033[1m -- OK -- \033[0m") \
	|| echo -e "\033[31m\033[1m -- FAIL -- \033[0m"



# gem0 #########################################################################
gem0: ual libbds
	@echo -e "\033[36m\033[1m ++++ Build GEM0 ++++ \033[0m"; \
	($(MAKE) --no-print-directory -C externals -f Makefile.gem0 \
	&& echo -e "\033[32m\033[1m -- OK -- \033[0m") \
	|| echo -e "\033[31m\033[1m -- FAIL -- \033[0m"

clean-gem0:
	@echo -e "\033[36m\033[1m ++++ Clean GEM0 ++++ \033[0m"; \
	($(MAKE) --no-print-directory -C externals -f Makefile.gem0 clean \
	&& echo -e "\033[32m\033[1m -- OK -- \033[0m") \
	|| echo -e "\033[31m\033[1m -- FAIL -- \033[0m"



# bdseq ########################################################################
bdseq: ual libbds
	@echo -e "\033[36m\033[1m ++++ Build BDSEQ ++++ \033[0m"; \
	($(MAKE) --no-print-directory -C externals -f Makefile.bdseq \
	&& echo -e "\033[32m\033[1m -- OK -- \033[0m") \
	|| echo -e "\033[31m\033[1m -- FAIL -- \033[0m"

clean-bdseq:
	@echo -e "\033[36m\033[1m ++++ Clean BDSEQ ++++ \033[0m"; \
	($(MAKE) --no-print-directory -C externals -f Makefile.bdseq clean \
	&& echo -e "\033[32m\033[1m -- OK -- \033[0m") \
	|| echo -e "\033[31m\033[1m -- FAIL -- \033[0m"



# gem (version from IPP repos) #################################################
# temporary patch to avoid memory leak, should be fixed in copy_cpo instead
patch-gem: 
	@grep -q Deallocate externals/gem/actor/itm_cpos.h90 \
	&& echo -e "\033[32m\033[1m -- ALREADY PATCHED -- \033[0m" \
	|| (echo -e "\033[36m\033[1m ++++ Patch GEM sources ++++ \033[0m"; \
	(patch -p0 -i externals/gem-tmppatch.diff \
	&& echo -e "\033[32m\033[1m -- OK -- \033[0m") \
	|| echo -e "\033[31m\033[1m -- FAIL -- \033[0m")

revert-gem:
	@echo -e "\033[36m\033[1m ++++ Revert GEM sources ++++ \033[0m"; \
	(svn revert externals/gem/actor/* \
	&& echo -e "\033[32m\033[1m -- OK -- \033[0m") \
	|| echo -e "\033[31m\033[1m -- FAIL -- \033[0m"

gem: ual libbds patch-gem
	@echo -e "\033[36m\033[1m ++++ Build GEM ++++ \033[0m"; \
	($(MAKE) --no-print-directory -C externals -f Makefile.gem \
	&& echo -e "\033[32m\033[1m -- OK -- \033[0m") \
	|| echo -e "\033[31m\033[1m -- FAIL -- \033[0m"

clean-gem: revert-gem
	@echo -e "\033[36m\033[1m ++++ Clean GEM ++++ \033[0m"; \
	($(MAKE) --no-print-directory -C externals -f Makefile.gem clean \
	&& echo -e "\033[32m\033[1m -- OK -- \033[0m") \
	|| echo -e "\033[31m\033[1m -- FAIL -- \033[0m"



# dfefi (version from IPP repos) ###############################################
dfefi: ual libbds
	@echo -e "\033[36m\033[1m ++++ Build dFEFI ++++ \033[0m"; \
	($(MAKE) --no-print-directory -C externals -f Makefile.dfefi \
	&& echo -e "\033[32m\033[1m -- OK -- \033[0m") \
	|| echo -e "\033[31m\033[1m -- FAIL -- \033[0m"

clean-dfefi:
	@echo -e "\033[36m\033[1m ++++ Clean dFEFI ++++ \033[0m"; \
	($(MAKE) --no-print-directory -C externals -f Makefile.dfefi clean \
	&& echo -e "\033[32m\033[1m -- OK -- \033[0m") \
	|| echo -e "\033[31m\033[1m -- FAIL -- \033[0m"



# imp4dv #######################################################################
imp4dv: ual libbds
	@echo -e "\033[36m\033[1m ++++ Build IMP4DV ++++ \033[0m"; \
	($(MAKE) --no-print-directory -C externals -f Makefile.imp4dv \
	&& echo -e "\033[32m\033[1m -- OK -- \033[0m") \
	|| echo -e "\033[31m\033[1m -- FAIL -- \033[0m"

clean-imp4dv:
	@echo -e "\033[36m\033[1m ++++ Clean IMP4DV ++++ \033[0m"; \
	($(MAKE) --no-print-directory -C externals -f Makefile.imp4dv clean \
	&& echo -e "\033[32m\033[1m -- OK -- \033[0m") \
	|| echo -e "\033[31m\033[1m -- FAIL -- \033[0m"



# chease #######################################################################
patch-chease: 
	@grep -q MUSCLE externals/chease/src-f90/Makefile \
	&& echo -e "\033[32m\033[1m -- ALREADY PATCHED -- \033[0m" \
	|| (echo -e "\033[36m\033[1m ++++ Patch CHEASE makefiles ++++ \033[0m"; \
	(patch -p0 -i externals/chease-patch.diff \
	&& echo -e "\033[32m\033[1m -- OK -- \033[0m") \
	|| echo -e "\033[31m\033[1m -- FAIL -- \033[0m")

revert-chease:
	@echo -e "\033[36m\033[1m ++++ Revert CHEASE makefiles ++++ \033[0m"; \
	(svn revert externals/chease/src-f90/Make* \
	&& echo -e "\033[32m\033[1m -- OK -- \033[0m") \
	|| echo -e "\033[31m\033[1m -- FAIL -- \033[0m"

chease: ual patch-chease 
	@echo -e "\033[36m\033[1m ++++ Build CHEASE ++++ \033[0m"; \
	($(MAKE) --no-print-directory -C externals/chease/src-f90 libchease_muscle \
	&& echo -e "\033[32m\033[1m -- OK -- \033[0m") \
	|| echo -e "\033[31m\033[1m -- FAIL -- \033[0m"

clean-chease: revert-chease
	@echo -e "\033[36m\033[1m ++++ Clean CHEASE ++++ \033[0m"; \
	($(MAKE) --no-print-directory -C externals/chease/src-f90 clean \
	&& echo -e "\033[32m\033[1m -- OK -- \033[0m") \
	|| echo -e "\033[31m\033[1m -- FAIL -- \033[0m"



# orb5 #########################################################################
orb5.git:
	@if [ ! -d "externals/orb5.git" ]; then \
	echo -e "\033[36m\033[1m ++++ Cloning ORB5 compat branch ++++ \033[0m"; \
	(git clone ssh://git@c4science.ch:2222/diffusion/O/orb5.git --branch compat --single-branch externals/orb5.git \
	&& echo -e "\033[32m\033[1m -- OK -- \033[0m") \
	|| echo -e "\033[31m\033[1m -- FAIL -- \033[0m"	; fi

orb5: orb5.git ual libbds
	@echo -e "\033[36m\033[1m ++++ Build ORB5 ++++ \033[0m"; \
	. ${MODULESHOME}/init/bash; \
	(module use -a externals/orb5.git/module_files/${ORB5_MOD}; \
	module load orb5 \
	&& echo -e "\033[32m\033[1m -- Module orb5 loaded -- \033[0m"; \
	($(MAKE) --no-print-directory -C externals/orb5.git -f makefile_module liborb5_compat \
	&& echo -e "\033[32m\033[1m -- OK -- \033[0m") \
	|| echo -e "\033[31m\033[1m -- FAIL -- \033[0m"	) \
	|| echo -e "\033[31m\033[1m -- Module orb5 not loaded -- \033[0m"

clean-orb5: orb5.git
	@echo -e "\033[36m\033[1m ++++ Clean ORB5 ++++ \033[0m"; \
	($(MAKE) --no-print-directory -C externals/orb5.git -f makefile_module clean \
	&& echo -e "\033[32m\033[1m -- OK -- \033[0m") \
	|| echo -e "\033[31m\033[1m -- FAIL -- \033[0m"

