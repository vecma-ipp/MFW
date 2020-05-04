#/* -*- makefile -*- */


# paths ########################################################################
SRCDIR = ets/src
OBJDIR = ets/obj/${SYS}


# set up the VPATH for searching for source files ##############################
VPATH = ${SRCDIR}/solvers ${SRCDIR}/ets ${SRCDIR}/types 
VPATH += ${SRCDIR}/xmllib ${SRCDIR}/itm_types ${SRCDIR}/itm_constants 
VPATH += ${SRCDIR}/itm_shared ${SRCDIR}/version ${SRCDIR}/equilibrium 
VPATH += ${SRCDIR}/itm_tools ${SRCDIR}/transport ${SRCDIR}/sources


# additional flags #############################################################
INCLUDES = -I${OBJDIR} -I${INC_UAL} -I${INC_TYPES} -I${INC_CONSTANTS} \
	-I${INC_XML}

LIBS = ${OBJDIR}/libets.a ${LIB_UAL}/libual.a ${LIBLAPACK}

FC90FLAGS += ${INCLUDES} ${MODSPEC}${OBJDIR} 
##-C -traceback -debug all

FPPFLAGS += -DNO_EUITM_ROUTINES -DNO_LIBMATHEVAL


# objects files ################################################################
TYPES_O = ${OBJDIR}/allocate_deallocate.o ${OBJDIR}/ets_plasma.o \
	${OBJDIR}/mod_f90_kind.o ${OBJDIR}/convert.o 

SOLVERS_O = ${OBJDIR}/type_solver.o ${OBJDIR}/solution_interface.o \
	${OBJDIR}/solution1.o ${OBJDIR}/solution2.o ${OBJDIR}/solution3.o \
	${OBJDIR}/solution4.o ${OBJDIR}/solution6.o ${OBJDIR}/solution7.o \
	${OBJDIR}/solution10.o ${OBJDIR}/SOLVERFEM.o

ETS_O = ${OBJDIR}/calculate_globals.o ${OBJDIR}/s4_parameters.o \
	${OBJDIR}/plasma_collisionality.o ${OBJDIR}/pedestal.o \
	${OBJDIR}/diagnostic_out_ets.o \
	${OBJDIR}/main_plasma.o ${OBJDIR}/ets.o \
	${OBJDIR}/equilibrium_input.o ${OBJDIR}/spitzer.o

SOURCES_O = ${OBJDIR}/gausian_src.o

SHARED_O = ${OBJDIR}/itm_toolbox.o ${OBJDIR}/l3interp.o 
### add local l3interp waiting for usage from itm_toolbox or l3interps modules

LIB_ETS_O = ${SHARED_O} ${TYPES_O} ${SOLVERS_O} ${SOURCES_O} ${ETS_O} 

#WRAPPER_O = ${OBJDIR}/ets_wrapper.o



# rules ########################################################################
${OBJDIR}/%.o: %.f90
	${FC90} -o $@ -c $< $(FC90FLAGS)


%.f90: %.F90
	$(FPP) $(FPPFLAGS) $< $@


all: ${SYS} ${OBJDIR}/libets.a

#${OBJDIR}/ets_wrapper


#${OBJDIR}/ets_wrapper:  ${OBJDIR}/libets.a ${SHARED_O} ${WRAPPER_O}
#	${LD} ${LDFLAGS} -o $@ ${WRAPPER_O} ${SHARED_O} ${LIBS} 


${OBJDIR}/libets.a: ${LIB_ETS_O}
	ar r $@ $?
	ranlib $@


${SYS}:
	@mkdir -p ${OBJDIR}


clean:
	rm -rf ${OBJDIR}


