#
# gem0/Makefile
#

NDIR	= new 

NDIR	+= $(ITM_UAL) 

INDIR	= include 
VPATH	= $(NDIR) $(INDIR) main 
IDIRS	= $(NDIR) $(INDIR)

HDR	= turb_coeff.h90 

OBJ	= gem.o 
OBJ	+= assign_turb_parameters.o 

LIB	= libgem.a

include	Makefile.actors

FPPFLAGS	+= -DDOUAL

