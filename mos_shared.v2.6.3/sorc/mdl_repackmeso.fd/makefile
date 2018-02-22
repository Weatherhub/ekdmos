# ====================================================================== 
# Makefile for MDL MOS Utility mdl_repackmeso
# ====================================================================== 
SHELL=/bin/sh
LIBSHARED?=/nwprod/lib
LIBLOCAL?=/nwprod/lib

# ====================================================================== 
# Source and Object File Lists
# ====================================================================== 
F_SRCS= repackmeso.f

F_OBJS= $(patsubst %.f,%.o,$(F_SRCS))

F90_SRCS= 

F90_OBJS= $(patsubst %.f90,%.o,$(F90_SRCS))

C_SRCS=

C_OBJS= $(patsubst %.c,%.o,$(C_SRCS))

OBJS= $(F90_OBJS) $(F_OBJS) $(C_OBJS)

# ====================================================================== 
# Tunable parameters
#
# FC		Name of the Fortran compiler
# CC		Name of the C compiler
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
# ====================================================================== 
FC =		${COMP}
CC =		icc
LIBS =		${MDL_LIB4} ${W3NCO_LIB4}
CMD =		mdl_repackmeso

# ====================================================================== 
# Compiler Options for Fortran, C and linking
#
# FFLAGS    Flags for the Fortran compiler
# CFLAGS    Flags for the C compiler
# LDFLAGS	Flags to the loader
# ====================================================================== 
FFLAGS =	-O3 -nofree -integer-size 32 -real-size 32 -auto -fpscomp logicals -fp-model strict -assume byterecl -axCORE-AVX2
CFLAGS =	-g -traceback
LDFLAGS =	-Bstatic

# ====================================================================== 
# Makefile rule tree. Lines from here on down should not need to be 
# changed.
# ====================================================================== 
all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

%.o:		%.c
	$(CC) $(CFLAGS) -c $<

%.o:		%.f
	$(FC) -fixed $(FFLAGS) -c -I. $<

%.o:		%.f90
	$(FC) -free $(FFLAGS) -c -I. $<

clean:
	-rm -f $(OBJS)

veryclean:
	-rm -f $(OBJS) $(CMD)
