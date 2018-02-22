SHELL=/bin/sh

# ====================================================================== 
# Source and Object File Lists
# ====================================================================== 
F_SRCS= itdlp_mod.f itdlp.f rdclargs.f wrgis.f prgrid.f \
        tdlp_open.f wrbin.f wrtdlp.f unpkplain.f tdlp_read_sq.f \
        tdlp_read_ra.f prisx.f probe.f file_info.f prinv.f itdlp_stop.f \
        wrtext.f rdstdin.f createra.f wrtdlpra.f

F_OBJS= $(patsubst %.f,%.o,$(F_SRCS))

C_SRCS=csignal.c usage.c

C_OBJS= $(patsubst %.c,%.o,$(C_SRCS))

OBJS= $(F_OBJS) $(C_OBJS)

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
LIBS =		${MDL_LIB4}
CMD =		itdlp

# ====================================================================== 
# Compiler Options for Fortran, C and linking
#
# FFLAGS    Flags for the Fortran compiler
# CFLAGS    Flags for the C compiler
# LDFLAGS	Flags to the loader
# ====================================================================== 
FFLAGS =	-O3 -zero -integer-size 32 -real-size 32 -auto -fpscomp logicals -fp-model strict -assume byterecl -axCORE-AVX2
CFLAGS =

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

clean:
	-rm -f $(OBJS)

veryclean:
	-rm -f $(OBJS) $(CMD)
