SHELL=/bin/sh
#
# If this makefile is invoked by the command line
#	make -f makefile
# it will compile the fortran modules indicated by SRCS into the object
# modules indicated by OBJS and produce an executable named a.out.
#
# To remove all the objects but leave the executables use the command line
#	make -f makefile clean
#
# To remove everything but the source files use the command line
#	make -f makefile clobber
#
# To remove the source files created by /usr/bin/fmgen and this makefile
# use the command line
#	make -f makefile void
#
# The parameters SRCS and OBJS should not need to be changed.  If, however,
# you need to add a new module add the name of the source module to the
# SRCS parameter and add the name of the resulting object file to the OBJS
# parameter.  The new modules are not limited to fortran, but may be C, YACC,
# LEX, or CAL.  An explicit rule will need to be added for PASCAL modules.
#
SRCS=	CLOUD.f GEOGRAPHY.f HRLYTBL.f Kelvin_F.f \
	MAXIMUM_TEMP.f MINIMUM_TEMP.f Meter_ft.f PRECIPITATION.f \
	PRESSURE.f REPORT_TIME.f SNOW.f SUNSHINE.f \
	TEMPERATURE.f TEMPQC.f VISIBILITY.f WEATHER.f \
	WINDS.f

OBJS=	CLOUD.o GEOGRAPHY.o HRLYTBL.o Kelvin_F.o \
	MAXIMUM_TEMP.o MINIMUM_TEMP.o Meter_ft.o PRECIPITATION.o \
	PRESSURE.o REPORT_TIME.o SNOW.o SUNSHINE.o \
	TEMPERATURE.o TEMPQC.o VISIBILITY.o WEATHER.o \
	WINDS.o

# Tunable parameters
#
# FC		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
#
FC =            ${COMP}
LDFLAGS =	
LIBS = 		${MDL_LIB8} ${W3NCO_LIB8} ${BUFR_LIB8}
CMD =		mdl_hrlytbl
FFLAGS =	-O3 -nofree -integer-size 64 -real-size 64 -auto -fpscomp logicals -fp-model strict -assume byterecl -axCORE-AVX2

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:		$(CMD)

$(CMD) :       $(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)


clean:
	-rm -f $(OBJS)

clobber:	clean
	-rm -f $(CMD) $(CMD).prof

void:	clobber
	-rm -f $(SRCS) makefile
