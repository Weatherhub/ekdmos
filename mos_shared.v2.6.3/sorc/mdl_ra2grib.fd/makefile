SHELL=/bin/sh
#
#
SRCS=	maingrib.f intgrib.f bitmap.f cress.f lat2xy.f ra2grib.f

OBJS=	maingrib.o intgrib.o bitmap.o cress.o lat2xy.o ra2grib.o

# Tunable parameters
#
# FC		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
# PROFLIB	Library needed for profiling
#
FC =            ${COMP}
LDFLAGS =	
LIBS =  	${MDL_LIB4} ${W3EMC_LIB4} ${W3NCO_LIB4} ${BACIO_LIB4}
CMD =		mdl_ra2grib
PROFLIB =	

# To perform the default compilation, use the first line
# To compile with flowtracing turned on, use the second line
# To compile giving profile additonal information, use the third line
# WARNING:  SIMULTANEOUSLY PROFILING AND FLOWTRACING IS NOT RECOMMENDED 
FFLAGS =	-O3 -nofree -integer-size 32 -real-size 32 -auto -fpscomp logicals -fp-model strict -assume byterecl -axCORE-AVX2

# Lines from here on down should not need to be changed.  They are the
# actual rules which make uses to build a.out.
#
all:		$(CMD)

$(CMD):		$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(LIBS)

# Make the profiled version of the command and call it a.out.prof
#
$(CMD).prof:	$(OBJS)
	$(FC) $(LDFLAGS) -o $(@) $(OBJS) $(PROFLIB) $(LIBS)

clean:
	-rm -f $(OBJS)

clobber:	clean
	-rm -f $(CMD) $(CMD).prof

void:	clobber
	-rm -f $(SRCS) makegribmos
