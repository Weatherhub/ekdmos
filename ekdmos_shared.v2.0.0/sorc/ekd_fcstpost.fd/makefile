SHELL=/bin/sh
LIBSHARED?=/nwprod/lib
LIBLOCAL?=/nwprod/lib
#
# This makefile was produced by /usr/bin/fmgen at 02:00:36 PM on 05/28/96
# If it is invoked by the command line
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
SRCS=   fcstpost.f int910.f u910.f tmpcmp.f optx.f catgrq.f mrftest.f

OBJS=   fcstpost.o int910.o u910.o tmpcmp.o optx.o catgrq.o mrftest.o

# Tunable parameters
#
# FC		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
#
FC =            ${COMP}  
LDFLAGS =	
LIBS =          ${ENSMDL_LIB4} ${W3NCO_LIB4} ${W3EMC_LIB4}
CMD =		ekd_fcstpost

FFLAGS =         -O3 -fp-model strict  -nofree -fpscomp logicals -i4 -real_size 32 -axCORE-AVX2
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
	-rm -f $(SRCS) makefile
