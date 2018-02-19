SHELL=/bin/sh
LIBSHARED?=/nwprod/lib
LIBLOCAL?=/nwprod/lib
#
# This makefile was produced by /usr/bin/fmgen at 03:45:31 PM on 03/07/96
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
SRCS=	tgintrp.f gridpost.f u202.f pred23.f pred24.f rdpred.f option.f ckbounds.f grduntcvt.f cmpdpgr.f cmpwdrgr.f ckwspdgr.f ckwndgst.f ckgpop.f cmpqpfgr.f cmptails.f grmean.f modcdf.f ckids.f gfetch.f packgp.f intrpb.f actual.f nominl.f

OBJS=	tgintrp.o gridpost.o u202.o pred23.o pred24.o rdpred.o option.o ckbounds.o grduntcvt.o cmpdpgr.o cmpwdrgr.o ckwspdgr.o ckwndgst.o ckgpop.o cmpqpfgr.o cmptails.o grmean.o modcdf.o ckids.o gfetch.o packgp.o intrpb.o actual.o nominl.o

# Tunable parameters
#
# FC		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
#
FC =            ${COMP}
LDFLAGS =	
LIBS =          ${ENSMDL_LIB4} ${MDL_LIB4} ${W3NCO_LIB4} ${W3EMC_LIB4}
CMD =		ekd_gridpost
PROFLIB =	
FFLAGS =	-O3 -fp-model strict -nofree -fpscomp logicals -real-size 32 -i4 -axCORE-AVX2

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
