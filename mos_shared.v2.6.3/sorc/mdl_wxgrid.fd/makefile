SHELL=/bin/sh
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
SRCS=	wxgrid.f u720.f int720.f pred25.f option.f pred26.f \
        scalx2.f gtvegr.f rdwxcn.f genwx.f \
        wxpcat.f \
        wxwrd3.f \
        ckmiss.f \
        wxvcat.f \
        hsmth.f \
        wxpchr.f \
        wxpphs.f \
        wxpint.f \
        wxvqpf.f \
        wxexpl.f \
        wxtstm.f \
        wxtsvr.f \
        wxkeys.f 

OBJS=   wxgrid.o u720.o int720.o pred25.o option.o pred26.o \
        scalx2.o gtvegr.o rdwxcn.o genwx.o \
        wxpcat.o \
        wxwrd3.o \
        ckmiss.o \
        wxvcat.o \
        hsmth.o \
        wxpchr.o \
        wxpphs.o \
        wxpint.o \
        wxvqpf.o \
        wxexpl.o \
        wxtstm.o \
        wxtsvr.o \
        wxkeys.o 

# Tunable parameters
#
# FC		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
#
FC =            ${COMP}
LDFLAGS =       -openmp
LIBS = 		${MDL_LIB4} ${W3NCO_LIB4}
CMD =		mdl_wxgrid
PROFLIB =	

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
	-rm -f $(SRCS) makefile
