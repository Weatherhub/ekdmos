SHELL=/bin/sh
LIBSHARED?=/nwprod/lib
LIBLOCAL?=/nwprod/lib
#
# This makefile was produced by /usr/bin/fmgen at 11:21:32 AM on 04/12/96
# If it is invoked by the command line
#	make -f makefile
# it will compile the fortran modules indicated by SRCS into the object
# modules indicated by OBJS and produce an executable named a.out.
#
# If it is invoked by the command line
#	make -f makefile a.out.prof
# it will compile the fortran modules indicated by SRCS into the object
# modules indicated by OBJS and produce an executable which profiles
# named a.out.prof.
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

SRCS= augmt1.f  augmt3.f avglap.f  bcd5.f  blend.f  bogus.f  caksno.f  cateq.f cconsn.f  cigft.f  cightc.f cigobc.f ckmaxt.f  ckmint.f  ckpop.f  ckqpf.f  cktdp.f  ckwndg.f  clip.f  clos.f  clos2.f  clos2g.f  concpr.f  conekd.f  conspr.o convpr.f  corbc5.f  cutit.f  dens.f  denser.f  densrm.f  densrq.f  difwh.f  dirspd.f  discig.f  elvar.f  elvarg.f  erest.f  eslp5.f  esp5.f  fgskya.f  fitwth.f  fltag.f  fltagm.f  fstgs5.f  gfetch.f  gradck.f  granalysis.f  gthres.f  hsmth.f  int155.f  intrpl.f  intrpx.f  itrp.f  itrpsl.f  itrpsx.f  keylst.f  lapse.f  lapsua.f  list.f  ltagrd.f  mshxms.f  newxy.f  orsmth.f  orvwsm.f osmth.f  osmth1.f  points.f  post.f  qpf6p6.f  rdnei.f  rdprs.f  rdstr1.f  rdstr7.f  rdv155.f  rdvrhl.f  regres.f restrw.f  restwh.f  rough.f  roughg.f  sample.f  sclq06.f  sclq12.f  sclcig.f sclsky.f  sclsno.f  sclvis.f setcft.f  setlnd.f  setpnt.f  setvmi.f  skyamt.f  skycig.f smoth.f  smothc.f  smothg.f  smothn.f  spotrm.f stsnoz.f  szgrdm.f  szgrid.f  terdif.f  trnwnd.f  u155.f  u155ck.f  u405a.f  vari.f  varig.f  varil.f  varilg.f  variw.f  variwg.f  vismi.f  visobc.f wndcor.f  wrdatm.f  wrtm.f  wthol1.f  wthol2.f wthol3.f xycom1.f actual.f nominl.f

OBJS= augmt1.o  augmt3.o avglap.o  bcd5.o  blend.o  bogus.o  caksno.o  cateq.o cconsn.o  cigft.o  cightc.o cigobc.o ckmaxt.o  ckmint.o  ckpop.o  ckqpf.o  cktdp.o  ckwndg.o  clip.o  clos.o  clos2.o  clos2g.o  concpr.o  conekd.o  conspr.o convpr.o  corbc5.o  cutit.o  dens.o  denser.o  densrm.o  densrq.o  difwh.o  dirspd.o  discig.o  elvar.o  elvarg.o  erest.o  eslp5.o  esp5.o  fgskya.o  fitwth.o  fltag.o  fltagm.o  fstgs5.o  gfetch.o  gradck.o  granalysis.o  gthres.o  hsmth.o  int155.o  intrpl.o  intrpx.o  itrp.o  itrpsl.o  itrpsx.o  keylst.o  lapse.o  lapsua.o  list.o  ltagrd.o  mshxms.o  newxy.o  orsmth.o  orvwsm.o osmth.o  osmth1.o  points.o  post.o  qpf6p6.o  rdnei.o  rdprs.o  rdstr1.o  rdstr7.o  rdv155.o  rdvrhl.o  regres.o restrw.o  restwh.o  rough.o  roughg.o  sample.o  sclq06.o  sclq12.o  sclcig.o sclsky.o  sclsno.o  sclvis.o setcft.o  setlnd.o  setpnt.o  setvmi.o  skyamt.o  skycig.o smoth.o  smothc.o  smothg.o  smothn.o  spotrm.o stsnoz.o  szgrdm.o  szgrid.o  terdif.o  trnwnd.o  u155.o  u155ck.o  u405a.o   vari.o  varig.o  varil.o  varilg.o  variw.o  variwg.o  vismi.o  visobc.o wndcor.o  wrdatm.o  wrtm.o  wthol1.o  wthol2.o wthol3.o  xycom1.o actual.o nominl.o

# Tunable parameters
#
# FC		Name of the fortran compiling system to use
# LDFLAGS	Flags to the loader
# LIBS		List of libraries
# CMD		Name of the executable
# PROFLIB	Library needed for profiling
#
FC =	        ${COMP}  
LDFLAGS =
LIBS =          ${ENSMDL_LIB4} ${MDL_LIB4} ${W3NCO_LIB4} ${W3EMC_LIB4}
CMD =		ekd_granalysis
PROFLIB =	

# To perform the default compilation, use the first line
# To compile with flowtracing turned on, use the second line
# To compile giving profile additonal information, use the third line
# WARNING:  SIMULTANEOUSLY PROFILING AND FLOWTRACING IS NOT RECOMMENDED 
# Changed on 20100518
FFLAGS = -nofree -xHost -fpscomp logicals -O3 -fp-model strict -i4 -real-size 32 -axCORE-AVX2


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
