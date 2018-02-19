#!/bin/sh
###############################################################
#
#   AUTHOR:    Gilbert - W/NP11
#
#   DATE:      01/07/99
#
#   PURPOSE:   This script uses the make utility to update the libensmdl 
#              archive library.
#              It first reads a list of the source files in the library and
#              then generates a makefile used to update the archive
#              library.  The make command is then executed,
#              where the archive library name and 
#              compilation flags are passed to the makefile through 
#              environment variables.
#
#   REMARKS:   Only source files that have been modified since the last
#              library update are recompiled and replaced in the object
#              archive libraries.  The make utility determines this
#              from the file modification times.
#
#              New source files are also compiled and added to the object 
#              archive libraries.
#
###############################################################

#
#     Generate a list of object files that corresponds to the
#     list of Fortran ( .f ) files in the current directory
#
for i in `ls *.f`
do
  obj=`basename $i .f`
  OBJS="$OBJS ${obj}.o"
done
#
#     Remove make file, if it exists.  May need a new make file
#     with an updated object file list.
#
if [ -f make.libensmdl ] 
then
  rm -f make.libensmdl
fi
#
#     Generate a new make file ( make.libensmdl), with the updated object list,
#     from this HERE file.
#
cat > make.libensmdl << EOF
SHELL=/bin/sh

\$(LIB):	\$(LIB)( ${OBJS} )

.f.a:
	ifort -c \$(FFLAGS) \$<
	#ftn -c \$(FFLAGS) \$<
	ar -ruv \$@ \$*.o
	rm -f \$*.o

EOF
#
#     Update 4-byte version of libensmdl_4.a
#
#export LIB="/nwprod/lib/libensmdl_4.a"
export LIB="libensmdl_4.a"
#export FFLAGS=" -O3 -qfixed -qnosave -qintlog \
#-qintsize=4 -qrealsize=4 -qstrict -qinitauto=0"
export FFLAGS=" -O3 -nofree -i4 -real_size 32 -auto -fpscomp logicals -fp-model strict -assume byterecl"
make -f make.libensmdl

#
#     Update 8-byte version of libensmdl_8.a
#
#export LIB="../../libensmdl_8.a"
#export FFLAGS=" -O3 -qfixed -qnosave -qintlog \
#-qintsize=8 -qrealsize=8 -qstrict -qinitauto=0"
#make -f make.libensmdl

#rm -f make.libensmdl
