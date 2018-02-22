#!/bin/sh
###############################################################
#
#   AUTHOR:    Engle - W/OST22 (MDL, Statistical Modeling Branch)
#
#   DATE:      04/05/2013
#
#   PURPOSE:   This script uses the make utility to update the libmdl 
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
if [ -f make.libmdl ] 
then
  rm -f make.libmdl
fi
#
#     Generate a new make file ( make.libmdl), with the updated object list,
#     from this HERE file.
#
cat > make.libmdl << EOF
SHELL=/bin/sh

\$(LIB):	\$(LIB)( ${OBJS} )

.f.a:
	${COMP} -c \$(FFLAGS) \$<
	ar -rvu \$@ \$*.o
	rm -f \$*.o

EOF
#
#     Update 4-byte version of libmdl_4.a
#
export LIB="libmdl_4_intel.a"
export FFLAGS="-O3 -nofree -integer-size 32 -real-size 32 -auto -fpscomp logicals -fp-model strict -assume byterecl -axCore-AVX2"
make -f make.libmdl

#
#     Update 8-byte version of libmdl_8.a
#
export LIB="libmdl_8_intel.a"
export FFLAGS="-O3 -nofree -integer-size 64 -real-size 64 -auto -fpscomp logicals -fp-model strict -assume byterecl -axCore-AVX2"
make -f make.libmdl

rm -f make.libmdl
