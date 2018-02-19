#!/bin/sh
# ========================================================================  
#
# History: July       2015  Wagner    Created from MOS version
#
# Purpose: This script will perform makefile actions for all EKDMOS
#          operational programs. All source directories ekd_*.fd/ in
#          the root directory of this script will be entered and the
#          user-specified makefile action will be performed.  Also, the
#          executable that is compiled will be move to ../../exec/. It is
#          assumed that this directory exists.
#
#          The script prompt the user before the actions are performed.
#
# Usage:   To make all programs, ./make-all-ekd-programs.sh
#          To clean all programs, ./make-all-ekd-programs.sh clean
#
# ========================================================================  

# ========================================================================
# Function to set module directory 
# ========================================================================
set_module_dir()
{
#   local default_module=$MODULEPATH
   local default_module="../modulefiles"
   read -e -p "Set the ekdmos_shared module path [default: $default_module]: " usermodule
   if [ X$usermodule == X ]; then
      use_local_module="Y"
      module_to_use=$default_module
   else
      use_local_module="Y"
      module_to_use=$usermodule
   fi
}

# ========================================================================
# Function to set lib directory (local libraries)
# ========================================================================
set_lib_dir()
{
   local default_lib="../../lib"
   read -e -p "Set the local library path [default: $default_lib]: " userlib
   if [ X$userlib == X ]; then
      use_local_lib="Y"
      lib_to_use=$default_lib
   else
      use_local_lib="Y"
      lib_to_use=$userlib
   fi
}

# ========================================================================
# Function to set lib directory (shared libraries)
# ========================================================================
set_shared_dir()
{
   local default_lib="/nwprod2/lib"
   read -e -p "Set the shared library path [default: /nwprod2/lib]: " userlib
   if [ X$userlib == X ]; then
      shared_lib_to_use=$default_lib
   else
      shared_lib_to_use=$userlib
   fi
}

# ========================================================================
# Function to set lib directory (3rd party libraries - ZEUS ONLY)
# ========================================================================
set_thirdparty_dir()
{
   local default_lib="/nwprod2/lib"
   read -e -p "Set the 3rd party library path [default: /nwprod2/lib]: " userlib
   if [ X$userlib == X ]; then
      thirdparty_lib_to_use=$default_lib
   else
      thirdparty_lib_to_use=$userlib
   fi
}

# ========================================================================  
# Function to ask user about actions to be performed
# ========================================================================  
ask_user()
{
   execdir=$(pwd | sed 's/\/sorc$/\/exec/g')
   echo -e "The following makefile actions will be performed:"
   case "$1" in
       clean)
          echo -e "\n     make clean in all ekd_*.fd/ directories\n" ;;
       *)
          echo -e "\n     make in all ekd_*.fd/ directiories and executables moved to $execdir\n"
          set_module_dir
          set_lib_dir ;;
   esac

#   read -p "Accept? (y) or (n): " answer
#   case "$answer" in
#      y|Y)
#          ;;
#      n|N)
#          exit 1
#          ;;
#      *)
#          ask_user
#   esac
}

user_accept()
{
   if [ "$makerule" != "clean" ]; then
      module list
   fi
   read -p "Accept? (y) or (n): " answer

   case "$answer" in
      y|Y)
          ;;
      n|N)
          make-all-ekd-programs.sh
          ;;
      *)
          ask_user
   esac
}

# ========================================================================  
# Begin script
# ========================================================================  
clear
if [ $# -eq 1 ]; then
   if [ "$1" == "clean" ]; then
      makerule=$1
   else
      exit 1
   fi
elif [ $# -gt 1 ]; then
   exit 1
fi

ask_user $makerule

ekddirs=$(/bin/ls -1d *.fd/)

export LIBLOCAL=$lib_to_use
if [ "$use_local_module" == Y ]; then
   if [ "$use_local_lib" == Y ]; then
      export USELOCALLIB="Y"
   else
      unset USELOCALLIB
   fi
   module use $module_to_use
   module load ekdmos_shared
elif [ "$makerule" != "clean" ]; then
   if [ "$use_local_lib" == Y ]; then
      export USELOCALLIB="Y"
   else
      unset USELOCALLIB
   fi
   module load ekdmos_shared
fi

user_accept

for dir in $ekddirs
do
   execname=$(echo $dir | cut -d"." -f 1)
   echo -e "\n----- $execname -----\n"
   cd $dir
#   export LIBSHARED=$shared_lib_to_use
#   export LIBEXTRA=$thirdparty_lib_to_use
   if [ ! -z "$makerule" ]; then
      make $makerule
   else
      make
      if [ $? -eq 0 ]; then
         echo -e "\nProgram $execname successfully created"
         mv $execname $execdir/$execname
      else
         echo -e "\n***** PROGRAM $execname NOT CREATED *****"
      fi
   fi
   cd ../

done

exit 0
