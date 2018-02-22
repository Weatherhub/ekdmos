set -ax
PWD=`pwd`
module use $PWD/modulefiles
module load mos_shared-intel
module list
echo
date
echo
cd lib/sorc/mdl
./makelibmdl_intel.sh
mv libmdl_4_intel.a libmdl_8_intel.a ../../../lib/.
echo
date
echo
cd $PWD/sorc
./make-all-mdl-programs.sh
echo
date

