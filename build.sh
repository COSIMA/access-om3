#!/usr/bin/env sh

# Compiles ACCESS-OM3 on gadi.nci.org.au
#
# This is just a prototype to get things working - eventually we'd want to do all of this with spack
#
# NB: Requires membership of the "access" project - apply at https://my.nci.org.au/mancini/project/access if needed

set -e
set -x

SCRIPT_DIR="$(dirname "$(readlink -f "$0")")"  # dir of this script

COMPILER_VERSION=2021.6.0
OPENMPI_VERSION=4.1.4


# CMakeLists.txt requires ESMF v8.3.0 or higher, FMS, and ParallelIO, but NCI doesn't supply them, so we create our own installation via spack.

# first clone spack
cd /g/data/${PROJECT}/${USER}
ls spack || git clone https://github.com/spack/spack.git  # everyone needs their own spack clone for their own spack tree
cd spack
git checkout remotes/origin/releases/v0.19

# set up for builds using Aidan's shared Spack config https://forum.access-hive.org.au/t/shared-access-spack-configuration/227
module unload conda  # to avoid clashes with module spack/config
module use /g/data/access/spack/modules  # requires membership of "access" group
module load spack/config  # contains just enough configuration to standardise compilers, MPI and external packages but use your own spack tree
. /g/data/${PROJECT}/${USER}/spack/share/spack/setup-env.sh  # see explanation https://forum.access-hive.org.au/t/shared-access-spack-configuration/227


# build esmf - might need several attempts!
spack install esmf@8.3.1 %intel@${COMPILER_VERSION} ^openmpi@${OPENMPI_VERSION}

# build fms
spack install fms %intel@${COMPILER_VERSION} ^openmpi@${OPENMPI_VERSION}

# build pio
spack install parallelio %intel@${COMPILER_VERSION} ^openmpi@${OPENMPI_VERSION}


# fix up LD_LIBRARY_PATH for a few modules - see https://github.com/COSIMA/access-om3/issues/17
# BUG: this will probably fail if there are multiple versions of these!
spack module tcl refresh -y netcdf-c  # BUG: will refresh all versions
echo "prepend-path LD_LIBRARY_PATH \"$(spack location -i netcdf-c)/lib\"" >> /g/data/${PROJECT}/${USER}/spack/share/spack/modules/linux-rocky8-cascadelake/$(spack module tcl find netcdf-c)
spack module tcl refresh -y parallel-netcdf  # BUG: will refresh all versions
echo "prepend-path LD_LIBRARY_PATH \"$(spack location -i parallel-netcdf)/lib\"" >> /g/data/${PROJECT}/${USER}/spack/share/spack/modules/linux-rocky8-cascadelake/$(spack module tcl find parallel-netcdf)
spack module tcl refresh -y xerces-c  # BUG: will refresh all versions
echo "prepend-path LD_LIBRARY_PATH \"$(spack location -i xerces-c)/lib\"" >> /g/data/${PROJECT}/${USER}/spack/share/spack/modules/linux-rocky8-cascadelake/$(spack module tcl find xerces-c)
echo "prepend-path    LIBRARY_PATH \"$(spack location -i xerces-c)/lib\"" >> /g/data/${PROJECT}/${USER}/spack/share/spack/modules/linux-rocky8-cascadelake/$(spack module tcl find xerces-c)


# now build access-om3 with cmake - eventually we'd want to do all of this with spack but for now we only use spack to provide esmf, fms & parallelio

cd ${SCRIPT_DIR}

module purge
module use /g/data/access/spack/modules  # requires membership of "access" group
module load spack/config  # contains just enough configuration to standardise compilers, MPI and external packages but use your own spack tree
. /g/data/${PROJECT}/${USER}/spack/share/spack/setup-env.sh  # see explanation https://forum.access-hive.org.au/t/shared-access-spack-configuration/227

module avail intel-compiler
module load intel-compiler/${COMPILER_VERSION}

module avail openmpi
module load openmpi/${OPENMPI_VERSION}  # using NCI rather than spack version - is this the best choice?

module avail netcdf
module load netcdf-c-4.9.0-intel-${COMPILER_VERSION}-t5vmcra
module load netcdf-fortran-4.6.0-intel-${COMPILER_VERSION}-l6jywju

module avail xerces
module load xerces-c-3.2.3-intel-${COMPILER_VERSION}-z57iwgl  # needed for esmf

module avail esmf
module load esmf-8.3.1-intel-${COMPILER_VERSION}-cludzvi  # use result of `module avail esmf` to determine this module name

module avail fms
module load fms-2022.04-intel-${COMPILER_VERSION}-i2vi3ed  # use result of `module avail fms` to determine this module name

module avail parallelio
module load parallelio-2.5.9-intel-${COMPILER_VERSION}-zqo4fh6  # use result of `module avail parallelio` to determine this module name

rm -r build || true

cmake --debug-find -S . -B build -DCMAKE_VERBOSE_MAKEFILE=ON -DCMAKE_C_COMPILER="mpicc" -DCMAKE_Fortran_COMPILER="mpif90"
cmake --build build -j 4 -v
