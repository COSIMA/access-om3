#!/usr/bin/env sh

# Compiles ACCESS-OM3 on gadi.nci.org.au
#
# This is just a prototype to get things working - eventually we'd want to do all of this with spack
#
# NB: Requires membership of the "ik11" project - apply at https://my.nci.org.au/mancini/project/ik11 if needed

set -e
set -x

SCRIPT_DIR="$(dirname "$(readlink -f "$0")")"  # dir of this script

COMPILER_VERSION=2021.6.0
OPENMPI_VERSION=4.1.4


# CMakeLists.txt requires ESMF v8.3.0 or higher, FMS, and ParallelIO, but NCI doesn't supply them, so we use our own installation via spack.

module purge
module use /g/data/ik11/spack/0.19.0/share/modules/linux-rocky8-cascadelake  # requires membership of "ik11" group
module load esmf/8.3.1-intel-${COMPILER_VERSION} fms/2022.04-intel-${COMPILER_VERSION} parallelio/2.5.9-intel-${COMPILER_VERSION}
module load intel-compiler/${COMPILER_VERSION} openmpi/${OPENMPI_VERSION}

cd ${SCRIPT_DIR}

rm -r build || true

cmake --debug-find -S . -B build -DCMAKE_BUILD_TYPE=Debug -DCMAKE_VERBOSE_MAKEFILE=ON -DCMAKE_C_COMPILER="mpicc" -DCMAKE_Fortran_COMPILER="mpif90" -DNetCDF_C_LIBRARY=$SPACK_NETCDF_C_ROOT/lib/libnetcdf.so -DNetCDF_Fortran_LIBRARY=$SPACK_NETCDF_FORTRAN_ROOT/lib/libnetcdff.so -DNetCDF_Fortran_INCLUDE_DIRS=$SPACK_NETCDF_FORTRAN_ROOT/include
cmake --build build -j 4 -v
