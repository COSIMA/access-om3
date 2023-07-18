#!/usr/bin/env sh

# Compiles ACCESS-OM3 on gadi.nci.org.au
#
# This is just a prototype to get things working - eventually we'd want to do all of this with spack
#
# NB: Requires membership of the "ik11" project - apply at https://my.nci.org.au/mancini/project/ik11 if needed

set -e

SCRIPT_DIR="$(dirname "$(readlink -f "$0")")"  # dir of this script

BUILD_TYPES=('Debug' 'Release')
COMPILER_VERSION=2021.6.0
OPENMPI_VERSION=4.1.4


# CMakeLists.txt requires ESMF v8.3.0 or higher, FMS, and ParallelIO, but NCI doesn't supply them, so we use our own installation via spack.
# This is in /g/data/ik11/spack/ which uses https://github.com/COSIMA/spack-config

module purge
module load cmake/3.24.2
module use /g/data/ik11/spack/0.20.1/share/modules/linux-rocky8-cascadelake  # requires membership of "ik11" group
module load esmf/8.3.1-intel-${COMPILER_VERSION} fms/2023.01-intel-${COMPILER_VERSION} parallelio/2.5.10-intel-${COMPILER_VERSION}
module load intel-compiler/${COMPILER_VERSION} openmpi/${OPENMPI_VERSION}

cd ${SCRIPT_DIR}

hash=`git rev-parse --short=7 HEAD`
test -z "$(git status --porcelain)" || hash=${hash}-modified # uncommitted changes or untracked files

mkdir -p bin

for BUILD_TYPE in "${BUILD_TYPES[@]}"; do
  echo "BUILD_TYPE = "${BUILD_TYPE}
  rm -r build || true

  cmake -S . -B build --preset=gadi -DCMAKE_BUILD_TYPE=${BUILD_TYPE} -DCMAKE_VERBOSE_MAKEFILE=ON
  cmake --build build -j 4

  for exec in build/access-om3*; do
    dest=bin/$(basename ${exec})
    if [[ ${BUILD_TYPE} != "Release" ]] ; then dest=${dest}-${BUILD_TYPE}; fi
    dest=${dest}-${hash}
    cp -p ${exec} ${dest}
    echo "Successfully built ${dest}"
  done
done
