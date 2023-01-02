cmake_minimum_required(VERSION 3.18)

project(ACCESS-OM3
        VERSION 0.1
        LANGUAGES C Fortran)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake)


# Debug or release mode?
if(DEBUG)
  set(CMAKE_BUILD_TYPE "Debug" CACHE STRING "Set type of build to Debug." FORCE)
else()
  set(CMAKE_BUILD_TYPE "Release" CACHE STRING "Set type of build to Release." FORCE)
endif()

# Find dependencies
find_package(MPI REQUIRED)
find_package(NetCDF 4.7.4 REQUIRED Fortran)
find_package(fms COMPONENTS R8 REQUIRED)

# Components
add_subdirectory(MOM6)
