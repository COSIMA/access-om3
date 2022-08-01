cmake_minimum_required(VERSION 3.18)

project(ACCESS-OM3
        VERSION 0.1
        LANGUAGES C Fortran)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake)


# Set what is going to be built
set(MOM6 ON CACHE BOOL "Enable MOM6")

message("")
message("MOM6 ............. ${MOM6}")

# Debug or release mode?
if(DEBUG)
  set(CMAKE_BUILD_TYPE "Debug" CACHE STRING "Set type of build to Debug." FORCE)
else()
  set(CMAKE_BUILD_TYPE "Release" CACHE STRING "Set type of build to Release." FORCE)
endif()

# Find dependencies
find_package(MPI REQUIRED)
find_package(NetCDF 4.7.4 REQUIRED Fortran)
if(MOM6)
  find_package(fms COMPONENTS R8 REQUIRED)
endif()

# Components
if(MOM6)
  add_subdirectory(MOM6)
endif()
