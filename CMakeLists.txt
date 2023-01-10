cmake_minimum_required(VERSION 3.18)

project(ACCESS-OM3
        VERSION 0.1
        LANGUAGES C Fortran)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake)

# Build options
set(OPENMP          ON  CACHE BOOL "Enable OpenMP threading")

message(STATUS "Build options")
message(STATUS "  - OPENMP ${OPENMP}")

# Common compiler flags and definitions
add_compile_definitions($<$<CONFIG:Debug>:DEBUG>)

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fbacktrace")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set(CMAKE_Fortran_FLAGS_DEBUG "-g -O0")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -traceback")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O2")
  set(CMAKE_Fortran_FLAGS_DEBUG "-g -O0")
else()
  message(WARNING "Fortran compiler with ID ${CMAKE_Fortran_COMPILER_ID} will be used with CMake default options")
endif()

if(CMAKE_C_COMPILER_ID MATCHES "GNU")
  set(CMAKE_C_FLAGS_RELEASE "")
  set(CMAKE_C_FLAGS_DEBUG "-g -O0")
elseif(CMAKE_C_COMPILER_ID MATCHES "Intel")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -traceback")
  set(CMAKE_C_FLAGS_RELEASE "")
  set(CMAKE_C_FLAGS_DEBUG "-g -O0 -ftrapuv")
elseif(CMAKE_C_COMPILER_ID MATCHES "Clang")
  set(CMAKE_C_FLAGS_RELEASE "")
  set(CMAKE_C_FLAGS_DEBUG "-g -O0")
else()
  message(WARNING "C compiler with ID ${CMAKE_C_COMPILER_ID} will be used with CMake default options")
endif()


# Find dependencies
find_package(MPI REQUIRED)
if(OPENMP)
  find_package(OpenMP REQUIRED)
endif()
find_package(NetCDF 4.7.4 REQUIRED Fortran)
find_package(ESMF 8.3.0 MODULE REQUIRED)
find_package(fms COMPONENTS R8 REQUIRED)

# Components
add_subdirectory(MOM6)
