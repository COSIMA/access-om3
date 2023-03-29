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
add_compile_definitions(
  $<$<CONFIG:Debug>:DEBUG>
  CESMCOUPLED
)

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
find_package(PIO 2.5.3 REQUIRED COMPONENTS C Fortran)

# Some code shared by several components
add_subdirectory(share)

# Components

# Ocean component (MOM6)
add_subdirectory(MOM6)

# Sea-ice component (CICE6)
add_subdirectory(CICE)

# Wave component (WW3)
set(SWITCH "multi_esmf" CACHE STRING "NUOPC mesh cap")
add_subdirectory(WW3)

# Mediator component (CMEPS)
add_subdirectory(CMEPS)

# Data component (CDEPS)
add_subdirectory(CDEPS)


# Driver

# We use the CESM driver from CMEPS
add_library(cesm_driver
  CMEPS/CMEPS/cesm/driver/esm.F90
  CMEPS/CMEPS/cesm/driver/ensemble_driver.F90
  CMEPS/CMEPS/cesm/driver/esm_time_mod.F90
)
target_link_libraries(cesm_driver PUBLIC share mom6 cice WW3::WW3 cmeps cdeps esmf PIO::PIO_Fortran)

set_target_properties(cesm_driver PROPERTIES Fortran_MODULE_DIRECTORY
  ${CMAKE_CURRENT_BINARY_DIR}/mod)
target_include_directories(cesm_driver PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/mod>
                                         $<INSTALL_INTERFACE:mod>)


add_executable(access-om3 CMEPS/CMEPS/cesm/driver/esmApp.F90)
target_link_libraries(access-om3 PRIVATE cesm_driver esmf PIO::PIO_Fortran NetCDF::NetCDF_Fortran)
