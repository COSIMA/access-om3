cmake_minimum_required(VERSION 3.18)

project(ACCESS-OM3
        VERSION 0.1
        LANGUAGES C Fortran)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake)

# Components to enable
set(ENABLE_MOM6     ON  CACHE BOOL "Enable MOM6")
set(ENABLE_CICE     ON  CACHE BOOL "Enable CICE6")
set(ENABLE_WW3      ON  CACHE BOOL "Enable WW3")

message(STATUS "Components")
message(STATUS "  - MOM6 ${ENABLE_MOM6}")
message(STATUS "  - CICE ${ENABLE_CICE}")
message(STATUS "  - WW3  ${ENABLE_WW3}")


# Build options
set(OPENMP          ON  CACHE BOOL "Enable OpenMP threading")

message(STATUS "Build options")
message(STATUS "  - OPENMP ${OPENMP}")

# Common compiler flags and definitions
add_compile_definitions(
  CESMCOUPLED
)

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fbacktrace -ffree-line-length-none")
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

# Data component (CDEPS)
add_subdirectory(CDEPS)

# Ocean component (MOM6)
if(ENABLE_MOM6)
  add_subdirectory(MOM6)
  list(APPEND COMPONENTS_TARGETS mom6)
else()
  list(APPEND COMPONENTS_TARGETS cdeps_docn)
endif()

# Sea-ice component (CICE6)
if(ENABLE_CICE)
  add_subdirectory(CICE)
  list(APPEND COMPONENTS_TARGETS cice)
else()
  list(APPEND COMPONENTS_TARGETS cdeps_dice)
endif()

# Wave component (WW3)
if(ENABLE_WW3)
  add_subdirectory(WW3)
  list(APPEND COMPONENTS_TARGETS ww3)
else()
  list(APPEND COMPONENTS_TARGETS cdeps_dwav)
endif()

# Mediator component (CMEPS)
add_subdirectory(CMEPS)

# Driver

# We use the CESM driver from CMEPS
add_library(cesm_driver STATIC
  CMEPS/CMEPS/cesm/driver/esm.F90
  CMEPS/CMEPS/cesm/driver/ensemble_driver.F90
  CMEPS/CMEPS/cesm/driver/esm_time_mod.F90
)
target_link_libraries(cesm_driver PUBLIC share ${COMPONENTS_TARGETS} cmeps cdeps_common esmf PIO::PIO_Fortran)
set_target_properties(cesm_driver PROPERTIES Fortran_MODULE_DIRECTORY
  ${CMAKE_CURRENT_BINARY_DIR}/mod)
target_include_directories(cesm_driver PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/mod>
                                              $<INSTALL_INTERFACE:mod>)

target_compile_definitions(cesm_driver PRIVATE MED_PRESENT
                                               ATM_PRESENT
                                               ICE_PRESENT
                                               LND_PRESENT
                                               OCN_PRESENT
                                               WAV_PRESENT
                                               ROF_PRESENT
                                               $<$<CONFIG:Debug>:DEBUG>
)

add_executable(access-om3 CMEPS/CMEPS/cesm/driver/esmApp.F90)
target_link_libraries(access-om3 PRIVATE cesm_driver esmf PIO::PIO_Fortran NetCDF::NetCDF_Fortran)
