cmake_minimum_required(VERSION 3.18)

project(ACCESS-OM3
        VERSION 0.1
        LANGUAGES C Fortran)

list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake)
include(${CMAKE_SOURCE_DIR}/cmake/FortranLib.cmake)
include(${CMAKE_SOURCE_DIR}/cmake/AddPatchedSource.cmake)

# Configurations to build
list(APPEND KNOWN_CONFIGURATIONS MOM6 CICE6 WW3 MOM6-CICE6 CICE6-WW3 MOM6-WW3 MOM6-CICE6-WW3)

set(ENABLE_MOM6           OFF CACHE BOOL "Build MOM6 configuration")
set(ENABLE_CICE6          OFF CACHE BOOL "Build CICE6 configuration")
set(ENABLE_WW3            OFF CACHE BOOL "Build WW3 configuration")
set(ENABLE_MOM6-CICE6     ON  CACHE BOOL "Build MOM6-CICE6 configuration")
set(ENABLE_CICE6-WW3      ON  CACHE BOOL "Build CICE6-WW3 configuration")
set(ENABLE_MOM6-WW3       OFF CACHE BOOL "Build MOM6-WW3 configuration")
set(ENABLE_MOM6-CICE6-WW3 ON  CACHE BOOL "Build MOM6-CICE6-WW3 configuration")

message(STATUS "Configurations")
message(STATUS "  - MOM6           ${ENABLE_MOM6}")
message(STATUS "  - CICE6          ${ENABLE_CICE6}")
message(STATUS "  - WW3            ${ENABLE_WW3}")
message(STATUS "  - MOM6-CICE6     ${ENABLE_MOM6-CICE6}")
message(STATUS "  - CICE6-WW3      ${ENABLE_CICE6-WW3}")
message(STATUS "  - MOM6-WW3       ${ENABLE_MOM6-WW3}")
message(STATUS "  - MOM6-CICE6-WW3 ${ENABLE_MOM6-CICE6-WW3}")

# Do not build components that are not going to be used
if(ENABLE_MOM6 OR ENABLE_MOM6-CICE6 OR ENABLE_MOM6-WW3 OR ENABLE_MOM6-CICE6-WW3)
  set(MOM6 ON FORCE)
else()
  set(MOM6 OFF FORCE)
endif()
if(ENABLE_CICE6 OR ENABLE_MOM6-CICE6 OR ENABLE_CICE6-WW3 OR ENABLE_MOM6-CICE6-WW3)
  set(CICE6 ON FORCE)
else()
  set(CICE6 OFF FORCE)
endif()
if(ENABLE_WW3 OR ENABLE_MOM6-WW3 OR ENABLE_CICE6-WW3 OR ENABLE_MOM6-CICE6-WW3)
  set(WW3 ON FORCE)
else()
  set(WW3 OFF FORCE)
endif()

# Build options
set(OPENMP          OFF  CACHE BOOL "Enable OpenMP threading")

message(STATUS "Build options")
message(STATUS "  - OPENMP ${OPENMP}")

# Common compiler flags and definitions
add_compile_definitions(
  CESMCOUPLED
)

if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fbacktrace -fconvert=big-endian -ffree-line-length-none -ffixed-line-length-none")
  if(${CMAKE_Fortran_COMPILER_VERSION} VERSION_GREATER_EQUAL 10)
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fallow-argument-mismatch")
  endif()
  set(CMAKE_Fortran_FLAGS_RELEASE "-O")
  set(CMAKE_Fortran_FLAGS_DEBUG "-g -Wall -Og -ffpe-trap=zero,overflow -fcheck=bounds")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -qno-opt-dynamic-align  -convert big_endian -assume byterecl -ftz -traceback -assume realloc_lhs -fp-model source")
  set(CMAKE_Fortran_FLAGS_RELEASE "-O2 -debug minimal")
  set(CMAKE_Fortran_FLAGS_DEBUG "-O0 -g -check uninit -check bounds -check pointers -fpe0 -check noarg_temp_created")
else()
  message(WARNING "Fortran compiler with ID ${CMAKE_Fortran_COMPILER_ID} will be used with CMake default options")
endif()

if(CMAKE_C_COMPILER_ID MATCHES "GNU")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -std=gnu99")
  set(CMAKE_C_FLAGS_RELEASE "-O")
  set(CMAKE_C_FLAGS_DEBUG "-g -Wall -Og -fbacktrace -ffpe-trap=invalid,zero,overflow -fcheck=bounds")
elseif(CMAKE_C_COMPILER_ID MATCHES "Intel")
  set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} -traceback -qno-opt-dynamic-align -fp-model precise -std=gnu99")
  set(CMAKE_C_FLAGS_RELEASE "-O2 -debug minimal")
  set(CMAKE_C_FLAGS_DEBUG "-O0 -g")
else()
  message(WARNING "C compiler with ID ${CMAKE_C_COMPILER_ID} will be used with CMake default options")
endif()


# Find dependencies
find_package(MPI REQUIRED)
if(OPENMP)
  find_package(OpenMP REQUIRED)
endif()
find_package(NetCDF 4.7.3 REQUIRED Fortran)
find_package(ESMF 8.3.0 MODULE REQUIRED)
find_package(fms COMPONENTS R8 REQUIRED)
find_package(PIO 2.5.3 REQUIRED COMPONENTS C Fortran)

# Some code shared by several components
add_subdirectory(share)

# Components

# Data component (CDEPS)
add_subdirectory(CDEPS)

# Ocean component (MOM6)
if(MOM6)
  add_subdirectory(MOM6)
endif()

# Sea-ice component (CICE6)
if(CICE6)
  add_subdirectory(CICE)
endif()

# Wave component (WW3)
if(WW3)
  add_subdirectory(WW3)
endif()

# Mediator component (CMEPS)
add_subdirectory(CMEPS)

# Build executable for each enabled configuration
foreach(CONF IN LISTS KNOWN_CONFIGURATIONS)
  if(NOT ENABLE_${CONF})
    continue()
  endif()

  set(COMPONENTS_TARGETS "")
  if(${CONF} MATCHES MOM6)
    list(APPEND COMPONENTS_TARGETS mom6)
  else()
    list(APPEND COMPONENTS_TARGETS cdeps_docn)
  endif()
  if(${CONF} MATCHES CICE6)
    list(APPEND COMPONENTS_TARGETS cice)
  else()
    list(APPEND COMPONENTS_TARGETS cdeps_dice)
  endif()
  if(${CONF} MATCHES WW3)
    list(APPEND COMPONENTS_TARGETS ww3)
  else()
    list(APPEND COMPONENTS_TARGETS cdeps_dwav)
  endif()

  # We use the CESM driver from CMEPS
  add_fortran_library(cesm_driver_${CONF} cesm_driver/${CONF}/mod STATIC
    CMEPS/CMEPS/cesm/driver/esm.F90
    CMEPS/CMEPS/cesm/driver/ensemble_driver.F90
    CMEPS/CMEPS/cesm/driver/esm_time_mod.F90
  )
  target_link_libraries(cesm_driver_${CONF} PUBLIC share ${COMPONENTS_TARGETS} cmeps cdeps_common cdeps_drof cdeps_datm esmf PIO::PIO_Fortran)
  target_compile_definitions(cesm_driver_${CONF} PRIVATE MED_PRESENT
                                                         ATM_PRESENT
                                                         ICE_PRESENT
                                                         OCN_PRESENT
                                                         WAV_PRESENT
                                                         ROF_PRESENT
                                                         $<$<CONFIG:Debug>:DEBUG>
  )

  add_executable(access-om3-${CONF} CMEPS/CMEPS/cesm/driver/esmApp.F90)
  target_link_libraries(access-om3-${CONF} PRIVATE cesm_driver_${CONF} esmf PIO::PIO_Fortran NetCDF::NetCDF_Fortran)
endforeach()
