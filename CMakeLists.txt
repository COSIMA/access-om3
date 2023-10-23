cmake_minimum_required(VERSION 3.18)
# CMake version compatibility

#[==============================================================================[
#                         Basic project definition                              #
#]==============================================================================]

project(ACCESS-OM3 VERSION "0.2.0"
                  HOMEPAGE_URL https://github.com/COSIMA/access-om3
                  DESCRIPTION "Global coupled ocean - sea ice - wave model"
                  LANGUAGES C Fortran)

#[==============================================================================[
#                                   Options                                     #
#]==============================================================================]

# Configurations to build
list(APPEND KnownConfigurations MOM6 CICE6 WW3 MOM6-CICE6 CICE6-WW3 MOM6-WW3 MOM6-CICE6-WW3)

option(OM3_ENABLE_MOM6           "Build MOM6 configuration" OFF)
option(OM3_ENABLE_CICE6          "Build CICE6 configuration" OFF)
option(OM3_ENABLE_WW3            "Build WW3 configuration" OFF)
option(OM3_ENABLE_MOM6-WW3       "Build MOM6-WW3 configuration" OFF)
option(OM3_ENABLE_MOM6-CICE6     "Build MOM6-CICE6 configuration" ON)
option(OM3_ENABLE_CICE6-WW3      "Build CICE6-WW3 configuration" ON)
option(OM3_ENABLE_MOM6-CICE6-WW3 "Build MOM6-CICE6-WW3 configuration" ON)

message(STATUS "Configurations")
message(STATUS "  - MOM6              ${OM3_ENABLE_MOM6}")
message(STATUS "  - CICE6             ${OM3_ENABLE_CICE6}")
message(STATUS "  - WW3               ${OM3_ENABLE_WW3}")
message(STATUS "  - MOM6-WW3          ${OM3_ENABLE_MOM6-WW3}")
message(STATUS "  - MOM6-CICE6        ${OM3_ENABLE_MOM6-CICE6}")
message(STATUS "  - CICE6-WW3         ${OM3_ENABLE_CICE6-WW3}")
message(STATUS "  - MOM6-CICE6-WW3    ${OM3_ENABLE_MOM6-CICE6-WW3}")

# Build options
option(OM3_BIN_INSTALL   "Instal ACCESS-OM3 executables" ${PROJECT_IS_TOP_LEVEL})
option(OM3_LIB_INSTALL   "Instal ACCESS-OM3 libraries" OFF)
option(OM3_OPENMP        "Enable OpenMP threading" OFF)
option(OM3_MOM_SYMMETRIC "Use symmetric memory" OFF)
set(OM3_CICE_IO   "PIO" CACHE STRING "CICE OPTIONS: Choose IO options.")
set_property(CACHE OM3_CICE_IO PROPERTY STRINGS "NetCDF" "PIO" "Binary")

message(STATUS "Build options")
message(STATUS "  - OM3_BIN_INSTALL   ${OM3_BIN_INSTALL}")
message(STATUS "  - OM3_LIB_INSTALL   ${OM3_LIB_INSTALL}")
message(STATUS "  - OM3_OPENMP        ${OM3_OPENMP}")
message(STATUS "  - OM3_MOM_SYMMETRIC ${OM3_MOM_SYMMETRIC}")
message(STATUS "  - OM3_CICE_IO       ${OM3_CICE_IO}")


#[==============================================================================[
#                           Project configuration                               #
#]==============================================================================]

list(APPEND CMAKE_MODULE_PATH ${CMAKE_SOURCE_DIR}/cmake)
include(GNUInstallDirs)

# Include some custom cmake modules
include(FortranLib)
include(AddPatchedSource)

## Configure project variables

# Do not build components that are not going to be used
if(OM3_ENABLE_MOM6 OR OM3_ENABLE_MOM6-CICE6 OR OM3_ENABLE_MOM6-WW3 OR OM3_ENABLE_MOM6-CICE6-WW3)
  set(OM3_BUILD_MOM6 ON)
else()
  set(OM3_BUILD_MOM6 OFF)
endif()
if(OM3_ENABLE_CICE6 OR OM3_ENABLE_MOM6-CICE6 OR OM3_ENABLE_CICE6-WW3 OR OM3_ENABLE_MOM6-CICE6-WW3)
  set(OM3_BUILD_CICE6 ON)
else()
  set(OM3_BUILD_CICE6 OFF)
endif()
if(OM3_ENABLE_WW3 OR OM3_ENABLE_MOM6-WW3 OR OM3_ENABLE_CICE6-WW3 OR OM3_ENABLE_MOM6-CICE6-WW3)
  set(OM3_BUILD_WW3 ON)
else()
  set(OM3_BUILD_WW3 OFF)
endif()
message(STATUS "Components to build")
message(STATUS "  - OM3_BUILD_MOM6    ${OM3_BUILD_MOM6}")
message(STATUS "  - OM3_BUILD_CICE6   ${OM3_BUILD_CICE6}")
message(STATUS "  - OM3_BUILD_WW3     ${OM3_BUILD_WW3}")

# Common compiler flags and definitions
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

add_compile_definitions(
  CESMCOUPLED
)

#[==============================================================================[
#                              External packages                                #
#]==============================================================================]

# Find dependencies
find_package(MPI REQUIRED)
if(OM3_OPENMP)
  find_package(OpenMP REQUIRED)
endif()
find_package(FoX 4.1.2 REQUIRED)
find_package(NetCDF 4.7.3 REQUIRED Fortran)
find_package(ESMF 8.3.0 MODULE REQUIRED)
find_package(fms COMPONENTS R8 REQUIRED)
find_package(PIO 2.5.3 REQUIRED COMPONENTS C Fortran)

#[==============================================================================[
#                             Main definitions                                  #
#]==============================================================================]

# Some code shared by several components
add_subdirectory(share)

## Components

# Data component (CDEPS)
add_subdirectory(CDEPS)

# Ocean component (MOM6)
if(OM3_BUILD_MOM6)
  add_subdirectory(MOM6)
endif()

# Sea-ice component (CICE6)
if(OM3_BUILD_CICE6)
  add_subdirectory(CICE)
endif()

# Wave component (WW3)
if(OM3_BUILD_WW3)
  add_subdirectory(WW3)
endif()

# Mediator component (CMEPS)
add_subdirectory(CMEPS)

# Add executable for each enabled configuration
foreach(CONF IN LISTS KnownConfigurations)
  if(NOT OM3_ENABLE_${CONF})
    continue()
  endif()

  set(ComponentsTargets "")
  if(OM3_${CONF} MATCHES MOM6)
    list(APPEND ComponentsTargets OM3_mom6)
  else()
    list(APPEND ComponentsTargets OM3_cdeps_docn)
  endif()
  if(OM3_${CONF} MATCHES CICE6)
    list(APPEND ComponentsTargets OM3_cice)
  else()
    list(APPEND ComponentsTargets OM3_cdeps_dice)
  endif()
  if(OM3_${CONF} MATCHES WW3)
    list(APPEND ComponentsTargets OM3_ww3)
  else()
    list(APPEND ComponentsTargets OM3_cdeps_dwav)
  endif()

  # We use the CESM driver from CMEPS
  add_fortran_library(OM3_cesm_driver_${CONF} OM3_cesm_driver_${CONF}/mod STATIC
    CMEPS/CMEPS/cesm/driver/esm.F90
    CMEPS/CMEPS/cesm/driver/ensemble_driver.F90
    CMEPS/CMEPS/cesm/driver/esm_time_mod.F90
  )
  target_link_libraries(OM3_cesm_driver_${CONF}
    PUBLIC esmf
    PRIVATE ${ComponentsTargets} OM3_cdeps_drof OM3_cdeps_datm OM3_cmeps OM3_nuopc_cap_share OM3_share OM3_timing
  )
  target_compile_definitions(OM3_cesm_driver_${CONF} PRIVATE MED_PRESENT
                                                             ATM_PRESENT
                                                             ICE_PRESENT
                                                             OCN_PRESENT
                                                             WAV_PRESENT
                                                             ROF_PRESENT
                                                             $<$<CONFIG:Debug>:DEBUG>
  )

  add_executable(OM3_${CONF} CMEPS/CMEPS/cesm/driver/esmApp.F90)
  target_link_libraries(OM3_${CONF} PRIVATE OM3_cesm_driver_${CONF} OM3_share esmf)

  set_target_properties(OM3_${CONF} PROPERTIES
    LINKER_LANGUAGE Fortran
    OUTPUT_NAME access-om3-${CONF}
  )
endforeach()

#[==============================================================================[
#                             Install or Export                                 #
#]==============================================================================]

## Installs
if(OM3_BIN_INSTALL)
  foreach(CONF IN LISTS KnownConfigurations)
    if(NOT OM3_ENABLE_${CONF})
      continue()
    endif()

    install(TARGETS OM3_${CONF}
      RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
    )
  endforeach()
endif()
