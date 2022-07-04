# MOM6 Fortran compiler flags
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -fdefault-real-8 -fdefault-double-8")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} -i4 -r8")
else()
  message(WARNING "Fortran compiler with ID ${CMAKE_Fortran_COMPILER_ID} will be used with CMake default options")
endif()


option(SYMMETRIC "Use symmetric memory" ON)
if(SYMMETRIC)
  set(MOM_memory "dynamic_symmetric")
else()
  set(MOM_memory "dynamic_nonsymmetric")
endif()


include("mom6_files.cmake")

add_executable(MOM6 ${MOM6_src})

target_include_directories(MOM6 PRIVATE $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/MOM6/config_src/memory/${MOM_memory}>
                                        $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/MOM6/src/framework>)

target_link_libraries(MOM6 PRIVATE FMS::fms_r8
                                   NetCDF::NetCDF_Fortran)


# Installation
include(GNUInstallDirs)

install(
  TARGETS MOM6
  EXPORT  MOM6-targets
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
)
