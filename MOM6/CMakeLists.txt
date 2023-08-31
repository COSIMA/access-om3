### MOM6 Fortran compiler flags
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  set(fortran_compile_flags -fdefault-real-8 -fdefault-double-8)
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  set(fortran_compile_flags -r8)
endif()

option(SYMMETRIC "Use symmetric memory" OFF)
if(SYMMETRIC)
  set(MOM_memory "dynamic_symmetric")
else()
  set(MOM_memory "dynamic_nonsymmetric")
endif()

include("mom6_files.cmake")

### Create target library and set PUBLIC interfaces on the library
add_fortran_library(mom6 mod STATIC ${mom6_src_files})
target_include_directories(mom6 PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/MOM6/config_src/memory/${MOM_memory}>
                                       $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/MOM6/src/framework>)
target_compile_options(mom6 PRIVATE "$<$<COMPILE_LANGUAGE:Fortran>:${fortran_compile_flags}>")
target_link_libraries(mom6 PUBLIC share
                                  cmeps
                                  FMS::fms_r8
                                  esmf
                                  NetCDF::NetCDF_Fortran)
add_patched_source(mom6 MOM6/config_src/drivers/nuopc_cap/mom_cap.F90)
