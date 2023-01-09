### MOM6 Fortran compiler flags
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  set(fortran_compile_flags -fdefault-real-8 -fdefault-double-8 -Waliasing -fcray-pointer -fconvert=big-endian -ffree-line-length-none -fno-range-check)
  set(fortran_compile_flags_debug -fcheck=bounds -ffpe-trap=invalid,zero,overflow,underflow)
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  set(fortran_compile_flags -i4 -r8 -fno-alias -auto -safe-cray-ptr -ftz -assume byterecl -sox)
  set(fortran_compile_flags_release -debug minimal -fp-model source)
  set(fortran_compile_flags_debug -check "SHELL:-check noarg_temp_created" "SHELL:-check nopointer" -fpe0 -ftrapuv -init=snan,arrays)
endif()

option(SYMMETRIC "Use symmetric memory" ON)
if(SYMMETRIC)
  set(MOM_memory "dynamic_symmetric")
else()
  set(MOM_memory "dynamic_nonsymmetric")
endif()

include("mom6_files.cmake")

### Create target library and set PUBLIC interfaces on the library
add_library(mom6 STATIC ${mom6_src_files})
set_target_properties(mom6 PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod)
target_include_directories(mom6 PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/mod>)
target_include_directories(mom6 PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/MOM6/config_src/memory/${MOM_memory}>
                                       $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/MOM6/src/framework>)
target_compile_options(mom6 PUBLIC "$<$<COMPILE_LANGUAGE:Fortran>:${fortran_compile_flags}>")
target_compile_options(mom6 PUBLIC "$<$<AND:$<CONFIG:Debug>,$<COMPILE_LANGUAGE:Fortran>>:${fortran_compile_flags_debug}>")
target_compile_options(mom6 PUBLIC "$<$<AND:$<CONFIG:Release>,$<COMPILE_LANGUAGE:Fortran>>:${fortran_compile_flags_release}>")
target_link_libraries(mom6 PUBLIC FMS::fms_r8
                                  esmf
                                  NetCDF::NetCDF_Fortran)

### Create standalone MOM6 executable
add_executable(mom6solo ${mom6_solo_src_files})
add_dependencies(mom6solo mom6)
set_target_properties(mom6solo PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod_solo)
target_include_directories(mom6solo PRIVATE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/mod>)
target_include_directories(mom6solo PRIVATE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/mod_solo>)
target_include_directories(mom6solo PRIVATE $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/MOM6/config_src/memory/${MOM_memory}>
                                            $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/MOM6/src/framework>)
target_link_libraries(mom6solo PRIVATE mom6
                                       FMS::fms_r8
                                       esmf
                                       NetCDF::NetCDF_Fortran)
