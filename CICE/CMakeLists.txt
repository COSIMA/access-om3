### CICE Fortran compiler flags
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  set(fortran_compile_flags -fconvert=big-endian -ffree-line-length-none)
  set(fortran_compile_flags_debug -Wall -Wextra -fcheck=bounds -ffpe-trap=invalid,zero,overflow,underflow)
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  set(fortran_compile_flags -FR -convert big_endian -assume byterecl -ftz -align array64byte -xHOST)
  set(fortran_compile_flags_release -fp-model precise)
  set(fortran_compile_flags_debug "SHELL:-check uninit" "SHELL:-check bounds" "SHELL:-check nopointer" -fpe0 "SHELL:-check noarg_temp_created" -init=snan,arrays)
endif()

### CICE C compiler flags
if(CMAKE_C_COMPILER_ID MATCHES "GNU")
  set(c_compile_flags_release -O3)
elseif(CMAKE_C_COMPILER_ID MATCHES "Intel")
  set(c_compile_flags -xHOST)
  set(c_compile_flags_release -O2 -fp-model precise)
elseif(CMAKE_C_COMPILER_ID MATCHES "Clang")
  set(c_compile_flags_release -O3)
endif()

# Configuration Options
set(CICE_IO "NetCDF" CACHE STRING "CICE OPTIONS: Choose IO options.")
set_property(CACHE CICE_IO PROPERTY STRINGS "NetCDF" "PIO" "Binary")

# Too many files to list, so include them via this file
include("cice_files.cmake")

# Collect source files for library
list(APPEND lib_src_files
  ${cice_shared_files}
  ${cice_shared_files_c}
  ${icepack_files}
  ${cice_mpi_comm_files}
  ${cice_nuopc_cmeps_driver_files})

list(APPEND _cice_defs FORTRANUNDERSCORE
                       coupled)

# Select IO source files based on CICE_IO
if(CICE_IO MATCHES "NetCDF")
  list(APPEND lib_src_files ${cice_netcdf_io_files})
  list(APPEND _cice_defs USE_NETCDF)
elseif(CICE_IO MATCHES "PIO")
  list(APPEND lib_src_files ${cice_pio2_io_files})
  list(APPEND _cice_defs USE_NETCDF)
elseif(CICE_IO MATCHES "Binary")
  list(APPEND lib_src_files ${cice_binary_io_files})
endif()

### Create target library and set PUBLIC interfaces on the library
add_library(cice STATIC ${lib_src_files})
set_target_properties(cice PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod)
target_compile_definitions(cice PUBLIC "${_cice_defs}")
target_include_directories(cice PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/mod>
                                       $<INSTALL_INTERFACE:mod>)
target_compile_options(cice PRIVATE "$<$<COMPILE_LANGUAGE:Fortran>:${fortran_compile_flags}>")
target_compile_options(cice PRIVATE "$<$<AND:$<CONFIG:Debug>,$<COMPILE_LANGUAGE:Fortran>>:${fortran_compile_flags_debug}>")
target_compile_options(cice PRIVATE "$<$<AND:$<CONFIG:Release>,$<COMPILE_LANGUAGE:Fortran>>:${fortran_compile_flags_release}>")
target_compile_options(cice PRIVATE "$<$<COMPILE_LANGUAGE:C>:${c_compile_flags}>")
target_compile_options(cice PRIVATE "$<$<AND:$<CONFIG:Release>,$<COMPILE_LANGUAGE:C>>:${c_compile_flags_release}>")
target_link_libraries(cice PUBLIC share cdeps esmf)

if(CICE_IO MATCHES "^(NetCDF|PIO)$")
  target_link_libraries(cice PUBLIC NetCDF::NetCDF_Fortran)
  if(CICE_IO MATCHES "PIO")
    target_link_libraries(cice PUBLIC PIO::PIO_Fortran)
  endif()
endif()
if(OpenMP_Fortran_FOUND)
  target_link_libraries(cice PRIVATE OpenMP::OpenMP_Fortran)
endif()
