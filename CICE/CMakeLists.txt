# Configuration Options
set(CICE_IO "PIO" CACHE STRING "CICE OPTIONS: Choose IO options.")
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
                       ncdf)

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
add_fortran_library(cice mod STATIC ${lib_src_files})
target_compile_definitions(cice PUBLIC "${_cice_defs}")
target_link_libraries(cice PUBLIC share cdeps_common esmf)

if(CICE_IO MATCHES "^(NetCDF|PIO)$")
  target_link_libraries(cice PUBLIC NetCDF::NetCDF_Fortran)
  if(CICE_IO MATCHES "PIO")
    target_link_libraries(cice PUBLIC PIO::PIO_Fortran)
  endif()
endif()
if(OpenMP_Fortran_FOUND)
  target_link_libraries(cice PRIVATE OpenMP::OpenMP_Fortran)
endif()
