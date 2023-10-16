# Commom source files. Too many files to list, so include them via this file
include("ww3_files.cmake")

# List of switches
list(APPEND switches "CESMCOUPLED" "NCO" "DIST" "MPI" "PR3" "UQ" "FLX4" "SEED" "ST6" "STAB0" "NL1" "BT1" "DB1" "MLIM" "TR0" "BS0" "RWND" "WNX1" "WNT1" "CRX1" "CRT1" "O0" "O1" "O2" "O3" "O4" "O5" "O6" "O7" "O14" "O15" "IS0" "REF0" "NOGRB" "IC0")

# Process switches and get list of extra source files
include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/check_switches.cmake)
check_switches("${switches}" switch_files)

# Compile definitions
foreach(switch ${switches})
  add_compile_definitions(W3_${switch})
endforeach() 

add_compile_definitions(ENDIANNESS="big_endian")

set_property(SOURCE WW3/model/src/w3initmd.F90
  APPEND
  PROPERTY COMPILE_DEFINITIONS
  "__WW3_SWITCHES__=\'\'"
)

message(VERBOSE "WW3 common source files : ${ww3_src_files}")
message(VERBOSE "WW3 switch files: ${switch_files}")

### Create target library and set PUBLIC interfaces on the library
add_fortran_library(OM3_ww3 mod STATIC ${ww3_src_files} ${switch_files})
target_link_libraries(OM3_ww3 PUBLIC esmf
                                     NetCDF::NetCDF_Fortran)

# WW3 executables
foreach(EXE ww3_grid ww3_strt ww3_ounf ww3_outf ww3_outp)
  add_executable(${EXE} ${${EXE}_src_files})
  target_link_libraries(${EXE} PRIVATE OM3_ww3)
endforeach()
