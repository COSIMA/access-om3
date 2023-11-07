### Global definitions, flags, etc

## List of switches
list(APPEND switches "CESMCOUPLED" "NCO" "DIST" "MPI" "PR3" "UQ" "FLX4" "SEED" "ST6" "STAB0" "NL1" "BT1" "DB1" "MLIM" "TR0" "BS0" "RWND" "WNX1" "WNT1" "CRX1" "CRT1" "O0" "O1" "O2" "O3" "O4" "O5" "O6" "O7" "O14" "O15" "IS0" "REF0" "NOGRB" "IC0")

## Global compile definitions
foreach(switch ${switches})
  add_compile_definitions(W3_${switch})
endforeach()

### Targets

## WW3 library
add_fortran_library(OM3_ww3 mod STATIC)
target_compile_definitions(OM3_ww3 PRIVATE ENDIANNESS="big_endian")
set_property(SOURCE WW3/model/src/w3initmd.F90
  APPEND
  PROPERTY COMPILE_DEFINITIONS
  "__WW3_SWITCHES__=\'\'"
)
target_link_libraries(OM3_ww3
  PUBLIC esmf
  PRIVATE NetCDF::NetCDF_Fortran
)
# Process switches and get list of extra source files
include(${CMAKE_CURRENT_SOURCE_DIR}/cmake/check_switches.cmake)
check_switches("${switches}" switch_files)
message(VERBOSE "WW3 switch files: ${switch_files}")
target_sources(OM3_ww3 PRIVATE
  WW3/model/src/constants.F90
  WW3/model/src/w3adatmd.F90
  WW3/model/src/w3arrymd.F90
  WW3/model/src/w3bullmd.F90
  WW3/model/src/w3cspcmd.F90
  WW3/model/src/w3dispmd.F90
  WW3/model/src/w3fldsmd.F90
  WW3/model/src/w3gdatmd.F90
  WW3/model/src/w3gridmd.F90
  WW3/model/src/w3gsrumd.F90
  WW3/model/src/w3idatmd.F90
  WW3/model/src/w3initmd.F90
  WW3/model/src/w3iobcmd.F90
  WW3/model/src/w3iogomd.F90
  WW3/model/src/w3iogrmd.F90
  WW3/model/src/w3iopomd.F90
  WW3/model/src/w3iorsmd.F90
  WW3/model/src/w3iosfmd.F90
  WW3/model/src/w3iotrmd.F90
  WW3/model/src/w3macros.h
  WW3/model/src/w3metamd.F90
  WW3/model/src/w3nmlbouncmd.F90
  WW3/model/src/w3nmlboundmd.F90
  WW3/model/src/w3nmlgridmd.F90
  WW3/model/src/w3nmlmultimd.F90
  WW3/model/src/w3nmlounfmd.F90
  WW3/model/src/w3nmlounpmd.F90
  WW3/model/src/w3nmlprncmd.F90
  WW3/model/src/w3nmlshelmd.F90
  WW3/model/src/w3nmltrncmd.F90
  WW3/model/src/w3nmluprstrmd.F90
  WW3/model/src/w3odatmd.F90
  WW3/model/src/w3parall.F90
  WW3/model/src/w3partmd.F90
  WW3/model/src/w3servmd.F90
  WW3/model/src/w3srcemd.F90
  WW3/model/src/w3strkmd.F90
  WW3/model/src/w3timemd.F90
  WW3/model/src/w3triamd.F90
  WW3/model/src/w3updtmd.F90
  WW3/model/src/w3wavemd.F90
  WW3/model/src/w3wdasmd.F90
  WW3/model/src/w3wdatmd.F90
  WW3/model/src/wmfinlmd.F90
  WW3/model/src/wmgridmd.F90
  WW3/model/src/wminiomd.F90
  WW3/model/src/wminitmd.F90
  WW3/model/src/wmiopomd.F90
  WW3/model/src/wmmdatmd.F90
  WW3/model/src/wmunitmd.F90
  WW3/model/src/wmupdtmd.F90
  WW3/model/src/w3tidemd.F90
  WW3/model/src/wav_grdout.F90
  WW3/model/src/w3iogoncdmd.F90
  WW3/model/src/wav_shr_flags.F90

  WW3/model/src/wav_kind_mod.F90
  WW3/model/src/wav_shr_mod.F90
  WW3/model/src/wav_shel_inp.F90
  WW3/model/src/wav_comp_nuopc.F90
  WW3/model/src/wav_import_export.F90

  ${switch_files}
)

target_include_directories(OM3_ww3 PRIVATE ${CMAKE_CURRENT_SOURCE_DIR}/WW3/model/src)

#patched source
add_patched_source(OM3_ww3 WW3/model/src/w3wavemd.F90)

## Utilities

# ww3_grid
add_executable(OM3_ww3_grid WW3/model/src/ww3_grid.F90)
set_target_properties(OM3_ww3_grid PROPERTIES
  LINKER_LANGUAGE Fortran
  OUTPUT_NAME ww3_grid
)
target_link_libraries(OM3_ww3_grid PRIVATE OM3_ww3)

# ww3_strt
add_executable(OM3_ww3_strt WW3/model/src/ww3_strt.F90)
set_target_properties(OM3_ww3_strt PROPERTIES
  LINKER_LANGUAGE Fortran
  OUTPUT_NAME ww3_strt
)
target_link_libraries(OM3_ww3_strt PRIVATE OM3_ww3)

# ww3_outf
add_executable(OM3_ww3_outf WW3/model/src/ww3_outf.F90)
set_target_properties(OM3_ww3_outf PROPERTIES
  LINKER_LANGUAGE Fortran
  OUTPUT_NAME ww3_outf
)
target_link_libraries(OM3_ww3_outf PRIVATE OM3_ww3)

# ww3_ounf
add_executable(OM3_ww3_ounf)
target_sources(OM3_ww3_ounf PRIVATE
  WW3/model/src/ww3_ounf.F90
  WW3/model/src/w3ounfmetamd.F90
)
set_target_properties(OM3_ww3_ounf PROPERTIES
  LINKER_LANGUAGE Fortran
  OUTPUT_NAME ww3_ounf
)
target_link_libraries(OM3_ww3_ounf PRIVATE OM3_ww3)

# ww3_outp
add_executable(OM3_ww3_outp WW3/model/src/ww3_outp.F90)
set_target_properties(OM3_ww3_outp PROPERTIES
  LINKER_LANGUAGE Fortran
  OUTPUT_NAME ww3_outp
)
target_link_libraries(OM3_ww3_outp PRIVATE OM3_ww3)


## Installs
if(OM3_BIN_INSTALL)
  install(TARGETS OM3_ww3_grid OM3_ww3_strt OM3_ww3_ounf OM3_ww3_outp
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
endif()
