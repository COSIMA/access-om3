### Global definitions, flags, etc

## List of switches
list(APPEND switches "CESMCOUPLED" "DIST" "MPI" "PR1" "FLX4" "ST6" "STAB0" "LN1" "NL1" "BT1" "DB1" "MLIM" "TR0" "BS0" "RWND" "WNX1" "WNT0" "CRX1" "CRT0" "O0" "O1" "O2" "O3" "O4" "O5" "O6" "O7" "O14" "O15" "IS2" "REF0" "NOGRB" "IC3")

## Global compile definitions
foreach(switch ${switches})
  add_compile_definitions(W3_${switch})
endforeach()

### Targets

## WW3 library
add_fortran_library(OM3_ww3 mod STATIC)
add_library(AccessOM3::ww3 ALIAS OM3_ww3)
target_include_directories(OM3_ww3 PRIVATE $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/WW3/model/src>)
target_compile_definitions(OM3_ww3 PRIVATE ENDIANNESS="big_endian")
set_property(SOURCE WW3/model/src/w3initmd.F90
  APPEND
  PROPERTY COMPILE_DEFINITIONS
  "__WW3_SWITCHES__=\'\'"
)
target_link_libraries(OM3_ww3
  PUBLIC esmf MPI::MPI_Fortran
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
  WW3/model/src/w3wavemd.F90
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
  WW3/model/src/wav_wrapper_mod.F90

  ${switch_files}
)

add_patched_source(OM3_ww3 WW3/model/src/w3srcemd.F90)

## Utilities

# ww3_grid
add_executable(OM3_ww3_grid WW3/model/src/ww3_grid.F90)
set_target_properties(OM3_ww3_grid PROPERTIES
  LINKER_LANGUAGE Fortran
  OUTPUT_NAME ww3_grid
)
target_link_libraries(OM3_ww3_grid PRIVATE AccessOM3::ww3)

# ww3_strt
add_executable(OM3_ww3_strt WW3/model/src/ww3_strt.F90)
set_target_properties(OM3_ww3_strt PROPERTIES
  LINKER_LANGUAGE Fortran
  OUTPUT_NAME ww3_strt
)
target_link_libraries(OM3_ww3_strt PRIVATE AccessOM3::ww3 MPI::MPI_Fortran)

# ww3_outf
add_executable(OM3_ww3_outf WW3/model/src/ww3_outf.F90)
set_target_properties(OM3_ww3_outf PROPERTIES
  LINKER_LANGUAGE Fortran
  OUTPUT_NAME ww3_outf
)
target_link_libraries(OM3_ww3_outf PRIVATE AccessOM3::ww3)

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
target_link_libraries(OM3_ww3_ounf PRIVATE AccessOM3::ww3)

# ww3_ounp
add_executable(OM3_ww3_ounp WW3/model/src/ww3_ounp.F90)
set_target_properties(OM3_ww3_ounp PROPERTIES
  LINKER_LANGUAGE Fortran
  OUTPUT_NAME ww3_ounp
)
target_link_libraries(OM3_ww3_ounp PRIVATE AccessOM3::ww3)

### Install and Export

## Library
if(OM3_LIB_INSTALL)
  set_target_properties(OM3_ww3 PROPERTIES
    OUTPUT_NAME access-ww3
    EXPORT_NAME ww3
  )
  install(TARGETS OM3_ww3
    EXPORT AccessOM3ww3_Targets
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT AccessOM3_RunTime NAMELINK_COMPONENT AccessOM3_Development
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT AccessOM3_Development
  )
  # Fortran module files are a special case, as currently there is no standard
  # way of handling them in CMake
  target_include_directories(OM3_ww3 PUBLIC "$<INSTALL_INTERFACE:${CMAKE_INSTALL_MODULEDIR}/access-ww3>")
  get_target_property(ww3_moddir OM3_ww3 Fortran_MODULE_DIRECTORY)
  install(FILES ${ww3_moddir}/wav_comp_nuopc.mod
    DESTINATION ${CMAKE_INSTALL_MODULEDIR}/access-ww3
    COMPONENT AccessOM3_Development
  )
  install(EXPORT AccessOM3ww3_Targets
    FILE AccessOM3ww3Targets.cmake
    NAMESPACE AccessOM3::
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/AccessOM3
  )
endif()

## Utilities
if(OM3_BIN_INSTALL)
  install(TARGETS OM3_ww3_grid OM3_ww3_strt OM3_ww3_outf OM3_ww3_ounf OM3_ww3_ounp
    RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR})
endif()
