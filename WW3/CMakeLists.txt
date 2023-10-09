# Switch definitions (taken from CESM)
add_compile_definitions(W3_CESMCOUPLED)
add_compile_definitions(W3_NCO)
add_compile_definitions(W3_DIST)
add_compile_definitions(W3_MPI)
add_compile_definitions(W3_PR3)
add_compile_definitions(W3_UQ)
add_compile_definitions(W3_FLX0)
add_compile_definitions(W3_SEED)
add_compile_definitions(W3_ST4)
add_compile_definitions(W3_STAB0)
add_compile_definitions(W3_NL1)
add_compile_definitions(W3_BT1)
add_compile_definitions(W3_DB1)
add_compile_definitions(W3_MLIM)
add_compile_definitions(W3_FLD2)
add_compile_definitions(W3_TR0)
add_compile_definitions(W3_BS0)
add_compile_definitions(W3_RWND)
add_compile_definitions(W3_WNX1)
add_compile_definitions(W3_WNT1)
add_compile_definitions(W3_CRX1)
add_compile_definitions(W3_CRT1)
add_compile_definitions(W3_O0)
add_compile_definitions(W3_O1)
add_compile_definitions(W3_O2)
add_compile_definitions(W3_O3)
add_compile_definitions(W3_O4)
add_compile_definitions(W3_O5)
add_compile_definitions(W3_O6)
add_compile_definitions(W3_O7)
add_compile_definitions(W3_O14)
add_compile_definitions(W3_O15)
add_compile_definitions(W3_IS0)
add_compile_definitions(W3_REF0)
add_compile_definitions(W3_NOGRB)
add_compile_definitions(W3_IC0)

add_compile_definitions(ENDIANNESS="big_endian")

set_property(SOURCE WW3/model/src/w3initmd.F90
  APPEND
  PROPERTY COMPILE_DEFINITIONS
  "__WW3_SWITCHES__=\'\'"
  )

# Too many files to list, so include them via this file
include("ww3_files.cmake")

### Create target library and set PUBLIC interfaces on the library
add_fortran_library(ww3 mod STATIC ${ww3_src_files})
target_link_libraries(ww3 PUBLIC  esmf
                                  NetCDF::NetCDF_Fortran)

# WW3 executables
foreach(EXE ww3_grid ww3_strt ww3_ounf ww3_outf ww3_outp)
    add_executable(${EXE} WW3/model/src/${EXE}.F90)
    target_link_libraries(${EXE} PRIVATE ww3)
endforeach()
