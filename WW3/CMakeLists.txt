### WW3 Fortran compiler flags
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  set(fortran_compile_flags -fno-second-underscore)
  if(${CMAKE_Fortran_COMPILER_VERSION} VERSION_GREATER_EQUAL 10)
    set(fortran_compile_flags -fallow-argument-mismatch)
  endif()
  set(fortran_compile_flags_release -O3)
  set(fortran_compile_flags_debug -Wall -Wno-unused-label -fcheck=all -ffpe-trap=invalid,zero,overflow -frecursive)
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  set(fortran_compile_flags  -no-fma -ip -i4 -real-size 32 -fp-model precise -assume byterecl -fno-alias -fno-fnalias -sox)
  set(fortran_compile_flags_release -O3)
  set(fortran_compile_flags_debug "SHELL:-debug all" "SHELL:-warn all" "SHELL:-check all" -check noarg_temp_created -fp-stack-check -heap-arrays -fpe0)
endif()

# Switch definitions (taken from CESM)
add_compile_definitions(W3_CESMCOUPLED)
add_compile_definitions(W3_NCO)
add_compile_definitions(W3_DIST)
add_compile_definitions(W3_MPI)
if (OPENMP)
  add_compile_definitions(W3_OMPG)
endif()
add_compile_definitions(W3_OMPH)
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
add_compile_definitions(W3_IC4)
add_compile_definitions(W3_IS0)
add_compile_definitions(W3_REF0)
add_compile_definitions(ENDIANNESS="big_endian")

set_property(SOURCE WW3/model/src/w3initmd.F90
  APPEND
  PROPERTY COMPILE_DEFINITIONS
  "__WW3_SWITCHES__=\'\'"
  )

# Too many files to list, so include them via this file
include("ww3_files.cmake")

### Create target library and set PUBLIC interfaces on the library
add_library(ww3 STATIC ${ww3_src_files})
set_target_properties(ww3 PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod)
target_include_directories(ww3 PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/mod>)
target_compile_options(ww3 PRIVATE "$<$<COMPILE_LANGUAGE:Fortran>:${fortran_compile_flags}>")
target_compile_options(ww3 PRIVATE "$<$<AND:$<CONFIG:Debug>,$<COMPILE_LANGUAGE:Fortran>>:${fortran_compile_flags_debug}>")
target_compile_options(ww3 PRIVATE "$<$<AND:$<CONFIG:Release>,$<COMPILE_LANGUAGE:Fortran>>:${fortran_compile_flags_release}>")
target_link_libraries(ww3 PUBLIC  esmf
                                  NetCDF::NetCDF_Fortran)