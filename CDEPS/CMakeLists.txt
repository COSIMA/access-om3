# CDEPS compiler flags
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  add_compile_definitions(CPRGNU)
  set(fortran_compile_flags -ffree-line-length-none)
  if(${CMAKE_Fortran_COMPILER_VERSION} VERSION_GREATER_EQUAL 10)
    list(APPEND fortran_compile_flags -fallow-argument-mismatch -fallow-invalid-boz)
  endif()
  set(fortran_compile_flags_debug -fcheck=bounds -ffpe-trap=invalid,zero,overflow,underflow)
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  add_compile_definitions(CPRINTEL)
  set(fortran_compile_flags -O -assume realloc_lhs)
  set(fortran_compile_flags_release -fp-model precise)
  set(fortran_compile_flags_debug -check "SHELL:-check noarg_temp_created" "SHELL:-check nopointer" -fpe0 -ftrapuv -init=snan,arrays)
endif()

# Too many files to list, so include them via this file
include("cdeps_files.cmake")

add_library(cdeps STATIC ${cdeps_generated_files} ${cdeps_src_files})

set_target_properties(cdeps PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod)
target_compile_definitions(cdeps PUBLIC "DISABLE_FoX")
target_include_directories(cdeps PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/mod>
  $<INSTALL_INTERFACE:mod>)
target_compile_options(cdeps PUBLIC "$<$<COMPILE_LANGUAGE:Fortran>:${fortran_compile_flags}>")
target_compile_options(cdeps PUBLIC "$<$<AND:$<CONFIG:Debug>,$<COMPILE_LANGUAGE:Fortran>>:${fortran_compile_flags_debug}>")
target_compile_options(cdeps PUBLIC "$<$<AND:$<CONFIG:Release>,$<COMPILE_LANGUAGE:Fortran>>:${fortran_compile_flags_release}>")
target_link_libraries(cdeps PUBLIC esmf PIO::PIO_Fortran)
list(APPEND LIB_TARGETS cdeps)
