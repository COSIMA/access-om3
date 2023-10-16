# The following files are generated with a script acting on templates
# We include pre-generated files that are committed to the repository
list(APPEND cesm_share_generated_files
  src/shr_assert_mod.F90
  src/shr_frz_mod.F90
  src/shr_infnan_mod.F90
  src/dtypes.h
)

# Modules shared among several components
list(APPEND cesm_share_src_files
  CESM_share/include/shr_assert.h
  CESM_share/src/shr_abort_mod.F90
  CESM_share/src/shr_cal_mod.F90
  CESM_share/src/shr_const_mod.F90
  CESM_share/src/shr_file_mod.F90
  CESM_share/src/shr_kind_mod.F90
  CESM_share/src/shr_log_mod.F90
  CESM_share/src/shr_mem_mod.F90
  CESM_share/src/shr_mpi_mod.F90
  CESM_share/src/shr_nl_mod.F90
  CESM_share/src/shr_orb_mod.F90
  CESM_share/src/shr_pio_mod.F90
  CESM_share/src/shr_precip_mod.F90
  CESM_share/src/shr_reprosum_mod.F90
  CESM_share/src/shr_reprosumx86.c
  CESM_share/src/shr_strconvert_mod.F90
  CESM_share/src/shr_string_mod.F90
  CESM_share/src/shr_sys_mod.F90
  CESM_share/src/shr_timer_mod.F90
  CESM_share/src/shr_wv_sat_mod.F90
  CESM_share/src/water_isotopes/water_isotopes.F90
)

# GPTL timing library srcs
list(APPEND timing_src_files
  timing/f_wrappers.c
  timing/gptl.c
  timing/GPTLget_memusage.c
  timing/gptl.h
  timing/gptl.inc
  timing/gptl_papi.c
  timing/GPTLprint_memusage.c
  timing/GPTLutil.c
  timing/perf_mod.F90
  timing/perf_utils.F90
)

# The following file is a stub.
list(APPEND stubs_src_files
  stubs/mct_mod.F90
)

if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  # CESM reduces the precision and increases speed for the following file
  set_source_files_properties(CESM_share/src/shr_wv_sat_mod.F90 PROPERTIES COMPILE_FLAGS "-fimf-precision=low -fp-model fast")
endif()


# Collect source files for library
add_library(OM3_share STATIC ${timing_src_files} ${stubs_src_files} ${cesm_share_generated_files} ${cesm_share_src_files})

add_compile_definitions(NUOPC_INTERFACE HAVE_MPI)
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  target_compile_definitions(OM3_share PRIVATE CPRGNU NAMING=_ADD_UNDERSCORE FORTRANUNDERSCORE)
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  target_compile_definitions(OM3_share PRIVATE CPRINTEL NAMING=_ADD_UNDERSCORE FORTRANUNDERSCORE)
endif()

set_target_properties(OM3_share PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod)
target_include_directories(OM3_share PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/mod>
  $<INSTALL_INTERFACE:mod>
  CESM_share/include
)
target_link_libraries(OM3_share PUBLIC esmf PIO::PIO_Fortran)
