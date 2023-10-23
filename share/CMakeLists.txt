### Targets

## share library
add_fortran_library(OM3_share mod/share STATIC)
target_include_directories(OM3_share PUBLIC CESM_share/include)
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  target_compile_definitions(OM3_share PRIVATE CPRGNU NAMING=_ADD_UNDERSCORE FORTRANUNDERSCORE)
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  target_compile_definitions(OM3_share PRIVATE CPRINTEL NAMING=_ADD_UNDERSCORE FORTRANUNDERSCORE)
endif()
target_link_libraries(OM3_share
  PUBLIC esmf PIO::PIO_Fortran
  PRIVATE OM3_timing
)
target_sources(OM3_share PRIVATE
  # The following files are generated with a script acting on templates
  # We include pre-generated files that are committed to the repository
  src/shr_assert_mod.F90
  src/shr_frz_mod.F90
  src/shr_infnan_mod.F90
  src/dtypes.h

  # Modules shared among several components
  CESM_share/include/shr_assert.h
  CESM_share/src/shr_abort_mod.F90
  CESM_share/src/shr_cal_mod.F90
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

  # The following file is a stub.
  stubs/mct_mod.F90
)
add_patched_source(OM3_share CESM_share/src/shr_const_mod.F90)
if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  # CESM reduces the precision and increases speed for the following file
  set_source_files_properties(CESM_share/src/shr_wv_sat_mod.F90 PROPERTIES COMPILE_FLAGS "-fimf-precision=low -fp-model fast")
endif()

## GPTL timing library
add_fortran_library(OM3_timing mod/timing STATIC)
target_compile_definitions(OM3_timing PRIVATE NUOPC_INTERFACE HAVE_MPI)
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  target_compile_definitions(OM3_timing PRIVATE CPRGNU NAMING=_ADD_UNDERSCORE FORTRANUNDERSCORE)
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  target_compile_definitions(OM3_timing PRIVATE CPRINTEL NAMING=_ADD_UNDERSCORE FORTRANUNDERSCORE)
endif()
target_link_libraries(OM3_timing PRIVATE esmf)
target_sources(OM3_timing PRIVATE
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
