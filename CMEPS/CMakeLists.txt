###############################################################################
### CMEPS compiler flags
###############################################################################
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  set(fortran_compile_flags -ffree-line-length-none)
  if(${CMAKE_Fortran_COMPILER_VERSION} VERSION_GREATER_EQUAL 10)
    list(APPEND fortran_compile_flags -fallow-argument-mismatch -fallow-invalid-boz)
  endif()
  set(fortran_compile_flags_debug -fcheck=bounds -ffpe-trap=invalid,zero,overflow,underflow)
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  set(fortran_compile_flags -O -assume realloc_lhs)
  set(fortran_compile_flags_release -fp-model precise)
  set(fortran_compile_flags_debug -check "SHELL:-check noarg_temp_created" "SHELL:-check nopointer" -fpe0 -ftrapuv -init=snan,arrays)
endif()

###############################################################################
### Source files
###############################################################################

list(APPEND _nems_util_files
  CMEPS/ufs/flux_atmocn_mod.F90
  CMEPS/ufs/glc_elevclass_mod.F90
  CMEPS/ufs/ufs_kind_mod.F90
  CMEPS/ufs/perf_mod.F90
  CMEPS/ufs/ufs_const_mod.F90)

list(APPEND _mediator_files
  CMEPS/mediator/med_phases_restart_mod.F90
  CMEPS/mediator/med_map_mod.F90
  CMEPS/mediator/med_methods_mod.F90
  CMEPS/mediator/med_phases_prep_ice_mod.F90
  CMEPS/mediator/med_phases_history_mod.F90
  CMEPS/mediator/med_phases_prep_glc_mod.F90
  CMEPS/mediator/med_internalstate_mod.F90
  CMEPS/mediator/med_phases_profile_mod.F90
  CMEPS/mediator/esmFldsExchange_hafs_mod.F90
  CMEPS/mediator/med_phases_prep_rof_mod.F90
  CMEPS/mediator/esmFldsExchange_cesm_mod.F90
  CMEPS/mediator/med_merge_mod.F90
  CMEPS/mediator/med_constants_mod.F90
  CMEPS/mediator/med_kind_mod.F90
  CMEPS/mediator/esmFldsExchange_nems_mod.F90
  CMEPS/mediator/med_phases_prep_lnd_mod.F90
  CMEPS/mediator/med_phases_prep_atm_mod.F90
  CMEPS/mediator/med_phases_prep_ocn_mod.F90
  CMEPS/mediator/esmFlds.F90
  CMEPS/mediator/med.F90
  CMEPS/mediator/med_time_mod.F90
  CMEPS/mediator/med_phases_ocnalb_mod.F90
  CMEPS/mediator/med_phases_prep_wav_mod.F90
  CMEPS/mediator/med_utils_mod.F90
  CMEPS/mediator/med_fraction_mod.F90
  CMEPS/mediator/med_phases_aofluxes_mod.F90
  CMEPS/mediator/med_io_mod.F90
  CMEPS/mediator/med_diag_mod.F90
  CMEPS/mediator/med_phases_post_atm_mod.F90
  CMEPS/mediator/med_phases_post_ice_mod.F90
  CMEPS/mediator/med_phases_post_ocn_mod.F90
  CMEPS/mediator/med_phases_post_lnd_mod.F90
  CMEPS/mediator/med_phases_post_glc_mod.F90
  CMEPS/mediator/med_phases_post_rof_mod.F90
  CMEPS/mediator/med_phases_post_wav_mod.F90)

###############################################################################
### Libraries
###############################################################################

### Create target library
add_library(cmeps STATIC ${_nems_util_files} ${_mediator_files})

### Set PUBLIC interfaces on the library
set_target_properties(cmeps PROPERTIES Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/mod)
target_include_directories(cmeps PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/mod>
                                        $<INSTALL_INTERFACE:mod>)
target_compile_options(cmeps PUBLIC "$<$<COMPILE_LANGUAGE:Fortran>:${fortran_compile_flags}>")
target_compile_options(cmeps PUBLIC "$<$<AND:$<CONFIG:Debug>,$<COMPILE_LANGUAGE:Fortran>>:${fortran_compile_flags_debug}>")
target_compile_options(cmeps PUBLIC "$<$<AND:$<CONFIG:Release>,$<COMPILE_LANGUAGE:Fortran>>:${fortran_compile_flags_release}>")
target_link_libraries(cmeps PUBLIC FMS::fms_r8
                                   esmf
                                   PIO::PIO_Fortran)
if(OpenMP_Fortran_FOUND)
  target_link_libraries(cmeps PRIVATE OpenMP::OpenMP_Fortran)
endif()
