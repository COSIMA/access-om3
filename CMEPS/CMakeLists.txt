### Targets

## CMEPS library
add_fortran_library(OM3_cmeps mod/cmeps STATIC)
add_library(AccessOM3::cmeps ALIAS OM3_cmeps)
target_link_libraries(OM3_cmeps
  PUBLIC esmf
  PRIVATE AccessOM3::nuopc_cap_share AccessOM3::share AccessOM3::timing
)
if(OpenMP_Fortran_FOUND)
  target_link_libraries(OM3_cmeps PRIVATE OpenMP::OpenMP_Fortran)
endif()
target_sources(OM3_cmeps PRIVATE
  CMEPS/cesm/flux_atmocn/shr_flux_mod.F90
  CMEPS/mediator/med_phases_history_mod.F90
  CMEPS/mediator/med_phases_restart_mod.F90
  CMEPS/mediator/med_map_mod.F90
  CMEPS/mediator/med_methods_mod.F90
  CMEPS/mediator/med_phases_prep_ice_mod.F90
  CMEPS/mediator/med_phases_prep_glc_mod.F90
  CMEPS/mediator/med_internalstate_mod.F90
  CMEPS/mediator/med_phases_profile_mod.F90
  CMEPS/mediator/esmFldsExchange_hafs_mod.F90
  CMEPS/mediator/med_phases_prep_rof_mod.F90
  CMEPS/mediator/esmFldsExchange_cesm_mod.F90
  CMEPS/mediator/med_merge_mod.F90
  CMEPS/mediator/med_constants_mod.F90
  CMEPS/mediator/med_kind_mod.F90
  CMEPS/mediator/esmFldsExchange_access_mod.F90
  CMEPS/mediator/esmFldsExchange_ufs_mod.F90
  CMEPS/mediator/med_phases_prep_lnd_mod.F90
  CMEPS/mediator/med_phases_prep_atm_mod.F90
  CMEPS/mediator/med_phases_prep_ocn_mod.F90
  CMEPS/mediator/esmFlds.F90
  CMEPS/mediator/med.F90
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
  CMEPS/mediator/med_phases_post_wav_mod.F90
)

## NUOPC cap share
add_fortran_library(OM3_nuopc_cap_share mod/nuopc_cap_share STATIC)
add_library(AccessOM3::nuopc_cap_share ALIAS OM3_nuopc_cap_share)
target_link_libraries(OM3_nuopc_cap_share
  PUBLIC esmf
  PRIVATE AccessOM3::share
)
target_sources(OM3_nuopc_cap_share PRIVATE
  CMEPS/cesm/nuopc_cap_share/driver_pio_mod.F90
  CMEPS/cesm/nuopc_cap_share/esm_utils_mod.F90
  CMEPS/cesm/nuopc_cap_share/glc_elevclass_mod.F90
)

<<<<<<< HEAD

### Install and Export

if(OM3_LIB_INSTALL)
  ## CMEPS library
  set_target_properties(OM3_cmeps PROPERTIES
    OUTPUT_NAME access-cmeps
    EXPORT_NAME cmeps
  )
  install(TARGETS OM3_cmeps
    EXPORT AccessOM3cmeps_Targets
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT AccessOM3_RunTime NAMELINK_COMPONENT AccessOM3_Development
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT AccessOM3_Development
  )
  # Fortran module files are a special case, as currently there is no standard
  # way of handling them in CMake
  target_include_directories(OM3_cmeps PUBLIC "$<INSTALL_INTERFACE:${CMAKE_INSTALL_MODULEDIR}/access-cmeps>")
  get_target_property(cmeps_moddir OM3_cmeps Fortran_MODULE_DIRECTORY)
  install(FILES ${cmeps_moddir}/med.mod ${cmeps_moddir}/med_time_mod.mod ${cmeps_moddir}/med_internalstate_mod.mod
    DESTINATION ${CMAKE_INSTALL_MODULEDIR}/access-cmeps
    COMPONENT AccessOM3_Development
  )
  install(EXPORT AccessOM3cmeps_Targets
    FILE AccessOM3cmepsTargets.cmake
    NAMESPACE AccessOM3::
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/AccessOM3
  )

  ## NUOPC cap share
  set_target_properties(OM3_nuopc_cap_share PROPERTIES
    OUTPUT_NAME access-nuopc_cap_share
    EXPORT_NAME nuopc_cap_share
  )
  install(TARGETS OM3_nuopc_cap_share
    EXPORT AccessOM3nuopc_cap_share_Targets
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT AccessOM3_RunTime NAMELINK_COMPONENT AccessOM3_Development
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT AccessOM3_Development
  )
  # Fortran module files are a special case, as currently there is no standard
  # way of handling them in CMake
  target_include_directories(OM3_nuopc_cap_share PUBLIC "$<INSTALL_INTERFACE:${CMAKE_INSTALL_MODULEDIR}/access-nuopc_cap_share>")
  get_target_property(nuopc_cap_share_moddir OM3_nuopc_cap_share Fortran_MODULE_DIRECTORY)
  install(DIRECTORY ${nuopc_cap_share_moddir}/
    DESTINATION ${CMAKE_INSTALL_MODULEDIR}/access-nuopc_cap_share
    COMPONENT AccessOM3_Development
  )
  install(EXPORT AccessOM3nuopc_cap_share_Targets
    FILE AccessOM3nuopc_cap_shareTargets.cmake
    NAMESPACE AccessOM3::
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/AccessOM3
  )
=======
### Install and Export

## Library
if(OM3_LIB_INSTALL)
  set_target_properties(OM3_cmeps PROPERTIES
    OUTPUT_NAME access-cmeps
  )
  install(TARGETS OM3_cmeps
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
  )
  # Fortran module files are a special case, as currently there is no standard
  # way of handling them in CMake
  get_target_property(cmeps_moddir OM3_cmeps Fortran_MODULE_DIRECTORY)
  foreach(MOD med med_time_mod med_internalstate_mod)
    install(FILES ${cmeps_moddir}/${MOD}.mod ${} DESTINATION ${CMAKE_INSTALL_MODULEDIR}/access-cmeps)
  endforeach()
>>>>>>> a548e6c (Add the possibility to install all the components as libraries.)
endif()
