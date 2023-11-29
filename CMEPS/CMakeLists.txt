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
  CMEPS/cesm/nuopc_cap_share/nuopc_shr_methods.F90
)
