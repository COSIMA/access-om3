### Targets

## CICE library
add_fortran_library(OM3_cice mod STATIC)
add_library(AccessOM3::cice ALIAS OM3_cice)
target_compile_definitions(OM3_cice PRIVATE FORTRANUNDERSCORE ncdf)
if(OM3_CICE_IO MATCHES "^(NetCDF|PIO)$")
  target_compile_definitions(OM3_cice PRIVATE USE_NETCDF)
endif()
target_link_libraries(OM3_cice
  PUBLIC esmf
  PRIVATE AccessOM3::cdeps_common AccessOM3::nuopc_cap_share AccessOM3::share AccessOM3::timing
)
if(OM3_CICE_IO MATCHES "^(NetCDF|PIO)$")
  target_link_libraries(OM3_cice PRIVATE NetCDF::NetCDF_Fortran)
  if(OM3_CICE_IO MATCHES "PIO")
    target_link_libraries(OM3_cice PRIVATE PIO::PIO_Fortran)
  endif()
endif()
if(OpenMP_Fortran_FOUND)
  target_link_libraries(OM3_cice PRIVATE OpenMP::OpenMP_Fortran)
endif()

target_sources(OM3_cice PRIVATE
  # Shared List:
  CICE/cicecore/shared/ice_arrays_column.F90
  CICE/cicecore/shared/ice_calendar.F90
  CICE/cicecore/shared/ice_constants.F90
  CICE/cicecore/shared/ice_distribution.F90
  CICE/cicecore/shared/ice_domain_size.F90
  CICE/cicecore/shared/ice_fileunits.F90
  CICE/cicecore/shared/ice_init_column.F90
  CICE/cicecore/shared/ice_kinds_mod.F90
  CICE/cicecore/shared/ice_restart_column.F90
  CICE/cicecore/shared/ice_restart_shared.F90
  CICE/cicecore/shared/ice_spacecurve.F90

  # Analysis
  CICE/cicecore/cicedynB/analysis/ice_diagnostics.F90
  CICE/cicecore/cicedynB/analysis/ice_diagnostics_bgc.F90
  CICE/cicecore/cicedynB/analysis/ice_history.F90
  CICE/cicecore/cicedynB/analysis/ice_history_bgc.F90
  CICE/cicecore/cicedynB/analysis/ice_history_drag.F90
  CICE/cicecore/cicedynB/analysis/ice_history_fsd.F90
  CICE/cicecore/cicedynB/analysis/ice_history_mechred.F90
  CICE/cicecore/cicedynB/analysis/ice_history_pond.F90
  CICE/cicecore/cicedynB/analysis/ice_history_shared.F90
  CICE/cicecore/cicedynB/analysis/ice_history_snow.F90

  # Dynamics
  CICE/cicecore/cicedynB/dynamics/ice_dyn_eap.F90
  CICE/cicecore/cicedynB/dynamics/ice_dyn_evp.F90
  CICE/cicecore/cicedynB/dynamics/ice_dyn_evp_1d.F90
  CICE/cicecore/cicedynB/dynamics/ice_dyn_shared.F90
  CICE/cicecore/cicedynB/dynamics/ice_dyn_vp.F90
  CICE/cicecore/cicedynB/dynamics/ice_transport_driver.F90
  CICE/cicecore/cicedynB/dynamics/ice_transport_remap.F90

  # General
  CICE/cicecore/cicedynB/general/ice_flux.F90
  CICE/cicecore/cicedynB/general/ice_flux_bgc.F90
  CICE/cicecore/cicedynB/general/ice_forcing.F90
  CICE/cicecore/cicedynB/general/ice_forcing_bgc.F90
  CICE/cicecore/cicedynB/general/ice_init.F90
  CICE/cicecore/cicedynB/general/ice_state.F90
  CICE/cicecore/cicedynB/general/ice_step_mod.F90

  # Infrastructure
  CICE/cicecore/cicedynB/infrastructure/ice_blocks.F90
  CICE/cicecore/cicedynB/infrastructure/ice_domain.F90
  CICE/cicecore/cicedynB/infrastructure/ice_grid.F90
  CICE/cicecore/cicedynB/infrastructure/ice_memusage.F90
  CICE/cicecore/cicedynB/infrastructure/ice_memusage_gptl.c
  CICE/cicecore/cicedynB/infrastructure/ice_read_write.F90
  CICE/cicecore/cicedynB/infrastructure/ice_restart_driver.F90
  CICE/cicecore/cicedynB/infrastructure/ice_restoring.F90

  # Icepack
  CICE/icepack/columnphysics/icepack_aerosol.F90
  CICE/icepack/columnphysics/icepack_age.F90
  CICE/icepack/columnphysics/icepack_algae.F90
  CICE/icepack/columnphysics/icepack_atmo.F90
  CICE/icepack/columnphysics/icepack_brine.F90
  CICE/icepack/columnphysics/icepack_firstyear.F90
  CICE/icepack/columnphysics/icepack_flux.F90
  CICE/icepack/columnphysics/icepack_fsd.F90
  CICE/icepack/columnphysics/icepack_intfc.F90
  CICE/icepack/columnphysics/icepack_isotope.F90
  CICE/icepack/columnphysics/icepack_itd.F90
  CICE/icepack/columnphysics/icepack_kinds.F90
  CICE/icepack/columnphysics/icepack_mechred.F90
  CICE/icepack/columnphysics/icepack_meltpond_lvl.F90
  CICE/icepack/columnphysics/icepack_meltpond_topo.F90
  CICE/icepack/columnphysics/icepack_mushy_physics.F90
  CICE/icepack/columnphysics/icepack_ocean.F90
  CICE/icepack/columnphysics/icepack_orbital.F90
  CICE/icepack/columnphysics/icepack_parameters.F90
  CICE/icepack/columnphysics/icepack_shortwave.F90
  CICE/icepack/columnphysics/icepack_snow.F90
  CICE/icepack/columnphysics/icepack_therm_bl99.F90
  CICE/icepack/columnphysics/icepack_therm_itd.F90
  CICE/icepack/columnphysics/icepack_therm_mushy.F90
  CICE/icepack/columnphysics/icepack_therm_shared.F90
  CICE/icepack/columnphysics/icepack_therm_vertical.F90
  CICE/icepack/columnphysics/icepack_tracers.F90
  CICE/icepack/columnphysics/icepack_warnings.F90
  CICE/icepack/columnphysics/icepack_wavefracspec.F90
  CICE/icepack/columnphysics/icepack_zbgc.F90
  CICE/icepack/columnphysics/icepack_zbgc_shared.F90
  CICE/icepack/columnphysics/icepack_zsalinity.F90

  # Shared C
  CICE/cicecore/cicedynB/infrastructure/ice_shr_reprosum86.c

  # MPI
  CICE/cicecore/cicedynB/infrastructure/comm/mpi/ice_boundary.F90
  CICE/cicecore/cicedynB/infrastructure/comm/mpi/ice_broadcast.F90
  CICE/cicecore/cicedynB/infrastructure/comm/mpi/ice_communicate.F90
  CICE/cicecore/cicedynB/infrastructure/comm/mpi/ice_exit.F90
  CICE/cicecore/cicedynB/infrastructure/comm/mpi/ice_gather_scatter.F90
  CICE/cicecore/cicedynB/infrastructure/comm/mpi/ice_global_reductions.F90
  CICE/cicecore/cicedynB/infrastructure/comm/mpi/ice_reprosum.F90
  CICE/cicecore/cicedynB/infrastructure/comm/mpi/ice_timers.F90

  # NUOPC CMEPS driver
  CICE/cicecore/drivers/nuopc/cmeps/CICE_FinalMod.F90
  CICE/cicecore/drivers/nuopc/cmeps/CICE_InitMod.F90
  CICE/cicecore/drivers/nuopc/cmeps/CICE_RunMod.F90
  CICE/cicecore/drivers/nuopc/cmeps/cice_wrapper_mod.F90
  CICE/cicecore/drivers/nuopc/cmeps/ice_comp_nuopc.F90
  CICE/cicecore/drivers/nuopc/cmeps/ice_import_export.F90
  CICE/cicecore/drivers/nuopc/cmeps/ice_prescribed_mod.F90
  CICE/cicecore/drivers/nuopc/cmeps/ice_scam.F90
  CICE/cicecore/drivers/nuopc/cmeps/ice_shr_methods.F90
)
add_patched_source(OM3_cice CICE/cicecore/drivers/nuopc/cmeps/ice_mesh_mod.F90)
# Select IO source files based on CICE_IO
if(OM3_CICE_IO MATCHES "NetCDF")
  target_sources(OM3_cice PRIVATE
    CICE/cicecore/cicedynB/infrastructure/io/io_netcdf/ice_history_write.F90
    CICE/cicecore/cicedynB/infrastructure/io/io_netcdf/ice_restart.F90
  )
elseif(OM3_CICE_IO MATCHES "PIO")
  target_sources(OM3_cice PRIVATE
    CICE/cicecore/cicedynB/infrastructure/io/io_pio2/ice_history_write.F90
    CICE/cicecore/cicedynB/infrastructure/io/io_pio2/ice_pio.F90
    CICE/cicecore/cicedynB/infrastructure/io/io_pio2/ice_restart.F90
  )
elseif(OM3_CICE_IO MATCHES "Binary")
  target_sources(OM3_cice PRIVATE
    CICE/cicecore/cicedynB/infrastructure/io/io_binary/ice_history_write.F90
    CICE/cicecore/cicedynB/infrastructure/io/io_binary/ice_restart.F90
  )
endif()
