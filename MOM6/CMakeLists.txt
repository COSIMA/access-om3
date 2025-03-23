### Global definitions, flags, etc

## MOM6 Fortran compiler flags
if(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
  set(fortran_compile_flags -fdefault-real-8 -fdefault-double-8)
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
  set(fortran_compile_flags -r8)
endif()

add_compile_definitions(
  _USE_GENERIC_TRACER
  _USE_MOM6_DIAG
)

### Targets

## MOM6 library
add_fortran_library(OM3_mom6 mod STATIC)
add_library(AccessOM3::mom6 ALIAS OM3_mom6)
target_include_directories(OM3_mom6 PRIVATE $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/MOM6/src/framework>)
if(OM3_MOM_SYMMETRIC)
  target_include_directories(OM3_mom6 PRIVATE $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/MOM6/config_src/memory/dynamic_symmetric>)
else()
  target_include_directories(OM3_mom6 PRIVATE $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/MOM6/config_src/memory/dynamic_nonsymmetric>)
endif()
target_compile_options(OM3_mom6 PRIVATE "$<$<COMPILE_LANGUAGE:Fortran>:${fortran_compile_flags}>")
target_link_libraries(OM3_mom6
  PUBLIC esmf
  PRIVATE AccessOM3::nuopc_cap_share AccessOM3::share FMS::fms_r8
)
target_sources(OM3_mom6 PRIVATE
  MOM6/src/ALE/coord_adapt.F90
  MOM6/src/ALE/coord_hycom.F90
  MOM6/src/ALE/coord_rho.F90
  MOM6/src/ALE/coord_sigma.F90
  MOM6/src/ALE/coord_zlike.F90
  MOM6/src/ALE/MOM_ALE.F90
  MOM6/src/ALE/MOM_hybgen_regrid.F90
  MOM6/src/ALE/MOM_hybgen_remap.F90
  MOM6/src/ALE/MOM_hybgen_unmix.F90
  MOM6/src/ALE/MOM_regridding.F90
  MOM6/src/ALE/MOM_remapping.F90
  MOM6/src/ALE/P1M_functions.F90
  MOM6/src/ALE/P3M_functions.F90
  MOM6/src/ALE/PCM_functions.F90
  MOM6/src/ALE/PLM_functions.F90
  MOM6/src/ALE/polynomial_functions.F90
  MOM6/src/ALE/PPM_functions.F90
  MOM6/src/ALE/PQM_functions.F90
  MOM6/src/ALE/regrid_consts.F90
  MOM6/src/ALE/regrid_edge_values.F90
  MOM6/src/ALE/regrid_interp.F90
  MOM6/src/ALE/regrid_solvers.F90
  MOM6/src/ALE/remapping_attic.F90

  MOM6/src/core/MOM_barotropic.F90
  MOM6/src/core/MOM_boundary_update.F90
  MOM6/src/core/MOM_check_scaling.F90
  MOM6/src/core/MOM_checksum_packages.F90
  MOM6/src/core/MOM_continuity.F90
  MOM6/src/core/MOM_continuity_PPM.F90
  MOM6/src/core/MOM_CoriolisAdv.F90
  MOM6/src/core/MOM_density_integrals.F90
  MOM6/src/core/MOM_dynamics_split_RK2.F90
  MOM6/src/core/MOM_dynamics_split_RK2b.F90
  MOM6/src/core/MOM_dynamics_unsplit.F90
  MOM6/src/core/MOM_dynamics_unsplit_RK2.F90
  MOM6/src/core/MOM.F90
  MOM6/src/core/MOM_grid.F90
  MOM6/src/core/MOM_interface_heights.F90
  MOM6/src/core/MOM_isopycnal_slopes.F90
  MOM6/src/core/MOM_open_boundary.F90
  MOM6/src/core/MOM_porous_barriers.F90
  MOM6/src/core/MOM_PressureForce.F90
  MOM6/src/core/MOM_PressureForce_FV.F90
  MOM6/src/core/MOM_PressureForce_Montgomery.F90
  MOM6/src/core/MOM_stoch_eos.F90
  MOM6/src/core/MOM_transcribe_grid.F90
  MOM6/src/core/MOM_unit_tests.F90
  MOM6/src/core/MOM_variables.F90
  MOM6/src/core/MOM_verticalGrid.F90

  MOM6/src/diagnostics/MOM_debugging.F90
  MOM6/src/diagnostics/MOM_diagnose_MLD.F90
  MOM6/src/diagnostics/MOM_diagnostics.F90
  MOM6/src/diagnostics/MOM_harmonic_analysis.F90
  MOM6/src/diagnostics/MOM_obsolete_diagnostics.F90
  MOM6/src/diagnostics/MOM_obsolete_params.F90
  MOM6/src/diagnostics/MOM_spatial_means.F90
  MOM6/src/diagnostics/MOM_sum_output.F90
  MOM6/src/diagnostics/MOM_wave_speed.F90

  MOM6/src/equation_of_state/MOM_EOS.F90
  MOM6/src/equation_of_state/MOM_EOS_Jackett06.F90
  MOM6/src/equation_of_state/MOM_EOS_linear.F90
  MOM6/src/equation_of_state/MOM_EOS_Roquet_rho.F90
  MOM6/src/equation_of_state/MOM_EOS_Roquet_SpV.F90
  MOM6/src/equation_of_state/MOM_EOS_TEOS10.F90
  MOM6/src/equation_of_state/MOM_EOS_UNESCO.F90
  MOM6/src/equation_of_state/MOM_EOS_Wright.F90
  MOM6/src/equation_of_state/MOM_EOS_Wright_full.F90
  MOM6/src/equation_of_state/MOM_EOS_Wright_red.F90
  MOM6/src/equation_of_state/MOM_EOS_base_type.F90
  MOM6/src/equation_of_state/MOM_temperature_convert.F90
  MOM6/src/equation_of_state/MOM_TFreeze.F90

  MOM6/src/equation_of_state/TEOS10/gsw_chem_potential_water_t_exact.f90
  MOM6/src/equation_of_state/TEOS10/gsw_ct_freezing_exact.f90
  MOM6/src/equation_of_state/TEOS10/gsw_ct_freezing_poly.f90
  MOM6/src/equation_of_state/TEOS10/gsw_ct_from_pt.f90
  MOM6/src/equation_of_state/TEOS10/gsw_ct_from_t.f90
  MOM6/src/equation_of_state/TEOS10/gsw_entropy_part.f90
  MOM6/src/equation_of_state/TEOS10/gsw_entropy_part_zerop.f90
  MOM6/src/equation_of_state/TEOS10/gsw_gibbs.f90
  MOM6/src/equation_of_state/TEOS10/gsw_gibbs_ice.f90
  MOM6/src/equation_of_state/TEOS10/gsw_gibbs_pt0_pt0.f90
  MOM6/src/equation_of_state/TEOS10/gsw_mod_error_functions.f90
  MOM6/src/equation_of_state/TEOS10/gsw_mod_freezing_poly_coefficients.f90
  MOM6/src/equation_of_state/TEOS10/gsw_mod_gibbs_ice_coefficients.f90
  MOM6/src/equation_of_state/TEOS10/gsw_mod_kinds.f90
  MOM6/src/equation_of_state/TEOS10/gsw_mod_specvol_coefficients.f90
  MOM6/src/equation_of_state/TEOS10/gsw_mod_teos10_constants.f90
  MOM6/src/equation_of_state/TEOS10/gsw_mod_toolbox.f90
  MOM6/src/equation_of_state/TEOS10/gsw_pt0_from_t.f90
  MOM6/src/equation_of_state/TEOS10/gsw_pt_from_ct.f90
  MOM6/src/equation_of_state/TEOS10/gsw_pt_from_t.f90
  MOM6/src/equation_of_state/TEOS10/gsw_rho.f90
  MOM6/src/equation_of_state/TEOS10/gsw_rho_first_derivatives.f90
  MOM6/src/equation_of_state/TEOS10/gsw_rho_second_derivatives.f90
  MOM6/src/equation_of_state/TEOS10/gsw_sp_from_sr.f90
  MOM6/src/equation_of_state/TEOS10/gsw_sr_from_sp.f90
  MOM6/src/equation_of_state/TEOS10/gsw_specvol.f90
  MOM6/src/equation_of_state/TEOS10/gsw_specvol_first_derivatives.f90
  MOM6/src/equation_of_state/TEOS10/gsw_specvol_second_derivatives.f90
  MOM6/src/equation_of_state/TEOS10/gsw_t_deriv_chem_potential_water_t_exact.f90
  MOM6/src/equation_of_state/TEOS10/gsw_t_freezing_exact.f90
  MOM6/src/equation_of_state/TEOS10/gsw_t_freezing_poly.f90
  MOM6/src/equation_of_state/TEOS10/gsw_t_from_ct.f90

  MOM6/src/framework/MOM_array_transform.F90
  MOM6/src/framework/MOM_checksums.F90
  MOM6/src/framework/MOM_coms.F90
  MOM6/src/framework/MOM_cpu_clock.F90
  MOM6/src/framework/MOM_data_override.F90
  MOM6/src/framework/MOM_diag_mediator.F90
  MOM6/src/framework/MOM_diag_remap.F90
  MOM6/src/framework/MOM_document.F90
  MOM6/src/framework/MOM_domains.F90
  MOM6/src/framework/MOM_dyn_horgrid.F90
  MOM6/src/framework/MOM_ensemble_manager.F90
  MOM6/src/framework/MOM_error_handler.F90
  MOM6/src/framework/MOM_file_parser.F90
  MOM6/src/framework/MOM_get_input.F90
  MOM6/src/framework/MOM_hor_index.F90
  MOM6/src/framework/MOM_horizontal_regridding.F90
  MOM6/src/framework/MOM_interpolate.F90
  MOM6/src/framework/MOM_intrinsic_functions.F90
  MOM6/src/framework/MOM_io.F90
  MOM6/src/framework/MOM_io_file.F90
  MOM6/src/framework/MOM_memory_macros.h
  MOM6/src/framework/MOM_netcdf.F90
  MOM6/src/framework/MOM_random.F90
  MOM6/src/framework/MOM_restart.F90
  MOM6/src/framework/MOM_safe_alloc.F90
  MOM6/src/framework/MOM_string_functions.F90
  MOM6/src/framework/MOM_unique_scales.F90
  MOM6/src/framework/MOM_unit_scaling.F90
  MOM6/src/framework/MOM_write_cputime.F90
  MOM6/src/framework/posix.F90

  MOM6/src/ice_shelf/MOM_ice_shelf_diag_mediator.F90
  MOM6/src/ice_shelf/MOM_ice_shelf_dynamics.F90
  MOM6/src/ice_shelf/MOM_ice_shelf.F90
  MOM6/src/ice_shelf/MOM_ice_shelf_initialize.F90
  MOM6/src/ice_shelf/MOM_ice_shelf_state.F90
  MOM6/src/ice_shelf/MOM_marine_ice.F90
  MOM6/src/ice_shelf/user_shelf_init.F90

  MOM6/src/initialization/MOM_coord_initialization.F90
  MOM6/src/initialization/MOM_fixed_initialization.F90
  MOM6/src/initialization/MOM_grid_initialize.F90
  MOM6/src/initialization/MOM_shared_initialization.F90
  MOM6/src/initialization/MOM_state_initialization.F90
  MOM6/src/initialization/MOM_tracer_initialization_from_Z.F90

  MOM6/src/ocean_data_assim/MOM_oda_driver.F90
  MOM6/src/ocean_data_assim/MOM_oda_incupd.F90

  MOM6/src/parameterizations/CVmix/cvmix_background.F90
  MOM6/src/parameterizations/CVmix/cvmix_convection.F90
  MOM6/src/parameterizations/CVmix/cvmix_ddiff.F90
  MOM6/src/parameterizations/CVmix/cvmix_kinds_and_types.F90
  MOM6/src/parameterizations/CVmix/cvmix_kpp.F90
  MOM6/src/parameterizations/CVmix/cvmix_math.F90
  MOM6/src/parameterizations/CVmix/cvmix_put_get.F90
  MOM6/src/parameterizations/CVmix/cvmix_shear.F90
  MOM6/src/parameterizations/CVmix/cvmix_tidal.F90
  MOM6/src/parameterizations/CVmix/cvmix_utils.F90

  MOM6/src/parameterizations/lateral/MOM_hor_visc.F90
  MOM6/src/parameterizations/lateral/MOM_interface_filter.F90
  MOM6/src/parameterizations/lateral/MOM_internal_tides.F90
  MOM6/src/parameterizations/lateral/MOM_lateral_mixing_coeffs.F90
  MOM6/src/parameterizations/lateral/MOM_load_love_numbers.F90
  MOM6/src/parameterizations/lateral/MOM_MEKE.F90
  MOM6/src/parameterizations/lateral/MOM_MEKE_types.F90
  MOM6/src/parameterizations/lateral/MOM_mixed_layer_restrat.F90
  MOM6/src/parameterizations/lateral/MOM_self_attr_load.F90
  MOM6/src/parameterizations/lateral/MOM_spherical_harmonics.F90
  MOM6/src/parameterizations/lateral/MOM_streaming_filter.F90
  MOM6/src/parameterizations/lateral/MOM_thickness_diffuse.F90
  MOM6/src/parameterizations/lateral/MOM_tidal_forcing.F90
  MOM6/src/parameterizations/lateral/MOM_Zanna_Bolton.F90

  MOM6/src/parameterizations/stochastic/MOM_stochastics.F90

  MOM6/src/parameterizations/vertical/MOM_ALE_sponge.F90
  MOM6/src/parameterizations/vertical/MOM_bkgnd_mixing.F90
  MOM6/src/parameterizations/vertical/MOM_bulk_mixed_layer.F90
  MOM6/src/parameterizations/vertical/MOM_CVMix_conv.F90
  MOM6/src/parameterizations/vertical/MOM_CVMix_ddiff.F90
  MOM6/src/parameterizations/vertical/MOM_CVMix_KPP.F90
  MOM6/src/parameterizations/vertical/MOM_CVMix_shear.F90
  MOM6/src/parameterizations/vertical/MOM_diabatic_aux.F90
  MOM6/src/parameterizations/vertical/MOM_diabatic_driver.F90
  MOM6/src/parameterizations/vertical/MOM_diapyc_energy_req.F90
  MOM6/src/parameterizations/vertical/MOM_energetic_PBL.F90
  MOM6/src/parameterizations/vertical/MOM_entrain_diffusive.F90
  MOM6/src/parameterizations/vertical/MOM_full_convection.F90
  MOM6/src/parameterizations/vertical/MOM_geothermal.F90
  MOM6/src/parameterizations/vertical/MOM_internal_tide_input.F90
  MOM6/src/parameterizations/vertical/MOM_kappa_shear.F90
  MOM6/src/parameterizations/vertical/MOM_opacity.F90
  MOM6/src/parameterizations/vertical/MOM_regularize_layers.F90
  MOM6/src/parameterizations/vertical/MOM_set_diffusivity.F90
  MOM6/src/parameterizations/vertical/MOM_set_viscosity.F90
  MOM6/src/parameterizations/vertical/MOM_sponge.F90
  MOM6/src/parameterizations/vertical/MOM_tidal_mixing.F90
  MOM6/src/parameterizations/vertical/MOM_vert_friction.F90

  MOM6/src/tracer/advection_test_tracer.F90
  MOM6/src/tracer/boundary_impulse_tracer.F90
  MOM6/src/tracer/DOME_tracer.F90
  MOM6/src/tracer/dyed_obc_tracer.F90
  MOM6/src/tracer/dye_example.F90
  MOM6/src/tracer/ideal_age_example.F90
  MOM6/src/tracer/ISOMIP_tracer.F90
  MOM6/src/tracer/MOM_CFC_cap.F90
  MOM6/src/tracer/MOM_hor_bnd_diffusion.F90
  MOM6/src/tracer/MOM_neutral_diffusion.F90
  MOM6/src/tracer/MOM_OCMIP2_CFC.F90
  MOM6/src/tracer/MOM_offline_aux.F90
  MOM6/src/tracer/MOM_offline_main.F90
  MOM6/src/tracer/MOM_tracer_advect.F90
  MOM6/src/tracer/MOM_tracer_diabatic.F90
  MOM6/src/tracer/MOM_tracer_flow_control.F90
  MOM6/src/tracer/MOM_tracer_hor_diff.F90
  MOM6/src/tracer/MOM_tracer_registry.F90
  MOM6/src/tracer/MOM_tracer_types.F90
  MOM6/src/tracer/MOM_tracer_Z_init.F90
  MOM6/src/tracer/nw2_tracers.F90
  MOM6/src/tracer/oil_tracer.F90
  MOM6/src/tracer/pseudo_salt_tracer.F90
  MOM6/src/tracer/RGC_tracer.F90
  MOM6/src/tracer/tracer_example.F90

  MOM6/src/user/adjustment_initialization.F90
  MOM6/src/user/baroclinic_zone_initialization.F90
  MOM6/src/user/basin_builder.F90
  MOM6/src/user/benchmark_initialization.F90
  MOM6/src/user/BFB_initialization.F90
  MOM6/src/user/BFB_surface_forcing.F90
  MOM6/src/user/circle_obcs_initialization.F90
  MOM6/src/user/dense_water_initialization.F90
  MOM6/src/user/DOME2d_initialization.F90
  MOM6/src/user/DOME_initialization.F90
  MOM6/src/user/dumbbell_initialization.F90
  MOM6/src/user/dumbbell_surface_forcing.F90
  MOM6/src/user/dyed_channel_initialization.F90
  MOM6/src/user/dyed_obcs_initialization.F90
  MOM6/src/user/external_gwave_initialization.F90
  MOM6/src/user/Idealized_Hurricane.F90
  MOM6/src/user/ISOMIP_initialization.F90
  MOM6/src/user/Kelvin_initialization.F90
  MOM6/src/user/lock_exchange_initialization.F90
  MOM6/src/user/MOM_controlled_forcing.F90
  MOM6/src/user/MOM_wave_interface.F90
  MOM6/src/user/Neverworld_initialization.F90
  MOM6/src/user/Phillips_initialization.F90
  MOM6/src/user/RGC_initialization.F90
  MOM6/src/user/Rossby_front_2d_initialization.F90
  MOM6/src/user/SCM_CVMix_tests.F90
  MOM6/src/user/seamount_initialization.F90
  MOM6/src/user/shelfwave_initialization.F90
  MOM6/src/user/sloshing_initialization.F90
  MOM6/src/user/soliton_initialization.F90
  MOM6/src/user/supercritical_initialization.F90
  MOM6/src/user/tidal_bay_initialization.F90
  MOM6/src/user/user_change_diffusivity.F90
  MOM6/src/user/user_initialization.F90
  MOM6/src/user/user_revise_forcing.F90

  MOM6/config_src/external/database_comms/MOM_database_comms.F90
  MOM6/config_src/external/database_comms/database_client_interface.F90

  MOM6/config_src/external/drifters/MOM_particles.F90
  MOM6/config_src/external/drifters/MOM_particles_types.F90

  GFDL_generic_tracers/generic_tracers/FMS_coupler_util.F90
  GFDL_generic_tracers/generic_tracers/generic_tracer.F90
  GFDL_generic_tracers/generic_tracers/generic_tracer_utils.F90
  GFDL_generic_tracers/generic_tracers/FMS_ocmip2_co2calc.F90
  GFDL_generic_tracers/generic_tracers/generic_abiotic.F90
  GFDL_generic_tracers/generic_tracers/generic_age.F90
  GFDL_generic_tracers/generic_tracers/generic_argon.F90
  GFDL_generic_tracers/generic_tracers/generic_BLING.F90
  GFDL_generic_tracers/generic_tracers/generic_blres.F90
  GFDL_generic_tracers/generic_tracers/generic_CFC.F90
  GFDL_generic_tracers/generic_tracers/generic_COBALT.F90
  GFDL_generic_tracers/generic_tracers/generic_ERGOM.F90
  GFDL_generic_tracers/generic_tracers/generic_miniBLING.F90
  GFDL_generic_tracers/generic_tracers/generic_SF6.F90
  GFDL_generic_tracers/generic_tracers/generic_TOPAZ.F90
  GFDL_generic_tracers/generic_tracers/generic_WOMBATlite.F90

  GFDL_generic_tracers/mocsy/src/mocsy_buffesm.F90
  GFDL_generic_tracers/mocsy/src/mocsy_constants.F90
  GFDL_generic_tracers/mocsy/src/mocsy_depth2press.F90
  GFDL_generic_tracers/mocsy/src/mocsy_derivauto.F90
  GFDL_generic_tracers/mocsy/src/mocsy_derivnum.F90
  GFDL_generic_tracers/mocsy/src/mocsy_DNAD.F90
  GFDL_generic_tracers/mocsy/src/mocsy_errors.F90
  GFDL_generic_tracers/mocsy/src/mocsy_f2pCO2.F90
  GFDL_generic_tracers/mocsy/src/mocsy_gasx.F90
  GFDL_generic_tracers/mocsy/src/mocsy_p2fCO2.F90
  GFDL_generic_tracers/mocsy/src/mocsy_p80.F90
  GFDL_generic_tracers/mocsy/src/mocsy_phsolvers.F90
  GFDL_generic_tracers/mocsy/src/mocsy_rho.F90
  GFDL_generic_tracers/mocsy/src/mocsy_rhoinsitu.F90
  GFDL_generic_tracers/mocsy/src/mocsy_singledouble.F90
  GFDL_generic_tracers/mocsy/src/mocsy_sw_adtg.F90
  GFDL_generic_tracers/mocsy/src/mocsy_sw_ptmp.F90
  GFDL_generic_tracers/mocsy/src/mocsy_sw_temp.F90
  GFDL_generic_tracers/mocsy/src/mocsy_tis.F90
  GFDL_generic_tracers/mocsy/src/mocsy_tpot.F90
  GFDL_generic_tracers/mocsy/src/mocsy_vars.F90
  GFDL_generic_tracers/mocsy/src/mocsy_varsolver.F90

  MOM6/config_src/external/ODA_hooks/kdtree.f90
  MOM6/config_src/external/ODA_hooks/ocean_da_core.F90
  MOM6/config_src/external/ODA_hooks/ocean_da_types.F90
  MOM6/config_src/external/ODA_hooks/write_ocean_obs.F90

  MOM6/config_src/external/stochastic_physics/get_stochy_pattern.F90
  MOM6/config_src/external/stochastic_physics/stochastic_physics.F90

  MOM6/config_src/infra/FMS2/MOM_coms_infra.F90
  MOM6/config_src/infra/FMS2/MOM_constants.F90
  MOM6/config_src/infra/FMS2/MOM_cpu_clock_infra.F90
  MOM6/config_src/infra/FMS2/MOM_data_override_infra.F90
  MOM6/config_src/infra/FMS2/MOM_diag_manager_infra.F90
  MOM6/config_src/infra/FMS2/MOM_domain_infra.F90
  MOM6/config_src/infra/FMS2/MOM_ensemble_manager_infra.F90
  MOM6/config_src/infra/FMS2/MOM_error_infra.F90
  MOM6/config_src/infra/FMS2/MOM_interp_infra.F90
  MOM6/config_src/infra/FMS2/MOM_time_manager.F90

  MOM6/config_src/drivers/nuopc_cap/mom_cap_time.F90
  MOM6/config_src/drivers/nuopc_cap/mom_surface_forcing_nuopc.F90
  MOM6/config_src/drivers/nuopc_cap/ocn_comp_NUOPC.F90
  MOM6/config_src/drivers/nuopc_cap/time_utils.F90

#   extra_sources/mom_cap_gtracer_flux.F90
  MOM6/config_src/drivers/nuopc_cap/mom_cap.F90
  MOM6/config_src/drivers/nuopc_cap/mom_cap_methods.F90
  MOM6/config_src/drivers/nuopc_cap/mom_ocean_model_nuopc.F90
  MOM6/config_src/infra/FMS2/MOM_couplertype_infra.F90
  MOM6/src/framework/MOM_coupler_types.F90
  MOM6/src/tracer/MOM_generic_tracer.F90
  MOM6/src/core/MOM_forcing_type.F90
  MOM6/config_src/infra/FMS2/MOM_io_infra.F90
  MOM6/src/diagnostics/MOM_PointAccel.F90
)
# add_patched_source(OM3_mom6 MOM6/config_src/drivers/nuopc_cap/mom_cap.F90)
# add_patched_source(OM3_mom6 MOM6/config_src/drivers/nuopc_cap/mom_cap_methods.F90)
# add_patched_source(OM3_mom6 MOM6/config_src/drivers/nuopc_cap/mom_ocean_model_nuopc.F90)
# add_patched_source(OM3_mom6 MOM6/config_src/infra/FMS2/MOM_couplertype_infra.F90)
# add_patched_source(OM3_mom6 MOM6/src/framework/MOM_coupler_types.F90)
# add_patched_source(OM3_mom6 MOM6/src/tracer/MOM_generic_tracer.F90)
# add_patched_source(OM3_mom6 MOM6/src/core/MOM_forcing_type.F90)
# add_patched_source(OM3_mom6 MOM6/config_src/infra/FMS2/MOM_io_infra.F90)
# add_patched_source(OM3_mom6 MOM6/src/diagnostics/MOM_PointAccel.F90)
### Install and Export

## Library
if(OM3_LIB_INSTALL)
  set_target_properties(OM3_mom6 PROPERTIES
    OUTPUT_NAME access-mom6
    EXPORT_NAME mom6
  )
  install(TARGETS OM3_mom6
    EXPORT AccessOM3mom6_Targets
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT AccessOM3_RunTime NAMELINK_COMPONENT AccessOM3_Development
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT AccessOM3_Development
  )
  # Fortran module files are a special case, as currently there is no standard
  # way of handling them in CMake
  target_include_directories(OM3_mom6 PUBLIC "$<INSTALL_INTERFACE:${CMAKE_INSTALL_MODULEDIR}/access-mom6>")
  get_target_property(mom_moddir OM3_mom6 Fortran_MODULE_DIRECTORY)
  install(FILES ${mom_moddir}/ocn_comp_nuopc.mod ${mom_moddir}/mom_cap_mod.mod
    DESTINATION ${CMAKE_INSTALL_MODULEDIR}/access-mom6
    COMPONENT AccessOM3_Development
  )
  install(EXPORT AccessOM3mom6_Targets
    FILE AccessOM3mom6Targets.cmake
    NAMESPACE AccessOM3::
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/AccessOM3
  )
endif()
