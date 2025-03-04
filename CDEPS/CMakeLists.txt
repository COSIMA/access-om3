### Targets

## Common library
add_fortran_library(OM3_cdeps_common mod/common STATIC)
add_library(AccessOM3::cdeps_common ALIAS OM3_cdeps_common)
target_include_directories(OM3_cdeps_common PRIVATE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/fox/include>)
target_link_libraries(OM3_cdeps_common
  PUBLIC esmf
  PRIVATE AccessOM3::share AccessOM3::cmeps AccessOM3::nuopc_cap_share FoX::FoX PIO::PIO_Fortran
)
target_sources(OM3_cdeps_common PRIVATE
  CDEPS/streams/dshr_methods_mod.F90
  CDEPS/streams/dshr_strdata_mod.F90
  CDEPS/streams/dshr_stream_mod.F90
  CDEPS/streams/dshr_tinterp_mod.F90
  CDEPS/dshr/dshr_mod.F90
  CDEPS/dshr/dshr_dfield_mod.F90
  CDEPS/dshr/dshr_fldlist_mod.F90
)

## DATM
add_fortran_library(OM3_cdeps_datm mod/datm STATIC)
add_library(AccessOM3::cdeps_datm ALIAS OM3_cdeps_datm)
target_include_directories(OM3_cdeps_datm PRIVATE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/fox/include>)
target_link_libraries(OM3_cdeps_datm
  PUBLIC esmf
  PRIVATE AccessOM3::cdeps_common AccessOM3::share
)
target_sources(OM3_cdeps_datm PRIVATE
  CDEPS/datm/datm_datamode_cfsr_mod.F90
  CDEPS/datm/datm_datamode_clmncep_mod.F90
  CDEPS/datm/datm_datamode_core2_mod.F90
  CDEPS/datm/datm_datamode_cplhist_mod.F90
  CDEPS/datm/datm_datamode_era5_mod.F90
  CDEPS/datm/datm_datamode_gefs_mod.F90
  CDEPS/datm/datm_datamode_jra_mod.F90
  CDEPS/datm/datm_datamode_simple_mod.F90
  extra_sources/datm_datamode_jra55do_mod.F90
)
add_patched_source(OM3_cdeps_datm CDEPS/datm/atm_comp_nuopc.F90)

## DOCN
add_fortran_library(OM3_cdeps_docn mod/docn STATIC)
add_library(AccessOM3::cdeps_docn ALIAS OM3_cdeps_docn)
target_include_directories(OM3_cdeps_docn PRIVATE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/fox/include>)
target_link_libraries(OM3_cdeps_docn
  PUBLIC esmf
  PRIVATE AccessOM3::cdeps_common AccessOM3::share
)
target_sources(OM3_cdeps_docn PRIVATE
  CDEPS/docn/docn_datamode_aquaplanet_mod.F90
  CDEPS/docn/docn_datamode_copyall_mod.F90
  CDEPS/docn/docn_datamode_cplhist_mod.F90
  CDEPS/docn/docn_datamode_iaf_mod.F90
  CDEPS/docn/docn_datamode_multilev_dom_mod.F90
  CDEPS/docn/docn_datamode_multilev_mod.F90
  CDEPS/docn/docn_datamode_som_mod.F90
  CDEPS/docn/docn_import_data_mod.F90
  CDEPS/docn/ocn_comp_nuopc.F90
)

## DICE
add_fortran_library(OM3_cdeps_dice mod/dice STATIC)
add_library(AccessOM3::cdeps_dice ALIAS OM3_cdeps_dice)
target_include_directories(OM3_cdeps_dice PRIVATE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/fox/include>)
target_link_libraries(OM3_cdeps_dice
  PUBLIC esmf
  PRIVATE AccessOM3::cdeps_common AccessOM3::share
)
target_sources(OM3_cdeps_dice PRIVATE
  CDEPS/dice/dice_datamode_ssmi_mod.F90
  CDEPS/dice/dice_flux_atmice_mod.F90
  CDEPS/dice/ice_comp_nuopc.F90
)

## DWAV
add_fortran_library(OM3_cdeps_dwav mod/dwav STATIC)
add_library(AccessOM3::cdeps_dwav ALIAS OM3_cdeps_dwav)
target_include_directories(OM3_cdeps_dwav PRIVATE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/fox/include>)
target_link_libraries(OM3_cdeps_dwav
  PUBLIC esmf
  PRIVATE AccessOM3::cdeps_common AccessOM3::share
)
target_sources(OM3_cdeps_dwav PRIVATE
  CDEPS/dwav/wav_comp_nuopc.F90
)

## DROF
add_fortran_library(OM3_cdeps_drof mod/drof STATIC)
add_library(AccessOM3::cdeps_drof ALIAS OM3_cdeps_drof)
target_include_directories(OM3_cdeps_drof PRIVATE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/fox/include>)
target_link_libraries(OM3_cdeps_drof
  PUBLIC esmf
  PRIVATE AccessOM3::cdeps_common AccessOM3::share
)
target_sources(OM3_cdeps_drof PRIVATE
  CDEPS/drof/rof_comp_nuopc.F90
)

### Install and Export

## Library
if(OM3_LIB_INSTALL)
  # cdeps_common
  set_target_properties(OM3_cdeps_common PROPERTIES
    OUTPUT_NAME access-cdeps-common
    EXPORT_NAME cdeps-common
  )
  install(TARGETS OM3_cdeps_common
    EXPORT AccessOM3cdeps_Targets
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT AccessOM3_RunTime NAMELINK_COMPONENT AccessOM3_Development
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT AccessOM3_Development
  )
  # components
  foreach(LIB atm ocn ice wav rof)
    set_target_properties(OM3_cdeps_d${LIB} PROPERTIES
      OUTPUT_NAME access-cdeps-d${LIB}
      EXPORT_NAME cdeps-d${LIB}
    )
    install(TARGETS OM3_cdeps_d${LIB}
      EXPORT AccessOM3cdeps_Targets
      LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT AccessOM3_RunTime NAMELINK_COMPONENT AccessOM3_Development
      ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR} COMPONENT AccessOM3_Development
    )
    # Fortran module files are a special case, as currently there is no standard
    # way of handling them in CMake
    target_include_directories(OM3_cdeps_d${LIB} PUBLIC "$<INSTALL_INTERFACE:${CMAKE_INSTALL_MODULEDIR}/access-cdeps-d${LIB}>")
    get_target_property(d${LIB}_moddir OM3_cdeps_d${LIB} Fortran_MODULE_DIRECTORY)
    install(FILES ${d${LIB}_moddir}/${LIB}_comp_nuopc.mod
      DESTINATION ${CMAKE_INSTALL_MODULEDIR}/access-cdeps-d${LIB}
      COMPONENT AccessOM3_Development
    )
  endforeach()
  install(EXPORT AccessOM3cdeps_Targets
    FILE AccessOM3cdepsTargets.cmake
    NAMESPACE AccessOM3::
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/AccessOM3
  )
endif()
