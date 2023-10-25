### Subdirectories

## Build FoX
add_subdirectory(fox)

### Targets

## Common library
add_fortran_library(OM3_cdeps_common mod/common STATIC)
target_include_directories(OM3_cdeps_common PRIVATE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/fox/include>)
target_link_libraries(OM3_cdeps_common
  PUBLIC esmf
  PRIVATE OM3_nuopc_cap_share OM3_share FoX_dom
)
target_sources(OM3_cdeps_common PRIVATE
  CDEPS/streams/dshr_methods_mod.F90
  CDEPS/streams/dshr_strdata_mod.F90
  CDEPS/streams/dshr_stream_mod.F90
  CDEPS/streams/dshr_tinterp_mod.F90

  CDEPS/dshr/dshr_dfield_mod.F90
  CDEPS/dshr/dshr_fldlist_mod.F90
  CDEPS/dshr/dshr_mod.F90
)

## DATM
add_fortran_library(OM3_cdeps_datm mod/datm STATIC)
target_include_directories(OM3_cdeps_datm PRIVATE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/fox/include>)
target_link_libraries(OM3_cdeps_datm
  PUBLIC esmf
  PRIVATE OM3_cdeps_common OM3_share
)
target_sources(OM3_cdeps_datm PRIVATE
  CDEPS/datm/datm_datamode_cfsr_mod.F90
  CDEPS/datm/datm_datamode_clmncep_mod.F90
  CDEPS/datm/datm_datamode_core2_mod.F90
  CDEPS/datm/datm_datamode_cplhist_mod.F90
  CDEPS/datm/datm_datamode_era5_mod.F90
  CDEPS/datm/datm_datamode_gefs_mod.F90
  CDEPS/datm/datm_datamode_jra_mod.F90
  extra_sources/datm_datamode_jra55do_mod.F90
)
add_patched_source(OM3_cdeps_datm CDEPS/datm/atm_comp_nuopc.F90)

## DOCN
add_fortran_library(OM3_cdeps_docn mod/docn STATIC)
target_include_directories(OM3_cdeps_docn PRIVATE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/fox/include>)
target_link_libraries(OM3_cdeps_docn
  PUBLIC esmf
  PRIVATE OM3_cdeps_common OM3_share
)
target_sources(OM3_cdeps_docn PRIVATE
  CDEPS/docn/docn_datamode_aquaplanet_mod.F90
  CDEPS/docn/docn_datamode_copyall_mod.F90
  CDEPS/docn/docn_datamode_cplhist_mod.F90
  CDEPS/docn/docn_datamode_iaf_mod.F90
  CDEPS/docn/docn_datamode_som_mod.F90
  CDEPS/docn/docn_import_data_mod.F90
  CDEPS/docn/ocn_comp_nuopc.F90
)

## DICE
add_fortran_library(OM3_cdeps_dice mod/dice STATIC)
target_include_directories(OM3_cdeps_dice PRIVATE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/fox/include>)
target_link_libraries(OM3_cdeps_dice
  PUBLIC esmf
  PRIVATE OM3_cdeps_common OM3_share
)
target_sources(OM3_cdeps_dice PRIVATE
  CDEPS/dice/dice_datamode_ssmi_mod.F90
  CDEPS/dice/dice_flux_atmice_mod.F90
  CDEPS/dice/ice_comp_nuopc.F90
)

## DWAV
add_fortran_library(OM3_cdeps_dwav mod/dwav STATIC)
target_include_directories(OM3_cdeps_dwav PRIVATE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/fox/include>)
target_link_libraries(OM3_cdeps_dwav
  PUBLIC esmf
  PRIVATE OM3_cdeps_common OM3_share
)
target_sources(OM3_cdeps_dwav PRIVATE
  CDEPS/dwav/wav_comp_nuopc.F90
)

## DROF
add_fortran_library(OM3_cdeps_drof mod/drof STATIC)
target_include_directories(OM3_cdeps_drof PRIVATE $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/fox/include>)
target_link_libraries(OM3_cdeps_drof
  PUBLIC esmf
  PRIVATE OM3_cdeps_common OM3_share
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
  )
  install(TARGETS OM3_cdeps_common
    LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
    ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
    PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
  )
  # components
  foreach(LIB atm ocn ice wav rof)
    set_target_properties(OM3_cdeps_d${LIB} PROPERTIES
      OUTPUT_NAME access-cdeps-d${LIB}
    )
    install(TARGETS OM3_cdeps_d${LIB}
      LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
      ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
      PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
    )
    # Fortran module files are a special case, as currently there is no standard
    # way of handling them in CMake
    get_target_property(d${LIB}_moddir OM3_cdeps_d${LIB} Fortran_MODULE_DIRECTORY)
    install(FILES ${d${LIB}_moddir}/${LIB}_comp_nuopc.mod DESTINATION ${CMAKE_INSTALL_MODULEDIR}/access-cdeps-d${LIB})
  endforeach()
endif()
