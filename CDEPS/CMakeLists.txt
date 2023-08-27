add_subdirectory(fox)

list(APPEND cdeps_common_src_files
  CDEPS/streams/dshr_methods_mod.F90
  CDEPS/streams/dshr_strdata_mod.F90
  CDEPS/streams/dshr_stream_mod.F90
  CDEPS/streams/dshr_tinterp_mod.F90

  CDEPS/dshr/dshr_dfield_mod.F90
  CDEPS/dshr/dshr_fldlist_mod.F90
  CDEPS/dshr/dshr_mod.F90
)

list(APPEND cdeps_drof_src_files
  CDEPS/drof/rof_comp_nuopc.F90
)
  
list(APPEND cdeps_datm_src_files
  CDEPS/datm/atm_comp_nuopc.F90
  CDEPS/datm/datm_datamode_cfsr_mod.F90
  CDEPS/datm/datm_datamode_clmncep_mod.F90
  CDEPS/datm/datm_datamode_core2_mod.F90
  CDEPS/datm/datm_datamode_cplhist_mod.F90
  CDEPS/datm/datm_datamode_era5_mod.F90
  CDEPS/datm/datm_datamode_gefs_mod.F90
  CDEPS/datm/datm_datamode_jra_mod.F90
)


list(APPEND cdeps_docn_src_files
  CDEPS/docn/docn_datamode_aquaplanet_mod.F90
  CDEPS/docn/docn_datamode_copyall_mod.F90
  CDEPS/docn/docn_datamode_cplhist_mod.F90
  CDEPS/docn/docn_datamode_iaf_mod.F90
  CDEPS/docn/docn_datamode_som_mod.F90
  CDEPS/docn/docn_import_data_mod.F90
  CDEPS/docn/ocn_comp_nuopc.F90
)

list(APPEND cdeps_dice_src_files
  CDEPS/dice/dice_datamode_ssmi_mod.F90
  CDEPS/dice/dice_flux_atmice_mod.F90
  CDEPS/dice/ice_comp_nuopc.F90
)

list(APPEND cdeps_dwav_src_files
  CDEPS/dwav/wav_comp_nuopc.F90
)

add_fortran_library(cdeps_common mod STATIC ${cdeps_common_src_files})
target_include_directories(cdeps_common PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/fox/include>)
target_link_libraries(cdeps_common PUBLIC share cmeps esmf PIO::PIO_Fortran FoX_dom)

foreach(LIB cdeps_docn cdeps_dice cdeps_dwav cdeps_drof cdeps_datm)
  add_fortran_library(${LIB} ${LIB}/mod STATIC ${${LIB}_src_files})
  target_include_directories(${LIB} PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/fox/include>)
  target_link_libraries(${LIB} PUBLIC share cmeps esmf PIO::PIO_Fortran FoX_dom cdeps_common)
endforeach()
