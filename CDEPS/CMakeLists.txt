# CDEPS compiler flags
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

add_subdirectory(fox)

list(APPEND cdeps_common_src_files
  CDEPS/streams/dshr_methods_mod.F90
  CDEPS/streams/dshr_strdata_mod.F90
  CDEPS/streams/dshr_stream_mod.F90
  CDEPS/streams/dshr_tinterp_mod.F90

  CDEPS/dshr/dshr_dfield_mod.F90
  CDEPS/dshr/dshr_fldlist_mod.F90
  CDEPS/dshr/dshr_mod.F90

  CDEPS/datm/atm_comp_nuopc.F90
  CDEPS/datm/datm_datamode_cfsr_mod.F90
  CDEPS/datm/datm_datamode_clmncep_mod.F90
  CDEPS/datm/datm_datamode_core2_mod.F90
  CDEPS/datm/datm_datamode_cplhist_mod.F90
  CDEPS/datm/datm_datamode_era5_mod.F90
  CDEPS/datm/datm_datamode_gefs_mod.F90
  #CDEPS/datm/datm_datamode_jra_mod.F90
  extra_sources/datm_datamode_jra_mod.F90
  CDEPS/dlnd/lnd_comp_nuopc.F90

  CDEPS/drof/rof_comp_nuopc.F90
)

list(APPEND cdeps_docn_src_files
  CDEPS/docn/docn_datamode_aquaplanet_mod.F90
  CDEPS/docn/docn_datamode_copyall_mod.F90
  CDEPS/docn/docn_datamode_iaf_mod.F90
  CDEPS/docn/docn_datamode_som_mod.F90
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
target_compile_options(cdeps_common PRIVATE "$<$<COMPILE_LANGUAGE:Fortran>:${fortran_compile_flags}>")
target_compile_options(cdeps_common PRIVATE "$<$<AND:$<CONFIG:Debug>,$<COMPILE_LANGUAGE:Fortran>>:${fortran_compile_flags_debug}>")
target_compile_options(cdeps_common PRIVATE "$<$<AND:$<CONFIG:Release>,$<COMPILE_LANGUAGE:Fortran>>:${fortran_compile_flags_release}>")
target_link_libraries(cdeps_common PUBLIC share cmeps esmf PIO::PIO_Fortran FoX_dom)

foreach(LIB cdeps_docn cdeps_dice cdeps_dwav)
  add_fortran_library(${LIB} ${LIB}/mod STATIC ${${LIB}_src_files})
  target_include_directories(${LIB} PUBLIC $<BUILD_INTERFACE:${CMAKE_CURRENT_BINARY_DIR}/fox/include>)
  target_compile_options(${LIB} PRIVATE "$<$<COMPILE_LANGUAGE:Fortran>:${fortran_compile_flags}>")
  target_compile_options(${LIB} PRIVATE "$<$<AND:$<CONFIG:Debug>,$<COMPILE_LANGUAGE:Fortran>>:${fortran_compile_flags_debug}>")
  target_compile_options(${LIB} PRIVATE "$<$<AND:$<CONFIG:Release>,$<COMPILE_LANGUAGE:Fortran>>:${fortran_compile_flags_release}>")
  target_link_libraries(${LIB} PUBLIC share cmeps esmf PIO::PIO_Fortran FoX_dom cdeps_common)
endforeach()
