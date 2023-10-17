# Common sources
list(APPEND ww3_src_files
  WW3/model/src/constants.F90
  WW3/model/src/w3adatmd.F90
  WW3/model/src/w3arrymd.F90
  WW3/model/src/w3bullmd.F90
  WW3/model/src/w3cspcmd.F90
  WW3/model/src/w3dispmd.F90
  WW3/model/src/w3fldsmd.F90
  WW3/model/src/w3gdatmd.F90
  WW3/model/src/w3gridmd.F90
  WW3/model/src/w3gsrumd.F90
  WW3/model/src/w3idatmd.F90
  WW3/model/src/w3initmd.F90
  WW3/model/src/w3iobcmd.F90
  WW3/model/src/w3iogomd.F90
  WW3/model/src/w3iogrmd.F90
  WW3/model/src/w3iopomd.F90
  WW3/model/src/w3iorsmd.F90
  WW3/model/src/w3iosfmd.F90
  WW3/model/src/w3iotrmd.F90
  WW3/model/src/w3macros.h
  WW3/model/src/w3metamd.F90
  WW3/model/src/w3nmlbouncmd.F90
  WW3/model/src/w3nmlboundmd.F90
  WW3/model/src/w3nmlgridmd.F90
  WW3/model/src/w3nmlmultimd.F90
  WW3/model/src/w3nmlounfmd.F90
  WW3/model/src/w3nmlounpmd.F90
  WW3/model/src/w3nmlprncmd.F90
  WW3/model/src/w3nmlshelmd.F90
  WW3/model/src/w3nmltrncmd.F90
  WW3/model/src/w3nmluprstrmd.F90
  WW3/model/src/w3odatmd.F90
  WW3/model/src/w3parall.F90
  WW3/model/src/w3partmd.F90
  WW3/model/src/w3servmd.F90
  WW3/model/src/w3srcemd.F90
  WW3/model/src/w3strkmd.F90
  WW3/model/src/w3timemd.F90
  WW3/model/src/w3triamd.F90
  WW3/model/src/w3updtmd.F90
  WW3/model/src/w3wavemd.F90
  WW3/model/src/w3wdasmd.F90
  WW3/model/src/w3wdatmd.F90
  WW3/model/src/wmfinlmd.F90
  WW3/model/src/wmgridmd.F90
  WW3/model/src/wminiomd.F90
  WW3/model/src/wminitmd.F90
  WW3/model/src/wmiopomd.F90
  WW3/model/src/wmmdatmd.F90
  WW3/model/src/wmunitmd.F90
  WW3/model/src/wmupdtmd.F90
  WW3/model/src/wmwavemd.F90
  WW3/model/src/w3tidemd.F90
  WW3/model/src/wav_grdout.F90
  WW3/model/src/w3iogoncdmd.F90
  WW3/model/src/wav_shr_flags.F90

  # NUOPC cap sources
  WW3/model/src/wav_kind_mod.F90
  WW3/model/src/wav_shr_mod.F90
  WW3/model/src/wav_shel_inp.F90
  WW3/model/src/wav_comp_nuopc.F90
  WW3/model/src/wav_import_export.F90
)

# Utilities sources
list(APPEND ww3_grid_src_files
  WW3/model/src/ww3_grid.F90
)

list(APPEND ww3_strt_src_files
  WW3/model/src/ww3_strt.F90
)

list(APPEND ww3_outf_src_files
  WW3/model/src/ww3_outf.F90
)

list(APPEND ww3_ounf_src_files
  WW3/model/src/ww3_ounf.F90
  WW3/model/src/w3ounfmetamd.F90
)

list(APPEND ww3_outp_src_files
  WW3/model/src/ww3_outp.F90
)
