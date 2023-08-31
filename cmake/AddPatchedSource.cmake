# Apply a patch to a source file and add the resulting file to the sources of a
# given target.
function(add_patched_source TARGET FILE)
  # This function makes the following assumptions:
  #  - patches are stored in ${CMAKE_CURRENT_SOURCE_DIR}/patches
  #  - all patches to a given file are kept in the same patch file
  #  - patch files are named ${FILE}.patch  

  get_filename_component(filename ${FILE} NAME)

  add_custom_command(
    OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${filename}
    COMMAND ${CMAKE_COMMAND}
    -Din_file:FILEPATH=${CMAKE_CURRENT_SOURCE_DIR}/${FILE}
    -Dpatch_file:FILEPATH=${CMAKE_CURRENT_SOURCE_DIR}/patches/${filename}.patch
    -Dout_file:FILEPATH=${CMAKE_CURRENT_BINARY_DIR}/${filename}
    -P ${CMAKE_SOURCE_DIR}/cmake/PatchFile.cmake
    DEPENDS ${CMAKE_CURRENT_SOURCE_DIR}/${FILE}
  )

  target_sources(${TARGET} PRIVATE ${CMAKE_CURRENT_BINARY_DIR}/${filename})
endfunction(add_patched_source)
