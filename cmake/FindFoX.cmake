# We will use pkgconfig to find the library
find_package(PkgConfig REQUIRED)

## Prepare arguments to pass to pkg_search_module

# QUIET
set(_quiet_arg)
if (FoX_FIND_QUIETLY)
  list(APPEND _quiet_arg QUIET)
endif ()

# REQUIRED
set(_required_arg)
if (FoX_FIND_REQUIRED)
  list(APPEND _required_arg REQUIRED)
endif ()

# Construct the moduleSpec to search for
if (DEFINED FoX_FIND_VERSION_RANGE)
  # Can only parse the minimum requirement
  list(APPEND PKG_MODULE_SPECS "fox>=${FoX_FIND_VERSION_MIN}")
elseif ({FoX_FIND_VERSION_EXACT)
  # Requesting exact version
  list(APPEND PKG_MODULE_SPECS "fox=${FoX_FIND_VERSION}")
elseif (DEFINED FoX_FIND_VERSION)
  # Otherwise treat the request as minimum requirement
  list(APPEND PKG_MODULE_SPECS "fox>=${FoX_FIND_VERSION}")
else ()
  # Fallthrough if no version is required
  list(APPEND PKG_MODULE_SPECS "fox")
endif ()

## Call pkg-config
if (CMAKE_VERSION VERSION_LESS 3.28)
  # https://gitlab.kitware.com/cmake/cmake/-/issues/25228
  set(ENV{PKG_CONFIG_ALLOW_SYSTEM_CFLAGS} 1)
endif ()
if (CMAKE_VERSION VERSION_LESS 3.22)
  # Back-porting
  # https://gitlab.kitware.com/cmake/cmake/-/merge_requests/6345
  set(ENV{PKG_CONFIG_ALLOW_SYSTEM_LIBS} 1)
endif ()
pkg_search_module(FoX
  ${_required_arg} ${_quiet_arg}
  IMPORTED_TARGET
  ${PKG_MODULE_SPECS})

## Create alias if package was found by pkg-config
if (FoX_FOUND)
  add_library(FoX::FoX ALIAS PkgConfig::FoX)
endif ()

# Sanitize local variables
set(PKG_MODULE_SPECS)
set(_quiet_arg)
set(_required_arg)
