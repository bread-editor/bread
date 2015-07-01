# Boilerplate CMake things
cmake_minimum_required (VERSION 3.0.0)
project (bread)

# Enable testing by default
option (testing_enabled "enable testing" on)

# Output binaries to bin/
set (CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

# CFlags
set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -g -Wall")

# Find Thread library
find_package (Threads REQUIRED)

# Find LibUV
find_package (PkgConfig)
if (PKG_CONFIG_FOUND)
  pkg_check_modules(PC-LIBUV REQUIRED libuv)
else ()
  message (FATAL_ERROR "Pkg-config not found. Please install it or add it to the PATH")
endif ()

find_path (LIBUV_INCLUDE_DIR uv.h
  HINTS ${PC_LIBUV_INCLUDEDIR} ${PC_LIBUV_INCLUDE_DIRS}
  ${LIMIT_SEARCH})

find_library (LIBUV_LIBRARY NAMES uv HINTS
  ${PC_LIBUV_LIBDIR} ${PC_LIBUV_LIBRARY_DIRS}
  ${LIMIT_SEARCH})

set (LIBUV_LIBRARIES ${LIBUV_LIBRARY} ${PC_LIBUV_LIBRARIES})
set (LIBUV_INCLUDE_DIRS ${LIBUV_INCLUDE_DIR})


# Add src directory to be built
add_subdirectory (src)

# If testing is enabled, add test directory to build
if (testing_enabled)
  message("Testing is enabled. Building tests")
  add_subdirectory (test)
endif()