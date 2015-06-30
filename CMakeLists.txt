# Boilerplate CMake things
cmake_minimum_required (VERSION 3.0.0)
project (bread)

# Enable testing by default
option (testing_enabled "enable testing" on)

# Output binaries to bin/
set (CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

# CFlags
set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -g -Wall")

# Add src directory to be built
add_subdirectory (src)

# If testing is enabled, add test directory to build
if (testing_enabled)
  message("Testing is enabled. Building tests")
  add_subdirectory (test)
endif()