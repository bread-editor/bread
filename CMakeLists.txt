# Boilerplate CMake things
cmake_minimum_required (VERSION 3.2.3)
project (bread)

# Set up version variables user Semantic Versioning
set (bread_version_major 0) # Used for major, api-breaking changes
set (bread_version_minor 1) # Used for minor, backwards-compatible changes
set (bread_version_patch 0) # Used for bug fixes

# Enable testing by default
option (testing_enabled "enable testing" on)

# Add src directory to be built
add_subdirectory (src)

# If testing is enabled, add test directory to build
if (testing_enabled)
  message("Testing is enabled. Building tests")
  add_subdirectory (test)
endif()