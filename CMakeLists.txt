# Boilerplate CMake things
cmake_minimum_required (VERSION 3.2.3)
project (bread)

# Set up version variables user Semantic Versioning
set (bread_version_major 0) # Used for major, api-breaking changes
set (bread_version_minor 1) # Used for minor, backwards-compatible changes
set (bread_version_patch 0) # Used for bug fixes

# Add src directory to be built
add_subdirectory (src)