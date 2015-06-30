# Options
option (enable_tests "Enable unit tests"    off)
option (use_logging  "Enable debug logging" off)

# Set binary output dir
set (CMAKE_RUNTIME_OUTPUT_DIRECTORY ${CMAKE_BINARY_DIR}/bin)

# Compiler flags
set (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11 -g -Wall")

# Subdirs
add_subdirectory (gui)
add_subdirectory (srv)

