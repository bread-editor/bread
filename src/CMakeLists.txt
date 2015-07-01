# Set Bread version
set (bread_version_major 0)
set (bread_version_minor 1)
set (bread_version_patch 0)
set (bread_version_name "Bagel")
set (bread_version "Bagel ${bread_version_major}.${bread_version_minor}.${bread_version_patch}")

# Bread executable
file (GLOB srcfiles *.cc)
file (GLOB _srcfiles */*.cc)
add_executable (bread ${srcfiles} ${_srcfiles})

# Add Libs
target_link_libraries (bread ${LIBUV_LIBRARIES})

# Include headers from this folder
include_directories (./)