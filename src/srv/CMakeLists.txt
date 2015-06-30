# Set Bread version using Semantic Versioning
set (bread_version_major 0)
set (bread_version_minor 1)
set (bread_version_patch 0)
set (bread_version ${bread_version_major}.${bread_version_minor}.${bread_version_patch})

# Bread executable
file (GLOB srcfiles *.cc)
add_executable (bread ${srcfiles})